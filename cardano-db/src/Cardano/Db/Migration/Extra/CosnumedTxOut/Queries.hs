{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Migration.Extra.CosnumedTxOut.Queries where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Db.Error (LookupFail (..))
import Cardano.Db.Insert (insertMany', insertUnchecked)
import Cardano.Db.Migration.Extra.CosnumedTxOut.Schema
import Cardano.Db.Query (isJust, listToMaybe, queryBlockHeight, queryMaxRefId)
import Cardano.Db.Text
import Control.Exception.Lifted (handle, throwIO)
import Control.Monad.Extra (unless, when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Word (Word64)

-- import Database.Esqueleto.Experimental hiding (update, (<=.), (=.), (==.))
import Database.Esqueleto.Experimental hiding (update, (<=.), (=.), (==.))
import qualified Database.Esqueleto.Experimental as E
import Database.Persist ((<=.), (=.), (==.))
import Database.Persist.Class (update)
import Database.Persist.Sql (deleteWhereCount)
import Database.PostgreSQL.Simple (SqlError)

pageSize :: Word64
pageSize = 100_000

data ConsumedTriplet = ConsumedTriplet
  { ctTxOutTxId :: TxId -- The txId of the txOut
  , ctTxOutIndex :: Word64 -- Tx index of the txOut
  , ctTxInTxId :: TxId -- The txId of the txId
  }

--------------------------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------------------------
queryUpdateListTxOutConsumedByTxId :: MonadIO m => [(TxOutId, TxId)] -> ReaderT SqlBackend m ()
queryUpdateListTxOutConsumedByTxId ls = do
  mapM_ (uncurry updateTxOutConsumedByTxId) ls

queryTxConsumedColumnExists :: MonadIO m => ReaderT SqlBackend m Bool
queryTxConsumedColumnExists = do
  columntExists :: [Text] <-
    fmap unSingle
      <$> rawSql
        ( mconcat
            [ "SELECT column_name FROM information_schema.columns "
            , "WHERE table_name='tx_out' and column_name='consumed_by_tx_id'"
            ]
        )
        []
  pure (not $ null columntExists)

-- | This is a count of the null consumed_by_tx_id
queryTxOutConsumedNullCount :: MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedNullCount = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (isNothing $ txOut ^. TxOutConsumedByTxId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryTxOutConsumedCount :: MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedCount = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (not_ $ isNothing $ txOut ^. TxOutConsumedByTxId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

querySetNullTxOut :: MonadIO m => Trace IO Text -> Maybe TxId -> ReaderT SqlBackend m ()
querySetNullTxOut trce mMinTxId = do
  whenJust mMinTxId $ \txId -> do
    txOutIds <- getTxOutConsumedAfter txId
    mapM_ setNullTxOutConsumedAfter txOutIds
    let updatedEntries = length txOutIds
    liftIO $ logInfo trce $ "Set to null " <> textShow updatedEntries <> " tx_out.consumed_by_tx_id"

createConsumedTxOut ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
createConsumedTxOut = do
  handle exceptHandler $
    rawExecute
      "ALTER TABLE tx_out ADD COLUMN consumed_by_tx_id INT8 NULL"
      []
  handle exceptHandler $
    rawExecute
      "CREATE INDEX IF NOT EXISTS idx_tx_out_consumed_by_tx_id ON tx_out (consumed_by_tx_id)"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE ma_tx_out ADD CONSTRAINT ma_tx_out_tx_out_id_fkey FOREIGN KEY(tx_out_id) REFERENCES tx_out(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      []
  where
    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DBPruneConsumed $ show e)

_validateMigration :: MonadIO m => Trace IO Text -> ReaderT SqlBackend m Bool
_validateMigration trce = do
  _migrated <- queryTxConsumedColumnExists
  --  unless migrated $ runMigration
  txInCount <- countTxIn
  consumedTxOut <- countConsumed
  if txInCount > consumedTxOut
    then do
      liftIO $
        logWarning trce $
          mconcat
            [ "Found incomplete TxOut migration. There are"
            , textShow txInCount
            , " TxIn, but only"
            , textShow consumedTxOut
            , " consumed TxOut"
            ]
      pure False
    else
      if txInCount == consumedTxOut
        then do
          liftIO $ logInfo trce "Found complete TxOut migration"
          pure True
        else do
          liftIO $
            logError trce $
              mconcat
                [ "The impossible happened! There are"
                , textShow txInCount
                , " TxIn, but "
                , textShow consumedTxOut
                , " consumed TxOut"
                ]
          pure False

--------------------------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------------------------
insertTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOutExtra = insertUnchecked "TxOutExtra"

insertManyTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutExtra = insertMany' "TxOut"

--------------------------------------------------------------------------------------------------
-- Updates
--------------------------------------------------------------------------------------------------
updateTxOutConsumedByTxId :: MonadIO m => TxOutId -> TxId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxId txOutId txId =
  update txOutId [TxOutConsumedByTxId =. Just txId]

-- | This requires an index at TxOutConsumedByTxId.
getTxOutConsumedAfter :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOutId]
getTxOutConsumedAfter txId = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (txOut ^. TxOutConsumedByTxId >=. just (val txId))
    pure $ txOut ^. persistIdField
  pure $ unValue <$> res

-- | This requires an index at TxOutConsumedByTxId.
setNullTxOutConsumedAfter :: MonadIO m => TxOutId -> ReaderT SqlBackend m ()
setNullTxOutConsumedAfter txOutId = do
  update txOutId [TxOutConsumedByTxId =. Nothing]

migrateTxOut ::
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  Maybe (Trace IO Text) ->
  ReaderT SqlBackend m ()
migrateTxOut mTrace = do
  _ <- createConsumedTxOut
  migrateNextPage 0
  where
    migrateNextPage :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    migrateNextPage offst = do
      whenJust mTrace $ \trce ->
        liftIO $ logInfo trce $ "Handling input offset " <> textShow offst
      page <- getInputPage offst pageSize
      updatePageEntries page
      when (fromIntegral (length page) == pageSize) $
        migrateNextPage $!
          offst
            + pageSize

--------------------------------------------------------------------------------------------------
-- Delete + Update
--------------------------------------------------------------------------------------------------

deleteAndUpdateConsumedTxOut ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Word64 ->
  ReaderT SqlBackend m ()
deleteAndUpdateConsumedTxOut trce blockNoDiff = do
  maxTxId <- findMaxTxInId blockNoDiff
  case maxTxId of
    Left errMsg -> do
      liftIO $ logInfo trce $ "No tx_out were deleted as no blocks found: " <> errMsg
      liftIO $ logInfo trce "Now Running extra migration prune tx_out"
      migrateTxOut (Just trce)
    Right mTxId -> do
      migrateNextPage mTxId False 0
  where
    migrateNextPage :: TxId -> Bool -> Word64 -> ReaderT SqlBackend m ()
    migrateNextPage maxTxId ranCreateConsumedTxOut offst = do
      pageEntries <- getInputPage offst pageSize
      resPageEntries <- splitAndProcessPageEntries trce ranCreateConsumedTxOut maxTxId pageEntries
      when (fromIntegral (length pageEntries) == pageSize) $
        migrateNextPage maxTxId resPageEntries $!
          offst
            + pageSize

-- Split the page entries by maxTxInId and process
splitAndProcessPageEntries ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Bool ->
  TxId ->
  [ConsumedTriplet] ->
  ReaderT SqlBackend m Bool
splitAndProcessPageEntries trce ranCreateConsumedTxOut maxTxId pageEntries = do
  let entriesSplit = span (\tr -> ctTxInTxId tr <= maxTxId) pageEntries
  case entriesSplit of
    ([], []) -> do
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      pure True
    -- the whole list is less that maxTxInId
    (xs, []) -> do
      deletePageEntries xs
      pure False
    -- the whole list is greater that maxTxInId
    ([], ys) -> do
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      updatePageEntries ys
      pure True
    -- the list has both bellow and above maxTxInId
    (xs, ys) -> do
      deletePageEntries xs
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      updatePageEntries ys
      pure True

-- | Update
updatePageEntries ::
  MonadIO m =>
  [ConsumedTriplet] ->
  ReaderT SqlBackend m ()
updatePageEntries =
  mapM_ updateTxOutConsumedByTxIdUnique

updateTxOutConsumedByTxIdUnique :: MonadIO m => ConsumedTriplet -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxIdUnique ConsumedTriplet {ctTxOutTxId, ctTxOutIndex, ctTxInTxId} =
  updateWhere [TxOutTxId ==. ctTxOutTxId, TxOutIndex ==. ctTxOutIndex] [TxOutConsumedByTxId =. Just ctTxInTxId]

-- | Delete
-- this builds up a single delete query using the pageEntries list
deletePageEntries ::
  MonadIO m =>
  [ConsumedTriplet] ->
  ReaderT SqlBackend m ()
deletePageEntries = mapM_ (\ConsumedTriplet {ctTxOutTxId, ctTxOutIndex} -> deleteTxOutConsumed ctTxOutTxId ctTxOutIndex)

deleteTxOutConsumed :: MonadIO m => TxId -> Word64 -> ReaderT SqlBackend m ()
deleteTxOutConsumed txOutId index =
  deleteWhere [TxOutTxId ==. txOutId, TxOutIndex ==. index]

shouldCreateConsumedTxOut ::
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Bool ->
  ReaderT SqlBackend m ()
shouldCreateConsumedTxOut trce rcc =
  unless rcc $ do
    liftIO $ logInfo trce "Created ConsumedTxOut when handling page entries."
    createConsumedTxOut

--------------------------------------------------------------------------------------------------
-- Delete
--------------------------------------------------------------------------------------------------
deleteConsumedTxOut ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  Word64 ->
  ReaderT SqlBackend m ()
deleteConsumedTxOut trce blockNoDiff = do
  maxTxInId <- findMaxTxInId blockNoDiff
  case maxTxInId of
    Left errMsg -> liftIO $ logInfo trce $ "No tx_out was deleted: " <> errMsg
    Right mxtid -> deleteConsumedBeforeTx trce mxtid

deleteConsumedBeforeTx :: MonadIO m => Trace IO Text -> TxId -> ReaderT SqlBackend m ()
deleteConsumedBeforeTx trce txId = do
  countDeleted <- deleteWhereCount [TxOutConsumedByTxId <=. Just txId]
  liftIO $ logInfo trce $ "Deleted " <> textShow countDeleted <> " tx_out"

--------------------------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------------------------
findMaxTxInId :: forall m. MonadIO m => Word64 -> ReaderT SqlBackend m (Either Text TxId)
findMaxTxInId blockNoDiff = do
  mBlockHeight <- queryBlockHeight
  maybe (pure $ Left "No blocks found") findConsumed mBlockHeight
  where
    findConsumed :: Word64 -> ReaderT SqlBackend m (Either Text TxId)
    findConsumed tipBlockNo = do
      if tipBlockNo <= blockNoDiff
        then pure $ Left $ "Tip blockNo is " <> textShow tipBlockNo
        else do
          mBlockId <- queryBlockNo $ tipBlockNo - blockNoDiff
          maybe
            (pure $ Left $ "BlockNo hole found at " <> textShow (tipBlockNo - blockNoDiff))
            findConsumedBeforeBlock
            mBlockId

    findConsumedBeforeBlock :: BlockId -> ReaderT SqlBackend m (Either Text TxId)
    findConsumedBeforeBlock blockId = do
      mTxId <- queryMaxRefId TxBlockId blockId False
      case mTxId of
        Nothing -> pure $ Left $ "No txs found before " <> textShow blockId
        Just txId -> pure $ Right txId

queryBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockNo blkNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo E.==. just (val blkNo))
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

getInputPage :: MonadIO m => Word64 -> Word64 -> ReaderT SqlBackend m [ConsumedTriplet]
getInputPage offs pgSize = do
  res <- select $ do
    txIn <- from $ table @TxIn
    limit (fromIntegral pgSize)
    offset (fromIntegral offs)
    orderBy [asc (txIn ^. TxInId)]
    pure txIn
  pure $ convert <$> res
  where
    convert txIn =
      ConsumedTriplet
        { ctTxOutTxId = txInTxOutId (entityVal txIn)
        , ctTxOutIndex = txInTxOutIndex (entityVal txIn)
        , ctTxInTxId = txInTxInId (entityVal txIn)
        }

countTxIn :: MonadIO m => ReaderT SqlBackend m Word64
countTxIn = do
  res <- select $ do
    _ <- from $ table @TxIn
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

countConsumed :: MonadIO m => ReaderT SqlBackend m Word64
countConsumed = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (isJust $ txOut ^. TxOutConsumedByTxId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)
