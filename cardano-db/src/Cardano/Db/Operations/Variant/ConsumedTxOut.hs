{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Operations.Variant.ConsumedTxOut where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Db.Error (LookupFail (..), logAndThrowIO)
import Cardano.Db.Operations.Core.Insert (insertExtraMigration)
import Cardano.Db.Operations.Core.Query (listToMaybe, queryAllExtraMigrations, queryBlockHeight, queryBlockNo, queryMaxRefId)
import Cardano.Db.Operations.Core.QueryHelper (isJust)
import Cardano.Db.Operations.Types (TxOutFields (..), TxOutIdW (..), TxOutTable, TxOutTableType (..))
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Db.Types (ExtraMigration (..), PruneConsumeMigration (..), wasPruneTxOutPreviouslySet)
import Cardano.Prelude (textShow)
import Control.Exception (throw)
import Control.Exception.Lifted (handle, throwIO)
import Control.Monad.Extra (unless, when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Word (Word64)
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
queryUpdateListTxOutConsumedByTxId :: MonadIO m => [(TxOutIdW, TxId)] -> ReaderT SqlBackend m ()
queryUpdateListTxOutConsumedByTxId ls = do
  mapM_ (uncurry updateTxOutConsumedByTxId) ls

queryTxConsumedColumnExists :: MonadIO m => ReaderT SqlBackend m Bool
queryTxConsumedColumnExists = do
  columnExists :: [Text] <-
    fmap unSingle
      <$> rawSql
        ( mconcat
            [ "SELECT column_name FROM information_schema.columns "
            , "WHERE table_name='tx_out' and column_name='consumed_by_tx_id'"
            ]
        )
        []
  pure (not $ null columnExists)

-- | This is a count of the null consumed_by_tx_id
queryTxOutConsumedNullCount :: TxOutTableType -> MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedNullCount = \case
  TxOutCore -> query @'TxOutCore
  TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word64
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        where_ (isNothing $ txOut ^. txOutConsumedByTxIdField @a)
        pure countRows
      pure $ maybe 0 unValue (listToMaybe res)

queryTxOutConsumedCount :: TxOutTableType -> MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedCount = \case
  TxOutCore -> query @'TxOutCore
  TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word64
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        where_ (not_ $ isNothing $ txOut ^. txOutConsumedByTxIdField @a)
        pure countRows
      pure $ maybe 0 unValue (listToMaybe res)

querySetNullTxOut :: MonadIO m => Trace IO Text -> TxOutTableType -> Maybe TxId -> ReaderT SqlBackend m ()
querySetNullTxOut trce txOutTableType mMinTxId = do
  whenJust mMinTxId $ \txId -> do
    txOutIds <- getTxOutConsumedAfter txOutTableType txId
    mapM_ (setNullTxOutConsumedAfter txOutTableType) txOutIds
    let updatedEntries = length txOutIds
    liftIO $ logInfo trce $ "Set to null " <> textShow updatedEntries <> " tx_out.consumed_by_tx_id"

-- TODO: cmdv need to fix the raw execute
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

_validateMigration :: MonadIO m => Trace IO Text -> TxOutTableType -> ReaderT SqlBackend m Bool
_validateMigration trce txOutTableType = do
  _migrated <- queryTxConsumedColumnExists
  --  unless migrated $ runMigration
  txInCount <- countTxIn
  consumedTxOut <- countConsumed txOutTableType
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

updateListTxOutConsumedByTxId :: MonadIO m => [(TxOutIdW, TxId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxId ls = do
  queryUpdateListTxOutConsumedByTxId ls

runExtraMigrations :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> TxOutTableType -> Word64 -> PruneConsumeMigration -> ReaderT SqlBackend m ()
runExtraMigrations trce txOutTableType blockNoDiff PruneConsumeMigration {..} = do
  hasConsumedField <- queryTxConsumedColumnExists
  ems <- queryAllExtraMigrations
  let wPruneTxOutPreviouslySet = wasPruneTxOutPreviouslySet ems
  -- first check if pruneTxOut flag is missing and it has previously been used
  case (pcmPruneTxOut, wPruneTxOutPreviouslySet) of
    (False, True) ->
      throw $
        DBExtraMigration
          ( "If --prune-tx-out flag is enabled and then db-sync is stopped all future executions of db-sync "
              <> "should still have this flag activated. Otherwise, it is considered bad usage and can cause crashes."
          )
    _ -> do
      case (hasConsumedField, pcmConsumeOrPruneTxOut, pcmPruneTxOut) of
        (False, False, False) -> do
          liftIO $ logInfo trce "No extra migration specified"
        (True, True, False) -> do
          liftIO $ logInfo trce "Extra migration consumed_tx_out already executed"
        (True, False, False) -> liftIO $ logAndThrowIO trce migratedButNotSet
        (False, True, False) -> do
          liftIO $ logInfo trce "Running extra migration consumed_tx_out"
          migrateTxOut (Just trce) txOutTableType
        (False, _, True) -> do
          shouldInsertToMigrationTable
          deleteAndUpdateConsumedTxOut trce txOutTableType blockNoDiff
        (True, _, True) -> do
          shouldInsertToMigrationTable
          liftIO $ logInfo trce "Running extra migration prune tx_out"
          deleteConsumedTxOut trce txOutTableType blockNoDiff
      where
        migratedButNotSet = "consumed-tx-out or prune-tx-out is not set, but consumed migration is found."
        -- if PruneTxOutFlagPreviouslySet isn't already set then set it.
        shouldInsertToMigrationTable :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
        shouldInsertToMigrationTable = do
          unless wPruneTxOutPreviouslySet $ insertExtraMigration PruneTxOutFlagPreviouslySet

queryWrongConsumedBy :: TxOutTableType -> MonadIO m => ReaderT SqlBackend m Word64
queryWrongConsumedBy = \case
  TxOutCore -> query @'TxOutCore
  TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word64
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        where_ (just (txOut ^. txOutTxIdField @a) E.==. txOut ^. txOutConsumedByTxIdField @a)
        pure countRows
      pure $ maybe 0 unValue (listToMaybe res)

--------------------------------------------------------------------------------------------------
-- Updates
--------------------------------------------------------------------------------------------------
updateTxOutConsumedByTxId :: MonadIO m => TxOutIdW -> TxId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxId txOutId txId =
  case txOutId of
    CTxOutIdW txOutId' -> update txOutId' [C.TxOutConsumedByTxId =. Just txId]
    VTxOutIdW txOutId' -> update txOutId' [V.TxOutConsumedByTxId =. Just txId]

-- | This requires an index at TxOutConsumedByTxId.
getTxOutConsumedAfter :: MonadIO m => TxOutTableType -> TxId -> ReaderT SqlBackend m [TxOutIdW]
getTxOutConsumedAfter txOutTableType txId =
  case txOutTableType of
    TxOutCore -> wrapTxOutIds CTxOutIdW (queryConsumedTxOutIds @'TxOutCore txId)
    TxOutVariantAddress -> wrapTxOutIds VTxOutIdW (queryConsumedTxOutIds @'TxOutVariantAddress txId)
  where
    wrapTxOutIds constructor = fmap (map constructor)

    queryConsumedTxOutIds ::
      forall a m.
      (TxOutFields a, MonadIO m) =>
      TxId ->
      ReaderT SqlBackend m [TxOutIdFor a]
    queryConsumedTxOutIds txId' = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        where_ (txOut ^. txOutConsumedByTxIdField @a >=. just (val txId'))
        pure $ txOut ^. txOutIdField @a
      pure $ map unValue res

-- | This requires an index at TxOutConsumedByTxId.
setNullTxOutConsumedAfter :: MonadIO m => TxOutTableType -> TxOutIdW -> ReaderT SqlBackend m ()
setNullTxOutConsumedAfter txOutTableType txOutId =
  case txOutTableType of
    TxOutCore -> setNull
    TxOutVariantAddress -> setNull
  where
    setNull ::
      (MonadIO m) =>
      ReaderT SqlBackend m ()
    setNull = do
      case txOutId of
        CTxOutIdW txOutId' -> update txOutId' [C.TxOutConsumedByTxId =. Nothing]
        VTxOutIdW txOutId' -> update txOutId' [V.TxOutConsumedByTxId =. Nothing]

migrateTxOut ::
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  Maybe (Trace IO Text) ->
  TxOutTableType ->
  ReaderT SqlBackend m ()
migrateTxOut mTrace txOutTableType = do
  _ <- createConsumedTxOut
  migrateNextPage 0
  where
    migrateNextPage :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    migrateNextPage offst = do
      whenJust mTrace $ \trce ->
        liftIO $ logInfo trce $ "Handling input offset " <> textShow offst
      page <- getInputPage offst pageSize
      updatePageEntries txOutTableType page
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
  TxOutTableType ->
  Word64 ->
  ReaderT SqlBackend m ()
deleteAndUpdateConsumedTxOut trce txOutTableType blockNoDiff = do
  maxTxId <- findMaxTxInId blockNoDiff
  case maxTxId of
    Left errMsg -> do
      liftIO $ logInfo trce $ "No tx_out were deleted as no blocks found: " <> errMsg
      liftIO $ logInfo trce "Now Running extra migration prune tx_out"
      migrateTxOut (Just trce) txOutTableType
    Right mTxId -> do
      migrateNextPage mTxId False 0
  where
    migrateNextPage :: TxId -> Bool -> Word64 -> ReaderT SqlBackend m ()
    migrateNextPage maxTxId ranCreateConsumedTxOut offst = do
      pageEntries <- getInputPage offst pageSize
      resPageEntries <- splitAndProcessPageEntries trce txOutTableType ranCreateConsumedTxOut maxTxId pageEntries
      when (fromIntegral (length pageEntries) == pageSize) $
        migrateNextPage maxTxId resPageEntries $!
          offst
            + pageSize

-- Split the page entries by maxTxInId and process
splitAndProcessPageEntries ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  TxOutTableType ->
  Bool ->
  TxId ->
  [ConsumedTriplet] ->
  ReaderT SqlBackend m Bool
splitAndProcessPageEntries trce txOutTableType ranCreateConsumedTxOut maxTxId pageEntries = do
  let entriesSplit = span (\tr -> ctTxInTxId tr <= maxTxId) pageEntries
  case entriesSplit of
    ([], []) -> do
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      pure True
    -- the whole list is less that maxTxInId
    (xs, []) -> do
      deletePageEntries txOutTableType xs
      pure False
    -- the whole list is greater that maxTxInId
    ([], ys) -> do
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      updatePageEntries txOutTableType ys
      pure True
    -- the list has both bellow and above maxTxInId
    (xs, ys) -> do
      deletePageEntries txOutTableType xs
      shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
      updatePageEntries txOutTableType ys
      pure True

-- | Update
updatePageEntries ::
  MonadIO m =>
  TxOutTableType ->
  [ConsumedTriplet] ->
  ReaderT SqlBackend m ()
updatePageEntries txOutTableType = mapM_ (updateTxOutConsumedByTxIdUnique txOutTableType)

updateTxOutConsumedByTxIdUnique :: MonadIO m => TxOutTableType -> ConsumedTriplet -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxIdUnique txOutTableType ConsumedTriplet {ctTxOutTxId, ctTxOutIndex, ctTxInTxId} =
  case txOutTableType of
    TxOutCore -> updateWhere [C.TxOutTxId ==. ctTxOutTxId, C.TxOutIndex ==. ctTxOutIndex] [C.TxOutConsumedByTxId =. Just ctTxInTxId]
    TxOutVariantAddress -> updateWhere [V.TxOutTxId ==. ctTxOutTxId, V.TxOutIndex ==. ctTxOutIndex] [V.TxOutConsumedByTxId =. Just ctTxInTxId]

-- | Delete
-- this builds up a single delete query using the pageEntries list
deletePageEntries ::
  MonadIO m =>
  TxOutTableType ->
  [ConsumedTriplet] ->
  ReaderT SqlBackend m ()
deletePageEntries txOutTableType = mapM_ (\ConsumedTriplet {ctTxOutTxId, ctTxOutIndex} -> deleteTxOutConsumed txOutTableType ctTxOutTxId ctTxOutIndex)

deleteTxOutConsumed :: MonadIO m => TxOutTableType -> TxId -> Word64 -> ReaderT SqlBackend m ()
deleteTxOutConsumed txOutTableType txOutId index = case txOutTableType of
  TxOutCore -> deleteWhere [C.TxOutTxId ==. txOutId, C.TxOutIndex ==. index]
  TxOutVariantAddress -> deleteWhere [V.TxOutTxId ==. txOutId, V.TxOutIndex ==. index]

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
  TxOutTableType ->
  Word64 ->
  ReaderT SqlBackend m ()
deleteConsumedTxOut trce txOutTableType blockNoDiff = do
  maxTxInId <- findMaxTxInId blockNoDiff
  case maxTxInId of
    Left errMsg -> liftIO $ logInfo trce $ "No tx_out was deleted: " <> errMsg
    Right mxtid -> deleteConsumedBeforeTx trce txOutTableType mxtid

deleteConsumedBeforeTx :: MonadIO m => Trace IO Text -> TxOutTableType -> TxId -> ReaderT SqlBackend m ()
deleteConsumedBeforeTx trce txOutTableType txId = do
  countDeleted <- case txOutTableType of
    TxOutCore -> deleteWhereCount [C.TxOutConsumedByTxId <=. Just txId]
    TxOutVariantAddress -> deleteWhereCount [V.TxOutConsumedByTxId <=. Just txId]
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

countConsumed ::
  MonadIO m =>
  TxOutTableType ->
  ReaderT SqlBackend m Word64
countConsumed = \case
  TxOutCore -> query @'TxOutCore
  TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word64
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        where_ (isJust $ txOut ^. txOutConsumedByTxIdField @a)
        pure countRows
      pure $ maybe 0 unValue (listToMaybe res)
