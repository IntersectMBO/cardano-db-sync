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

module Cardano.Db.Operations.Other.ConsumedTxOut where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Db.Error (LookupFail (..), logAndThrowIO)
import Cardano.Db.Operations.Insert (insertExtraMigration)
import Cardano.Db.Operations.Query (listToMaybe, queryAllExtraMigrations, queryBlockHeight, queryBlockNo, queryMaxRefId)
import Cardano.Db.Operations.QueryHelper (isJust)
import Cardano.Db.Operations.Types (TxOutFields (..), TxOutIdW (..), TxOutTable, TxOutTableType (..), isTxOutVariantAddress)
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Db.Types (ExtraMigration (..), MigrationValues (..), PruneConsumeMigration (..), processMigrationValues)
import Cardano.Prelude (textShow, void)
import Control.Exception (throw)
import Control.Exception.Lifted (handle, throwIO)
import Control.Monad.Extra (unless, when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
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
querySetNullTxOut ::
  MonadIO m =>
  TxOutTableType ->
  Maybe TxId ->
  ReaderT SqlBackend m (Text, Int64)
querySetNullTxOut txOutTableType mMinTxId = do
  case mMinTxId of
    Nothing -> do
      pure ("No tx_out to set to null", 0)
    Just txId -> do
      txOutIds <- getTxOutConsumedAfter txId
      mapM_ setNullTxOutConsumedAfter txOutIds
      let updatedEntriesCount = length txOutIds
      pure ("tx_out.consumed_by_tx_id", fromIntegral updatedEntriesCount)
  where
    -- \| This requires an index at TxOutConsumedByTxId.
    getTxOutConsumedAfter :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOutIdW]
    getTxOutConsumedAfter txId =
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

    -- \| This requires an index at TxOutConsumedByTxId.
    setNullTxOutConsumedAfter :: MonadIO m => TxOutIdW -> ReaderT SqlBackend m ()
    setNullTxOutConsumedAfter txOutId =
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

runExtraMigrations :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> TxOutTableType -> Word64 -> PruneConsumeMigration -> ReaderT SqlBackend m ()
runExtraMigrations trce txOutTableType blockNoDiff pcm = do
  ems <- queryAllExtraMigrations
  isTxOutNull <- queryTxOutIsNull txOutTableType
  let migrationValues = processMigrationValues ems pcm
      isTxOutVariant = isTxOutVariantAddress txOutTableType
      isTxOutAddressSet = isTxOutAddressPreviouslySet migrationValues

  -- can only run "use_address_table" on a non populated database but don't throw if the migration was previously set
  when (isTxOutVariant && not isTxOutNull && not isTxOutAddressSet) $
    throw $
      DBExtraMigration "runExtraMigrations: The use of the config 'tx_out.use_address_table' can only be caried out on a non populated database."
  -- Make sure the config "use_address_table" is there if the migration wasn't previously set in the past
  when (not isTxOutVariant && isTxOutAddressSet) $
    throw $
      DBExtraMigration "runExtraMigrations: The configuration option 'tx_out.use_address_table' was previously set and the database updated. Unfortunately reverting this isn't possible."
  -- Has the user given txout address config && the migration wasn't previously set
  when (isTxOutVariant && not isTxOutAddressSet) $ do
    updateTxOutAndCreateAddress trce
    insertExtraMigration TxOutAddressPreviouslySet
  -- first check if pruneTxOut flag is missing and it has previously been used
  when (isPruneTxOutPreviouslySet migrationValues && not (pcmPruneTxOut pcm)) $
    throw $
      DBExtraMigration
        "If --prune-tx-out flag is enabled and then db-sync is stopped all future executions of db-sync should still have this flag activated. Otherwise, it is considered bad usage and can cause crashes."
  handleMigration migrationValues
  where
    handleMigration :: (MonadBaseControl IO m, MonadIO m) => MigrationValues -> ReaderT SqlBackend m ()
    handleMigration migrationValues@MigrationValues {..} = do
      let PruneConsumeMigration {..} = pruneConsumeMigration
      case (isConsumeTxOutPreviouslySet, pcmConsumedTxOut, pcmPruneTxOut) of
        -- No Migration Needed
        (False, False, False) -> do
          liftIO $ logInfo trce "runExtraMigrations: No extra migration specified"
        -- Already migrated
        (True, True, False) -> do
          liftIO $ logInfo trce "runExtraMigrations: Extra migration consumed_tx_out already executed"
        -- Invalid State
        (True, False, False) -> liftIO $ logAndThrowIO trce "runExtraMigrations: consumed-tx-out or prune-tx-out is not set, but consumed migration is found."
        -- Consume TxOut
        (False, True, False) -> do
          liftIO $ logInfo trce "runExtraMigrations: Running extra migration consumed_tx_out"
          insertExtraMigration ConsumeTxOutPreviouslySet
          migrateTxOut trce txOutTableType $ Just migrationValues
        -- Prune TxOut
        (_, _, True) -> do
          unless isPruneTxOutPreviouslySet $ insertExtraMigration PruneTxOutFlagPreviouslySet
          if isConsumeTxOutPreviouslySet
            then do
              liftIO $ logInfo trce "runExtraMigrations: Running extra migration prune tx_out"
              deleteConsumedTxOut trce txOutTableType blockNoDiff
            else deleteAndUpdateConsumedTxOut trce txOutTableType migrationValues blockNoDiff

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
-- Queries Tests
--------------------------------------------------------------------------------------------------

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

queryTxOutIsNull :: TxOutTableType -> MonadIO m => ReaderT SqlBackend m Bool
queryTxOutIsNull = \case
  TxOutCore -> pure False
  TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Bool
    query = do
      res <- select $ do
        _ <- from $ table @(TxOutTable a)
        limit 1
        pure (val (1 :: Int))
      pure $ null res

--------------------------------------------------------------------------------------------------
-- Updates
--------------------------------------------------------------------------------------------------
updateListTxOutConsumedByTxId :: MonadIO m => [(TxOutIdW, TxId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxId ls = do
  mapM_ (uncurry updateTxOutConsumedByTxId) ls
  where
    updateTxOutConsumedByTxId :: MonadIO m => TxOutIdW -> TxId -> ReaderT SqlBackend m ()
    updateTxOutConsumedByTxId txOutId txId =
      case txOutId of
        CTxOutIdW txOutId' -> update txOutId' [C.TxOutConsumedByTxId =. Just txId]
        VTxOutIdW txOutId' -> update txOutId' [V.TxOutConsumedByTxId =. Just txId]

migrateTxOut ::
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  Trace IO Text ->
  TxOutTableType ->
  Maybe MigrationValues ->
  ReaderT SqlBackend m ()
migrateTxOut trce txOutTableType mMvs = do
  whenJust mMvs $ \mvs -> do
    when (pcmConsumedTxOut (pruneConsumeMigration mvs) && not (isTxOutAddressPreviouslySet mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding consumed-by-id Index"
      void createConsumedIndexTxOut
    when (pcmPruneTxOut (pruneConsumeMigration mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding prune contraint on tx_out table"
      void createPruneConstraintTxOut
  migrateNextPageTxOut (Just trce) txOutTableType 0

migrateNextPageTxOut :: MonadIO m => Maybe (Trace IO Text) -> TxOutTableType -> Word64 -> ReaderT SqlBackend m ()
migrateNextPageTxOut mTrce txOutTableType offst = do
  whenJust mTrce $ \trce ->
    liftIO $ logInfo trce $ "migrateNextPageTxOut: Handling input offset " <> textShow offst
  page <- getInputPage offst pageSize
  updatePageEntries txOutTableType page
  when (fromIntegral (length page) == pageSize) $
    migrateNextPageTxOut mTrce txOutTableType $!
      (offst + pageSize)

--------------------------------------------------------------------------------------------------
-- Delete + Update
--------------------------------------------------------------------------------------------------
deleteAndUpdateConsumedTxOut ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  TxOutTableType ->
  MigrationValues ->
  Word64 ->
  ReaderT SqlBackend m ()
deleteAndUpdateConsumedTxOut trce txOutTableType migrationValues blockNoDiff = do
  maxTxId <- findMaxTxInId blockNoDiff
  case maxTxId of
    Left errMsg -> do
      liftIO $ logInfo trce $ "No tx_out were deleted as no blocks found: " <> errMsg
      liftIO $ logInfo trce "deleteAndUpdateConsumedTxOut: Now Running extra migration prune tx_out"
      migrateTxOut trce txOutTableType $ Just migrationValues
      insertExtraMigration ConsumeTxOutPreviouslySet
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

shouldCreateConsumedTxOut ::
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Bool ->
  ReaderT SqlBackend m ()
shouldCreateConsumedTxOut trce rcc =
  unless rcc $ do
    liftIO $ logInfo trce "Created ConsumedTxOut when handling page entries."
    createConsumedIndexTxOut

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

--------------------------------------------------------------------------------------------------
-- Raw Queries
--------------------------------------------------------------------------------------------------

createConsumedIndexTxOut ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
createConsumedIndexTxOut = do
  handle exceptHandler $ rawExecute createIndex []
  where
    createIndex =
      "CREATE INDEX IF NOT EXISTS idx_tx_out_consumed_by_tx_id ON tx_out (consumed_by_tx_id)"

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DBPruneConsumed $ show e)

createPruneConstraintTxOut ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
createPruneConstraintTxOut = do
  handle exceptHandler $ rawExecute addConstraint []
  where
    addConstraint =
      Text.unlines
        [ "do $$"
        , "begin"
        , "  if not exists ("
        , "    select 1"
        , "    from information_schema.table_constraints"
        , "    where constraint_name = 'ma_tx_out_tx_out_id_fkey'"
        , "      and table_name = 'ma_tx_out'"
        , "  ) then"
        , "    execute 'alter table ma_tx_out add constraint ma_tx_out_tx_out_id_fkey foreign key(tx_out_id) references tx_out(id) on delete cascade on update restrict';"
        , "  end if;"
        , "end $$;"
        ]

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DBPruneConsumed $ show e)

-- Be very mindfull that these queries can fail silently and make tests fail making it hard to know why.
-- To help mitigate this, logs are printed after each query is ran, so one can know where it stopped.
updateTxOutAndCreateAddress ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  Trace IO Text ->
  ReaderT SqlBackend m ()
updateTxOutAndCreateAddress trc = do
  handle exceptHandler $ rawExecute dropViewsQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Dropped views"
  handle exceptHandler $ rawExecute alterTxOutQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Altered tx_out"
  handle exceptHandler $ rawExecute alterCollateralTxOutQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Altered collateral_tx_out"
  handle exceptHandler $ rawExecute createAddressTableQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Created address table"
  handle exceptHandler $ rawExecute createIndexPaymentCredQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Created index payment_cred"
  handle exceptHandler $ rawExecute createIndexRawQuery []
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Created index raw"
  liftIO $ logInfo trc "updateTxOutAndCreateAddress: Completed"
  where
    dropViewsQuery =
      Text.unlines
        [ "DROP VIEW IF EXISTS utxo_byron_view;"
        , "DROP VIEW IF EXISTS utxo_view;"
        ]

    alterTxOutQuery =
      Text.unlines
        [ "ALTER TABLE \"tx_out\""
        , "  ADD COLUMN \"address_id\" INT8 NOT NULL,"
        , "  DROP COLUMN \"address\","
        , "  DROP COLUMN \"address_has_script\","
        , "  DROP COLUMN \"payment_cred\""
        ]

    alterCollateralTxOutQuery =
      Text.unlines
        [ "ALTER TABLE \"collateral_tx_out\""
        , "  ADD COLUMN \"address_id\" INT8 NOT NULL,"
        , "  DROP COLUMN \"address\","
        , "  DROP COLUMN \"address_has_script\","
        , "  DROP COLUMN \"payment_cred\""
        ]

    createAddressTableQuery =
      Text.unlines
        [ "CREATE TABLE \"address\" ("
        , "  \"id\" SERIAL8 PRIMARY KEY UNIQUE,"
        , "  \"address\" VARCHAR NOT NULL,"
        , "  \"raw\" BYTEA NOT NULL,"
        , "  \"has_script\" BOOLEAN NOT NULL,"
        , "  \"payment_cred\" hash28type NULL,"
        , "  \"stake_address_id\" INT8 NULL"
        , ")"
        ]

    createIndexPaymentCredQuery =
      "CREATE INDEX IF NOT EXISTS idx_address_payment_cred ON address(payment_cred);"

    createIndexRawQuery =
      "CREATE INDEX IF NOT EXISTS idx_address_raw ON address USING HASH (raw);"

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DBPruneConsumed $ show e)

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
migrateTxOutDbTool :: (MonadIO m, MonadBaseControl IO m) => TxOutTableType -> ReaderT SqlBackend m ()
migrateTxOutDbTool txOutTableType = do
  _ <- createConsumedIndexTxOut
  migrateNextPageTxOut Nothing txOutTableType 0

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
