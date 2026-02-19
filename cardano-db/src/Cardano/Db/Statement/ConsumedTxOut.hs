{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.ConsumedTxOut where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Prelude (ByteString, Int64, textShow)
import Contravariant.Extras (contrazip2, contrazip3)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Word (Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbLookupError (..), logAndThrowIO, mkDbCallStack)
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants (TxOutIdW (..), TxOutVariantType (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Base (insertExtraMigration, queryAllExtraMigrations)
import Cardano.Db.Statement.Function.Core (bulkEncoder, runSession)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (DbM, ExtraMigration (..), MigrationValues (..), PruneConsumeMigration (..), processMigrationValues)

data ConsumedTriplet = ConsumedTriplet
  { ctTxOutTxId :: !Id.TxId -- The txId of the txOut
  , ctTxOutIndex :: !Word64 -- Tx index of the txOut
  , ctTxInTxId :: !Id.TxId -- The txId of the txId
  }

--------------------------------------------------------------------------------

-- | Run extra migrations for the database
runConsumedTxOutMigrations ::
  -- | Tracer for logging
  Trace IO Text.Text ->
  -- | Bulk size
  Int ->
  -- | TxOut table type being used
  TxOutVariantType ->
  -- | Block number difference
  Word64 ->
  -- | Prune/consume migration config
  PruneConsumeMigration ->
  DbM ()
runConsumedTxOutMigrations trce bulkSize txOutVariantType blockNoDiff pcm = do
  ems <- queryAllExtraMigrations
  isTxOutNull <- queryTxOutIsNull txOutVariantType
  let migrationValues = processMigrationValues ems pcm
      isTxOutVariant = txOutVariantType == TxOutVariantAddress
      isTxOutAddressSet = isTxOutAddressPreviouslySet migrationValues

  -- Can only run "use_address_table" on a non populated database but don't throw if the migration was previously set
  when (isTxOutVariant && not isTxOutNull && not isTxOutAddressSet) $ do
    let msg = "The use of the config 'tx_out.use_address_table' can only be carried out on a non populated database."
    liftIO $ throwIO $ DbLookupError mkDbCallStack msg

  -- Make sure the config "use_address_table" is there if the migration wasn't previously set in the past
  when (not isTxOutVariant && isTxOutAddressSet) $ do
    let msg = "The configuration option 'tx_out.use_address_table' was previously set and the database updated. Unfortunately reverting this isn't possible."
    liftIO $ throwIO $ DbLookupError mkDbCallStack msg

  -- Has the user given txout address config && the migration wasn't previously set
  when (isTxOutVariant && not isTxOutAddressSet) $ do
    updateTxOutAndCreateAddress trce
    insertExtraMigration TxOutAddressPreviouslySet

  -- First check if pruneTxOut flag is missing and it has previously been used
  when (isPruneTxOutPreviouslySet migrationValues && not (pcmPruneTxOut pcm)) $ do
    let msg = "If --prune-tx-out flag is enabled and then db-sync is stopped all future executions of db-sync should still have this flag activated. Otherwise, it is considered bad usage and can cause crashes."
    liftIO $ throwIO $ DbLookupError mkDbCallStack msg

  handleMigration migrationValues
  where
    msgName = "runConsumedTxOutMigrations: "
    handleMigration :: MigrationValues -> DbM ()
    handleMigration migrationValues@MigrationValues {..} = do
      let PruneConsumeMigration {..} = pruneConsumeMigration

      case (isConsumeTxOutPreviouslySet, pcmConsumedTxOut, pcmPruneTxOut) of
        -- No Migration Needed
        (False, False, False) -> do
          liftIO $ logInfo trce $ msgName <> "No extra migration specified"

        -- Already migrated
        (True, True, False) -> do
          liftIO $ logInfo trce $ msgName <> "Extra migration consumed_tx_out already executed"

        -- Invalid State
        (True, False, False) ->
          liftIO $ logAndThrowIO trce $ msgName <> "consumed-tx-out or prune-tx-out is not set, but consumed migration is found."
        -- Consume TxOut
        (False, True, False) -> do
          liftIO $ logInfo trce $ msgName <> "Running extra migration consumed_tx_out"
          insertExtraMigration ConsumeTxOutPreviouslySet
          migrateTxOut bulkSize trce txOutVariantType $ Just migrationValues

        -- Prune TxOut
        (_, _, True) -> do
          unless isPruneTxOutPreviouslySet $
            insertExtraMigration PruneTxOutFlagPreviouslySet
          if isConsumeTxOutPreviouslySet
            then do
              liftIO $ logInfo trce $ msgName <> "Running extra migration prune tx_out"
              deleteConsumedTxOut trce txOutVariantType blockNoDiff
            else deleteAndUpdateConsumedTxOut bulkSize trce txOutVariantType migrationValues blockNoDiff

--------------------------------------------------------------------------------

-- | Statement to check if tx_out is null for specified table type
queryTxOutIsNullStmt :: Text.Text -> HsqlStmt.Statement () Bool
queryTxOutIsNullStmt tName =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT NOT EXISTS (SELECT 1 FROM "
          , tName
          , " LIMIT 1)"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.bool)

-- | Check if the tx_out table is empty (null)
queryTxOutIsNull :: TxOutVariantType -> DbM Bool
queryTxOutIsNull = \case
  TxOutVariantCore -> queryTxOutIsNullImpl @SVC.TxOutCore
  TxOutVariantAddress -> queryTxOutIsNullImpl @SVA.TxOutAddress

-- | Implementation of queryTxOutIsNull using DbInfo
queryTxOutIsNullImpl :: forall a. DbInfo a => DbM Bool
queryTxOutIsNullImpl = do
  let tName = tableName (Proxy @a)
      stmt = queryTxOutIsNullStmt tName
  runSession mkDbCallStack $
    HsqlSes.statement () stmt

--------------------------------------------------------------------------------

-- | Update tx_out tables and create address table
updateTxOutAndCreateAddress ::
  Trace IO Text.Text ->
  DbM ()
updateTxOutAndCreateAddress trce = do
  runStep "Dropped views" dropViewsQuery
  runStep "Altered tx_out" alterTxOutQuery
  runStep "Altered collateral_tx_out" alterCollateralTxOutQuery
  runStep "Created address table" createAddressTableQuery
  runStep "Created index payment_cred" createIndexPaymentCredQuery
  runStep "Created index raw" createIndexRawQuery
  liftIO $ logInfo trce "updateTxOutAndCreateAddress: Completed"
  where
    -- Helper to run a step with proper logging and error handling
    runStep :: Text.Text -> Text.Text -> DbM ()
    runStep stepDesc sql = do
      let sqlBS = TextEnc.encodeUtf8 sql
      runSession mkDbCallStack $ HsqlSes.sql sqlBS
      liftIO $ logInfo trce $ "updateTxOutAndCreateAddress: " <> stepDesc

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

--------------------------------------------------------------------------------

-- | Migrate tx_out data
migrateTxOut ::
  -- | Bulk size
  Int ->
  Trace IO Text.Text ->
  TxOutVariantType ->
  Maybe MigrationValues ->
  DbM ()
migrateTxOut bulkSize trce txOutVariantType mMvs = do
  whenJust mMvs $ \mvs -> do
    when (pcmConsumedTxOut (pruneConsumeMigration mvs) && not (isTxOutAddressPreviouslySet mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding consumed-by-id Index"
      createConsumedIndexTxOut
    when (pcmPruneTxOut (pruneConsumeMigration mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding prune contraint on tx_out table"
      createPruneConstraintTxOut
  migrateNextPageTxOut bulkSize (Just trce) txOutVariantType 0

-- | Process the tx_out table in pages for migration
migrateNextPageTxOut ::
  -- | Bulk size
  Int ->
  Maybe (Trace IO Text.Text) ->
  TxOutVariantType ->
  Word64 ->
  DbM ()
migrateNextPageTxOut bulkSize mTrce txOutVariantType offst = do
  whenJust mTrce $ \trce ->
    liftIO $ logInfo trce $ "migrateNextPageTxOut: Handling input offset " <> textShow offst
  page <- getInputPage bulkSize offst
  updatePageEntries txOutVariantType page
  when (length page == bulkSize) $
    migrateNextPageTxOut bulkSize mTrce txOutVariantType $!
      (offst + fromIntegral bulkSize)

--------------------------------------------------------------------------------

-- | Statement to update tx_out consumed_by_tx_id field
updateTxOutConsumedStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement ConsumedTriplet ()
updateTxOutConsumedStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE "
          , table
          , " SET consumed_by_tx_id = $3"
          , " WHERE tx_id = $1"
          , " AND index = $2"
          ]

    -- Encoder using ConsumedTriplet
    txIdEncoder = HsqlE.param $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8
    word64Encoder = HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8

    encoder =
      contramap ctTxOutTxId txIdEncoder
        <> contramap ctTxOutIndex word64Encoder
        <> contramap ctTxInTxId txIdEncoder

-- | Update a tx_out record to set consumed_by_tx_id based on transaction info
updateTxOutConsumedByTxIdUnique ::
  TxOutVariantType ->
  ConsumedTriplet ->
  DbM ()
updateTxOutConsumedByTxIdUnique txOutVariantType triplet = do
  case txOutVariantType of
    TxOutVariantCore ->
      runSession mkDbCallStack $
        HsqlSes.statement triplet (updateTxOutConsumedStmt @SVC.TxOutCore)
    TxOutVariantAddress ->
      runSession mkDbCallStack $
        HsqlSes.statement triplet (updateTxOutConsumedStmt @SVA.TxOutAddress)

-- | Update page entries from a list of ConsumedTriplet
updatePageEntries ::
  TxOutVariantType ->
  [ConsumedTriplet] ->
  DbM ()
updatePageEntries txOutVariantType triplets = do
  mapM_ (updateTxOutConsumedByTxIdUnique txOutVariantType) triplets

--------------------------------------------------------------------------------

-- | Statement for creating the consumed_by_tx_id index
createConsumedIndexTxOutStmt :: HsqlStmt.Statement () ()
createConsumedIndexTxOutStmt =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8
        "CREATE INDEX IF NOT EXISTS idx_tx_out_consumed_by_tx_id ON tx_out (consumed_by_tx_id)"

-- | Create index on consumed_by_tx_id in tx_out table
createConsumedIndexTxOut :: DbM ()
createConsumedIndexTxOut = runSession mkDbCallStack $ HsqlSes.statement () createConsumedIndexTxOutStmt

--------------------------------------------------------------------------------

-- | Statement for creating the pruning constraint
createPruneConstraintTxOutStmt :: HsqlStmt.Statement () ()
createPruneConstraintTxOutStmt =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8 $
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

-- | Create constraint for pruning tx_out
createPruneConstraintTxOut :: DbM ()
createPruneConstraintTxOut = runSession mkDbCallStack $ HsqlSes.statement () createPruneConstraintTxOutStmt

--------------------------------------------------------------------------------

-- | Statement to get a page of inputs from tx_in table
getInputPageStmt :: Int -> HsqlStmt.Statement Word64 [ConsumedTriplet]
getInputPageStmt bulkSize =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out_id, tx_out_index, tx_in_id"
          , " FROM tx_in"
          , " ORDER BY id"
          , " LIMIT "
          , Text.pack (show bulkSize)
          , " OFFSET $1"
          ]

    encoder = HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8

    decoder = HsqlD.rowList $ do
      txOutId <- Id.idDecoder Id.TxId
      txOutIndex <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      txInId <- Id.idDecoder Id.TxId
      pure $
        ConsumedTriplet
          { ctTxOutTxId = txOutId
          , ctTxOutIndex = txOutIndex
          , ctTxInTxId = txInId
          }

-- | Get a page of consumed TX inputs
getInputPage ::
  -- | Bulk size
  Int ->
  -- | Offset
  Word64 ->
  DbM [ConsumedTriplet]
getInputPage bulkSize offset =
  runSession mkDbCallStack $ HsqlSes.statement offset (getInputPageStmt bulkSize)

--------------------------------------------------------------------------------

-- Statement function for finding max TxInId by block difference
findMaxTxInIdStmt :: HsqlStmt.Statement Word64 (Either Text.Text Id.TxId)
findMaxTxInIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH target_block_no AS ("
          , "  SELECT MAX(block_no) - $1 AS target_block_no FROM block"
          , ")"
          , "SELECT MAX(tx.id) AS max_tx_id "
          , "FROM tx "
          , "INNER JOIN block ON tx.block_id = block.id "
          , "WHERE block.block_no <= (SELECT target_block_no FROM target_block_no)"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

    decoder = HsqlD.singleRow $ do
      mTxId <- Id.maybeIdDecoder Id.TxId
      let result = case mTxId of
            Nothing -> Left "No transactions found before the specified block"
            Just txId -> Right txId
      pure result

findMaxTxInId :: Word64 -> DbM (Either Text.Text Id.TxId)
findMaxTxInId blockNoDiff =
  runSession mkDbCallStack $ HsqlSes.statement blockNoDiff findMaxTxInIdStmt

--------------------------------------------------------------------------------

-- Delete consumed tx outputs before a specified tx
deleteConsumedBeforeTxStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement (Maybe Id.TxId) Int64
deleteConsumedBeforeTxStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH deleted AS ("
          , "  DELETE FROM " <> tableN
          , "  WHERE consumed_by_tx_id <= $1"
          , "  RETURNING 1"
          , ") SELECT COUNT(*) FROM deleted"
          ]

    encoder = HsqlE.param $ HsqlE.nullable $ Id.getTxId >$< HsqlE.int8
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

-- Function to run delete operation
deleteConsumedBeforeTx ::
  Trace IO Text.Text ->
  TxOutVariantType ->
  Id.TxId ->
  DbM ()
deleteConsumedBeforeTx trce txOutVariantType txId =
  runSession mkDbCallStack $ do
    countDeleted <- case txOutVariantType of
      TxOutVariantCore ->
        HsqlSes.statement (Just txId) (deleteConsumedBeforeTxStmt @SVC.TxOutCore)
      TxOutVariantAddress ->
        HsqlSes.statement (Just txId) (deleteConsumedBeforeTxStmt @SVA.TxOutAddress)
    liftIO $ logInfo trce $ "deleteConsumedBeforeTx: Deleted " <> textShow countDeleted <> " tx_out"

-- Delete consumed tx outputs
deleteConsumedTxOut ::
  Trace IO Text.Text ->
  TxOutVariantType ->
  Word64 ->
  DbM ()
deleteConsumedTxOut trce txOutVariantType blockNoDiff = do
  maxTxIdResult <- findMaxTxInId blockNoDiff
  case maxTxIdResult of
    Left errMsg -> liftIO $ logInfo trce $ "deleteConsumedTxOut: No tx_out was deleted: " <> errMsg
    Right txId -> deleteConsumedBeforeTx trce txOutVariantType txId

--------------------------------------------------------------------------------

-- Statement for deleting TxOut entries
deletePageEntriesStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement [ConsumedTriplet] ()
deletePageEntriesStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH entries AS ("
          , "  SELECT unnest($1::bigint[]) as tx_out_tx_id,"
          , "         unnest($2::int[]) as tx_out_index"
          , ")"
          , "DELETE FROM " <> tableN
          , "WHERE (tx_id, index) IN (SELECT tx_out_tx_id, tx_out_index FROM entries)"
          ]

    encoder = contramap extract encodePartialBulk

    extract :: [ConsumedTriplet] -> ([Id.TxId], [Word64])
    extract xs =
      ( map ctTxOutTxId xs
      , map ctTxOutIndex xs
      )

    encodePartialBulk :: HsqlE.Params ([Id.TxId], [Word64])
    encodePartialBulk =
      contrazip2
        (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)
        (bulkEncoder $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int4)

-- Function to delete page entries
deletePageEntries ::
  TxOutVariantType ->
  [ConsumedTriplet] ->
  DbM ()
deletePageEntries txOutVariantType entries = do
  unless (null entries) $
    runSession mkDbCallStack $ do
      case txOutVariantType of
        TxOutVariantCore ->
          HsqlSes.statement entries (deletePageEntriesStmt @SVC.TxOutCore)
        TxOutVariantAddress ->
          HsqlSes.statement entries (deletePageEntriesStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- | Data for bulk consumption using tx hash
data BulkConsumedByHash = BulkConsumedByHash
  { bchTxHash :: !ByteString
  , bchOutputIndex :: !Word64
  , bchConsumingTxId :: !Id.TxId
  }

updateConsumedByTxHashChunked ::
  TxOutVariantType ->
  [[BulkConsumedByHash]] ->
  DbM ()
updateConsumedByTxHashChunked txOutVariantType consumedData = do
  unless (null consumedData) $ do
    case txOutVariantType of
      TxOutVariantCore -> do
        !_result <-
          runSession mkDbCallStack $
            traverse (\chunk -> HsqlSes.statement chunk (updateConsumedByTxHashBulkStmt @SVC.TxOutCore)) consumedData
        pure ()
      TxOutVariantAddress -> do
        !_result <-
          runSession mkDbCallStack $
            traverse (\chunk -> HsqlSes.statement chunk (updateConsumedByTxHashBulkStmt @SVA.TxOutAddress)) consumedData
        pure ()

updateConsumedByTxHashBulkStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement [BulkConsumedByHash] ()
updateConsumedByTxHashBulkStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH consumption_data AS ("
          , "  SELECT unnest($1::bytea[]) as tx_hash,"
          , "         unnest($2::bigint[]) as output_index,"
          , "         unnest($3::bigint[]) as consuming_tx_id"
          , ")"
          , "UPDATE " <> tableN
          , "SET consumed_by_tx_id = consumption_data.consuming_tx_id"
          , "FROM consumption_data"
          , "INNER JOIN tx ON tx.hash = consumption_data.tx_hash"
          , "WHERE " <> tableN <> ".tx_id = tx.id"
          , "  AND " <> tableN <> ".index = consumption_data.output_index"
          ]
    encoder = contramap extractBulkData bulkConsumedByHashEncoder

extractBulkData :: [BulkConsumedByHash] -> ([ByteString], [Word64], [Id.TxId])
extractBulkData xs =
  ( map bchTxHash xs
  , map bchOutputIndex xs
  , map bchConsumingTxId xs
  )

bulkConsumedByHashEncoder :: HsqlE.Params ([ByteString], [Word64], [Id.TxId])
bulkConsumedByHashEncoder =
  contrazip3
    (bulkEncoder $ HsqlE.nonNullable HsqlE.bytea)
    (bulkEncoder $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)

--------------------------------------------------------------------------------

-- Helper function for creating consumed index if needed
shouldCreateConsumedTxOut ::
  Trace IO Text.Text ->
  Bool ->
  DbM ()
shouldCreateConsumedTxOut trce rcc = do
  unless rcc $ do
    liftIO $ logInfo trce "Created ConsumedTxOut when handling page entries."
    createConsumedIndexTxOut

--------------------------------------------------------------------------------

-- Split and process page entries
splitAndProcessPageEntries ::
  Trace IO Text.Text ->
  TxOutVariantType ->
  Bool ->
  Id.TxId ->
  [ConsumedTriplet] ->
  DbM Bool
splitAndProcessPageEntries trce txOutVariantType ranCreateConsumedTxOut maxTxId pageEntries =
  do
    let entriesSplit = span (\tr -> ctTxInTxId tr <= maxTxId) pageEntries
    case entriesSplit of
      ([], []) -> do
        shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
        pure True
      -- the whole list is less than maxTxInId
      (xs, []) -> do
        deletePageEntries txOutVariantType xs
        pure False
      -- the whole list is greater than maxTxInId
      ([], ys) -> do
        shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
        updatePageEntries txOutVariantType ys
        pure True
      -- the list has both below and above maxTxInId
      (xs, ys) -> do
        deletePageEntries txOutVariantType xs
        shouldCreateConsumedTxOut trce ranCreateConsumedTxOut
        updatePageEntries txOutVariantType ys
        pure True

--------------------------------------------------------------------------------

-- Main function for delete and update
deleteAndUpdateConsumedTxOut ::
  -- | Bulk size
  Int ->
  Trace IO Text.Text ->
  TxOutVariantType ->
  MigrationValues ->
  Word64 ->
  DbM ()
deleteAndUpdateConsumedTxOut bulkSize trce txOutVariantType migrationValues blockNoDiff = do
  maxTxIdResult <- findMaxTxInId blockNoDiff
  case maxTxIdResult of
    Left errMsg -> do
      liftIO $ logInfo trce $ "No tx_out were deleted as no blocks found: " <> errMsg
      liftIO $ logInfo trce "deleteAndUpdateConsumedTxOut: Now Running extra migration prune tx_out"
      migrateTxOut bulkSize trce txOutVariantType $ Just migrationValues
      insertExtraMigration ConsumeTxOutPreviouslySet
    Right maxTxId -> do
      migrateNextPage maxTxId False 0
  where
    migrateNextPage :: Id.TxId -> Bool -> Word64 -> DbM ()
    migrateNextPage maxTxId ranCreateConsumedTxOut offst = do
      pageEntries <- getInputPage bulkSize offst
      resPageEntries <- splitAndProcessPageEntries trce txOutVariantType ranCreateConsumedTxOut maxTxId pageEntries
      when (length pageEntries == bulkSize) $
        migrateNextPage maxTxId resPageEntries $!
          offst + fromIntegral bulkSize

--------------------------------------------------------------------------------

migrateTxOutDbTool :: Int -> TxOutVariantType -> DbM ()
migrateTxOutDbTool bulkSize txOutVariantType = do
  createConsumedIndexTxOut
  migrateNextPageTxOut bulkSize Nothing txOutVariantType 0

--------------------------------------------------------------------------------

-- | Update a list of TxOut consumed by TxId mappings using bulked statements
updateListTxOutConsumedByTxIdChunked :: [[(TxOutIdW, Id.TxId)]] -> DbM ()
updateListTxOutConsumedByTxIdChunked chunks = do
  unless (null chunks) $ do
    !_results <-
      runSession mkDbCallStack $
        traverse executeUpdate chunks
    pure ()
  where
    executeUpdate :: [(TxOutIdW, Id.TxId)] -> HsqlSes.Session ()
    executeUpdate chunk =
      case chunk of
        [] -> pure () -- Empty chunk, do nothing
        ((VCTxOutIdW _, _) : _) ->
          -- All are Core type, extract Core IDs
          let coreChunk = [(coreId, txId) | (VCTxOutIdW coreId, txId) <- chunk]
              (coreIds, txIds) = unzip coreChunk
           in HsqlSes.statement (coreIds, txIds) updateBulkConsumedByTxIdCore
        ((VATxOutIdW _, _) : _) ->
          -- All are Address type, extract Address IDs
          let addressChunk = [(addrId, txId) | (VATxOutIdW addrId, txId) <- chunk]
              (addrIds, txIds) = unzip addressChunk
           in HsqlSes.statement (addrIds, txIds) updateBulkConsumedByTxIdAddress

updateBulkConsumedByTxId ::
  forall a b.
  DbInfo a =>
  Proxy a ->
  HsqlE.Params b ->
  HsqlStmt.Statement b ()
updateBulkConsumedByTxId proxy encoder =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH update_data AS ("
          , "  SELECT unnest($1::bigint[]) as row_id,"
          , "         unnest($2::bigint[]) as consumed_by_tx_id"
          , ")"
          , "UPDATE " <> tableName proxy
          , " SET consumed_by_tx_id = update_data.consumed_by_tx_id"
          , " FROM update_data"
          , " WHERE " <> tableName proxy <> ".id = update_data.row_id"
          ]

updateBulkConsumedByTxIdCore :: HsqlStmt.Statement ([Id.TxOutCoreId], [Id.TxId]) ()
updateBulkConsumedByTxIdCore = updateBulkConsumedByTxId (Proxy @SVC.TxOutCore) encoderCore
  where
    encoderCore :: HsqlE.Params ([Id.TxOutCoreId], [Id.TxId])
    encoderCore =
      contrazip2
        (bulkEncoder $ HsqlE.nonNullable $ Id.getTxOutCoreId >$< HsqlE.int8)
        (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)

updateBulkConsumedByTxIdAddress :: HsqlStmt.Statement ([Id.TxOutAddressId], [Id.TxId]) ()
updateBulkConsumedByTxIdAddress = updateBulkConsumedByTxId (Proxy @SVA.TxOutAddress) encoderAddress
  where
    encoderAddress :: HsqlE.Params ([Id.TxOutAddressId], [Id.TxId])
    encoderAddress =
      contrazip2
        (bulkEncoder $ HsqlE.nonNullable $ Id.getTxOutAddressId >$< HsqlE.int8)
        (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)

--------------------------------------------------------------------------------

-- | Count of TxOuts with null consumed_by_tx_id
queryTxOutConsumedNullCountStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement () Word64
queryTxOutConsumedNullCountStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> tableN
          , " WHERE consumed_by_tx_id IS NULL"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

-- | Query for count of TxOuts with null consumed_by_tx_id
queryTxOutConsumedNullCount :: TxOutVariantType -> DbM Word64
queryTxOutConsumedNullCount = \case
  TxOutVariantCore ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryTxOutConsumedNullCountStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryTxOutConsumedNullCountStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- | Count of TxOuts with non-null consumed_by_tx_id
queryTxOutConsumedCountStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement () Word64
queryTxOutConsumedCountStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> tableN
          , " WHERE consumed_by_tx_id IS NOT NULL"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryTxOutConsumedCount :: TxOutVariantType -> DbM Word64
queryTxOutConsumedCount = \case
  TxOutVariantCore ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryTxOutConsumedCountStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryTxOutConsumedCountStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- | Statement for querying TxOuts where consumed_by_tx_id equals tx_id
queryWrongConsumedByStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement () Word64
queryWrongConsumedByStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> tableN
          , " WHERE tx_id = consumed_by_tx_id"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

-- | Query for count of TxOuts with consumed_by_tx_id equal to tx_id (which is wrong)
queryWrongConsumedBy :: TxOutVariantType -> DbM Word64
queryWrongConsumedBy = \case
  TxOutVariantCore ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryWrongConsumedByStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runSession mkDbCallStack $
      HsqlSes.statement () (queryWrongConsumedByStmt @SVA.TxOutAddress)
