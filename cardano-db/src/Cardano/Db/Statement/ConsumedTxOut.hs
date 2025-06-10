{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.ConsumedTxOut where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Prelude (Int64, textShow)
import Contravariant.Extras (contrazip2, contrazip3)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Word (Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..), logAndThrowIO)
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants (TxOutIdW (..), TxOutVariantType (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Base (insertExtraMigration, queryAllExtraMigrations)
import Cardano.Db.Statement.Function.Core (bulkEncoder, mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (DbAction, ExtraMigration (..), MigrationValues (..), PruneConsumeMigration (..), processMigrationValues)

data ConsumedTriplet = ConsumedTriplet
  { ctTxOutTxId :: !Id.TxId -- The txId of the txOut
  , ctTxOutIndex :: !Word64 -- Tx index of the txOut
  , ctTxInTxId :: !Id.TxId -- The txId of the txId
  }

consumedTripletDecoder :: HsqlD.Row ConsumedTriplet
consumedTripletDecoder =
  ConsumedTriplet
    <$> Id.idDecoder Id.TxId -- ctTxOutTxId
    <*> HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8) -- ctTxOutIndex
    <*> Id.idDecoder Id.TxId -- ctTxInTxId

consumedTripletEncoder :: HsqlE.Params ConsumedTriplet
consumedTripletEncoder =
  mconcat
    [ ctTxOutTxId >$< Id.idEncoder Id.getTxId
    , ctTxOutIndex >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    , ctTxInTxId >$< Id.idEncoder Id.getTxId
    ]

encodeConsumedTripletBulk :: HsqlE.Params ([Id.TxId], [Word64], [Id.TxId])
encodeConsumedTripletBulk =
  contrazip3
    (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)
    (bulkEncoder $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    (bulkEncoder $ HsqlE.nonNullable $ Id.getTxId >$< HsqlE.int8)

--------------------------------------------------------------------------------

pageSize :: Word64
pageSize = 100_000

--------------------------------------------------------------------------------

-- | Run extra migrations for the database
runConsumedTxOutMigrations ::
  MonadIO m =>
  -- | Tracer for logging
  Trace IO Text.Text ->
  -- | TxOut table type being used
  TxOutVariantType ->
  -- | Block number difference
  Word64 ->
  -- | Prune/consume migration config
  PruneConsumeMigration ->
  DbAction m ()
runConsumedTxOutMigrations trce txOutVariantType blockNoDiff pcm = do
  ems <- queryAllExtraMigrations
  isTxOutNull <- queryTxOutIsNull txOutVariantType
  let migrationValues = processMigrationValues ems pcm
      isTxOutVariant = txOutVariantType == TxOutVariantAddress
      isTxOutAddressSet = isTxOutAddressPreviouslySet migrationValues

  -- Can only run "use_address_table" on a non populated database but don't throw if the migration was previously set
  when (isTxOutVariant && not isTxOutNull && not isTxOutAddressSet) $ do
    let msg = "The use of the config 'tx_out.use_address_table' can only be carried out on a non populated database."
    liftIO $ throwIO $ DbError (mkDbCallStack msgName) msg Nothing

  -- Make sure the config "use_address_table" is there if the migration wasn't previously set in the past
  when (not isTxOutVariant && isTxOutAddressSet) $ do
    let msg = "The configuration option 'tx_out.use_address_table' was previously set and the database updated. Unfortunately reverting this isn't possible."
    liftIO $ throwIO $ DbError (mkDbCallStack msgName) msg Nothing

  -- Has the user given txout address config && the migration wasn't previously set
  when (isTxOutVariant && not isTxOutAddressSet) $ do
    updateTxOutAndCreateAddress trce
    insertExtraMigration TxOutAddressPreviouslySet

  -- First check if pruneTxOut flag is missing and it has previously been used
  when (isPruneTxOutPreviouslySet migrationValues && not (pcmPruneTxOut pcm)) $ do
    let msg = "If --prune-tx-out flag is enabled and then db-sync is stopped all future executions of db-sync should still have this flag activated. Otherwise, it is considered bad usage and can cause crashes."
    liftIO $ throwIO $ DbError (mkDbCallStack msgName) msg Nothing

  handleMigration migrationValues
  where
    msgName = "runConsumedTxOutMigrations: "
    handleMigration :: MonadIO m => MigrationValues -> DbAction m ()
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
          migrateTxOut trce txOutVariantType $ Just migrationValues

        -- Prune TxOut
        (_, _, True) -> do
          unless isPruneTxOutPreviouslySet $
            insertExtraMigration PruneTxOutFlagPreviouslySet
          if isConsumeTxOutPreviouslySet
            then do
              liftIO $ logInfo trce $ msgName <> "Running extra migration prune tx_out"
              deleteConsumedTxOut trce txOutVariantType blockNoDiff
            else deleteAndUpdateConsumedTxOut trce txOutVariantType migrationValues blockNoDiff

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
queryTxOutIsNull :: MonadIO m => TxOutVariantType -> DbAction m Bool
queryTxOutIsNull = \case
  TxOutVariantCore -> queryTxOutIsNullImpl @SVC.TxOutCore
  TxOutVariantAddress -> queryTxOutIsNullImpl @SVA.TxOutAddress

-- | Implementation of queryTxOutIsNull using DbInfo
queryTxOutIsNullImpl :: forall a m. (DbInfo a, MonadIO m) => DbAction m Bool
queryTxOutIsNullImpl = do
  let tName = tableName (Proxy @a)
      stmt = queryTxOutIsNullStmt tName
  runDbSession (mkDbCallStack "queryTxOutIsNull") $
    HsqlSes.statement () stmt

--------------------------------------------------------------------------------

-- | Update tx_out tables and create address table
updateTxOutAndCreateAddress ::
  MonadIO m =>
  Trace IO Text.Text ->
  DbAction m ()
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
    runStep :: MonadIO m => Text.Text -> Text.Text -> DbAction m ()
    runStep stepDesc sql = do
      let sqlBS = TextEnc.encodeUtf8 sql
      runDbSession (mkDbCallStack "updateTxOutAndCreateAddress") $ HsqlSes.sql sqlBS
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
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Maybe MigrationValues ->
  DbAction m ()
migrateTxOut trce txOutVariantType mMvs = do
  whenJust mMvs $ \mvs -> do
    when (pcmConsumedTxOut (pruneConsumeMigration mvs) && not (isTxOutAddressPreviouslySet mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding consumed-by-id Index"
      createConsumedIndexTxOut
    when (pcmPruneTxOut (pruneConsumeMigration mvs)) $ do
      liftIO $ logInfo trce "migrateTxOut: adding prune contraint on tx_out table"
      createPruneConstraintTxOut
  migrateNextPageTxOut (Just trce) txOutVariantType 0

-- | Process the tx_out table in pages for migration
migrateNextPageTxOut ::
  MonadIO m =>
  Maybe (Trace IO Text.Text) ->
  TxOutVariantType ->
  Word64 ->
  DbAction m ()
migrateNextPageTxOut mTrce txOutVariantType offst = do
  whenJust mTrce $ \trce ->
    liftIO $ logInfo trce $ "migrateNextPageTxOut: Handling input offset " <> textShow offst
  page <- getInputPage offst
  updatePageEntries txOutVariantType page
  when (fromIntegral (length page) == pageSize) $
    migrateNextPageTxOut mTrce txOutVariantType $!
      (offst + pageSize)

--------------------------------------------------------------------------------

-- | Statement to update tx_out consumed_by_tx_id field
updateTxOutConsumedStmt ::
  forall a.
  (DbInfo a) =>
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
  MonadIO m =>
  TxOutVariantType ->
  ConsumedTriplet ->
  DbAction m ()
updateTxOutConsumedByTxIdUnique txOutVariantType triplet = do
  let dbCallStack = mkDbCallStack "updateTxOutConsumedByTxIdUnique"

  case txOutVariantType of
    TxOutVariantCore ->
      runDbSession dbCallStack $
        HsqlSes.statement triplet (updateTxOutConsumedStmt @SVC.TxOutCore)
    TxOutVariantAddress ->
      runDbSession dbCallStack $
        HsqlSes.statement triplet (updateTxOutConsumedStmt @SVA.TxOutAddress)

-- | Update page entries from a list of ConsumedTriplet
updatePageEntries ::
  MonadIO m =>
  TxOutVariantType ->
  [ConsumedTriplet] ->
  DbAction m ()
updatePageEntries txOutVariantType = mapM_ (updateTxOutConsumedByTxIdUnique txOutVariantType)

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
createConsumedIndexTxOut ::
  MonadIO m =>
  DbAction m ()
createConsumedIndexTxOut =
  runDbSession (mkDbCallStack "createConsumedIndexTxOut") $
    HsqlSes.statement () createConsumedIndexTxOutStmt

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
createPruneConstraintTxOut ::
  MonadIO m =>
  DbAction m ()
createPruneConstraintTxOut =
  runDbSession (mkDbCallStack "createPruneConstraintTxOut") $
    HsqlSes.statement () createPruneConstraintTxOutStmt

--------------------------------------------------------------------------------

-- | Get a page of consumed TX inputs
getInputPage ::
  MonadIO m =>
  -- | Offset
  Word64 ->
  DbAction m [ConsumedTriplet]
getInputPage offset =
  runDbSession (mkDbCallStack "getInputPage") $
    HsqlSes.statement offset getInputPageStmt

-- | Statement to get a page of inputs from tx_in table
getInputPageStmt :: HsqlStmt.Statement Word64 [ConsumedTriplet]
getInputPageStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out_id, tx_out_index, tx_in_id"
          , " FROM tx_in"
          , " ORDER BY id"
          , " LIMIT "
          , Text.pack (show pageSize)
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

--------------------------------------------------------------------------------

-- Statement function for finding max TxInId by block difference
findMaxTxInIdStmt :: HsqlStmt.Statement Word64 (Either Text.Text Id.TxId)
findMaxTxInIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH tip AS ("
          , "  SELECT MAX(block_no) AS max_block_no FROM block"
          , ")"
          , ", target_block AS ("
          , "  SELECT id FROM block WHERE block_no = (SELECT max_block_no - $1 FROM tip)"
          , ")"
          , ", max_tx AS ("
          , "  SELECT MAX(id) AS max_tx_id FROM tx"
          , "  WHERE block_id <= (SELECT id FROM target_block)"
          , ")"
          , "SELECT max_tx_id FROM max_tx"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

    decoder = HsqlD.singleRow $ do
      mTxId <- Id.maybeIdDecoder Id.TxId
      let result = case mTxId of
            Nothing -> Left "No transactions found before the specified block"
            Just txId -> Right txId
      pure result

findMaxTxInId :: MonadIO m => Word64 -> DbAction m (Either Text.Text Id.TxId)
findMaxTxInId blockNoDiff =
  runDbSession (mkDbCallStack "findMaxTxInId") $
    HsqlSes.statement blockNoDiff findMaxTxInIdStmt

--------------------------------------------------------------------------------

-- Delete consumed tx outputs before a specified tx
deleteConsumedBeforeTxStmt ::
  forall a.
  (DbInfo a) =>
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
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Id.TxId ->
  DbAction m ()
deleteConsumedBeforeTx trce txOutVariantType txId =
  runDbSession (mkDbCallStack "deleteConsumedBeforeTx") $ do
    countDeleted <- case txOutVariantType of
      TxOutVariantCore ->
        HsqlSes.statement (Just txId) (deleteConsumedBeforeTxStmt @SVC.TxOutCore)
      TxOutVariantAddress ->
        HsqlSes.statement (Just txId) (deleteConsumedBeforeTxStmt @SVA.TxOutAddress)
    liftIO $ logInfo trce $ "deleteConsumedBeforeTx: Deleted " <> textShow countDeleted <> " tx_out"

-- Delete consumed tx outputs
deleteConsumedTxOut ::
  forall m.
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Word64 ->
  DbAction m ()
deleteConsumedTxOut trce txOutVariantType blockNoDiff = do
  maxTxIdResult <- findMaxTxInId blockNoDiff
  case maxTxIdResult of
    Left errMsg -> liftIO $ logInfo trce $ "deleteConsumedTxOut: No tx_out was deleted: " <> errMsg
    Right txId -> deleteConsumedBeforeTx trce txOutVariantType txId

--------------------------------------------------------------------------------

-- Statement for deleting TxOut entries
deletePageEntriesStmt ::
  forall a.
  (DbInfo a) =>
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
  MonadIO m =>
  TxOutVariantType ->
  [ConsumedTriplet] ->
  DbAction m ()
deletePageEntries txOutVariantType entries =
  unless (null entries) $
    runDbSession (mkDbCallStack "deletePageEntries") $ do
      case txOutVariantType of
        TxOutVariantCore ->
          HsqlSes.statement entries (deletePageEntriesStmt @SVC.TxOutCore)
        TxOutVariantAddress ->
          HsqlSes.statement entries (deletePageEntriesStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- Statement for updating TxOut entries with consumed_by_tx_id
updatePageEntriesStmt ::
  forall a.
  (DbInfo a) =>
  HsqlStmt.Statement [ConsumedTriplet] ()
updatePageEntriesStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH entries AS ("
          , "  SELECT unnest($1::bigint[]) as tx_out_tx_id,"
          , "         unnest($2::int[]) as tx_out_index,"
          , "         unnest($3::bigint[]) as tx_in_tx_id"
          , ")"
          , "UPDATE " <> tableN
          , "SET consumed_by_tx_id = entries.tx_in_tx_id"
          , "WHERE (tx_id, index) IN (SELECT tx_out_tx_id, tx_out_index FROM entries)"
          ]

    encoder = contramap extract encodeConsumedTripletBulk

    extract :: [ConsumedTriplet] -> ([Id.TxId], [Word64], [Id.TxId])
    extract xs =
      ( map ctTxOutTxId xs
      , map ctTxOutIndex xs
      , map ctTxInTxId xs
      )

--------------------------------------------------------------------------------

-- Helper function for creating consumed index if needed
shouldCreateConsumedTxOut ::
  MonadIO m =>
  Trace IO Text.Text ->
  Bool ->
  DbAction m ()
shouldCreateConsumedTxOut trce rcc =
  unless rcc $ do
    liftIO $ logInfo trce "Created ConsumedTxOut when handling page entries."
    createConsumedIndexTxOut

--------------------------------------------------------------------------------

-- Split and process page entries
splitAndProcessPageEntries ::
  forall m.
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Bool ->
  Id.TxId ->
  [ConsumedTriplet] ->
  DbAction m Bool
splitAndProcessPageEntries trce txOutVariantType ranCreateConsumedTxOut maxTxId pageEntries = do
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
  forall m.
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  MigrationValues ->
  Word64 ->
  DbAction m ()
deleteAndUpdateConsumedTxOut trce txOutVariantType migrationValues blockNoDiff = do
  maxTxIdResult <- findMaxTxInId blockNoDiff
  case maxTxIdResult of
    Left errMsg -> do
      liftIO $ logInfo trce $ "No tx_out were deleted as no blocks found: " <> errMsg
      liftIO $ logInfo trce "deleteAndUpdateConsumedTxOut: Now Running extra migration prune tx_out"
      migrateTxOut trce txOutVariantType $ Just migrationValues
      insertExtraMigration ConsumeTxOutPreviouslySet
    Right maxTxId -> do
      migrateNextPage maxTxId False 0
  where
    migrateNextPage :: Id.TxId -> Bool -> Word64 -> DbAction m ()
    migrateNextPage maxTxId ranCreateConsumedTxOut offst = do
      pageEntries <- getInputPage offst
      resPageEntries <- splitAndProcessPageEntries trce txOutVariantType ranCreateConsumedTxOut maxTxId pageEntries
      when (fromIntegral (length pageEntries) == pageSize) $
        migrateNextPage maxTxId resPageEntries $!
          offst + pageSize

--------------------------------------------------------------------------------

migrateTxOutDbTool :: MonadIO m => TxOutVariantType -> DbAction m ()
migrateTxOutDbTool txOutVariantType = do
  createConsumedIndexTxOut
  migrateNextPageTxOut Nothing txOutVariantType 0

--------------------------------------------------------------------------------

-- | Update a list of TxOut consumed by TxId mappings
updateListTxOutConsumedByTxId :: MonadIO m => [(TxOutIdW, Id.TxId)] -> DbAction m ()
updateListTxOutConsumedByTxId = mapM_ (uncurry updateTxOutConsumedByTxId)
  where
    updateTxOutConsumedByTxId :: MonadIO m => TxOutIdW -> Id.TxId -> DbAction m ()
    updateTxOutConsumedByTxId txOutId txId =
      case txOutId of
        VCTxOutIdW txOutCoreId ->
          runDbSession (mkDbCallStack "updateTxOutConsumedByTxId") $
            HsqlSes.statement (txOutCoreId, Just txId) updateTxOutConsumedByTxIdCore
        VATxOutIdW txOutAddressId ->
          runDbSession (mkDbCallStack "updateTxOutConsumedByTxId") $
            HsqlSes.statement (txOutAddressId, Just txId) updateTxOutConsumedByTxIdAddress

-- | Statement to update Core TxOut consumed_by_tx_id field by ID
updateTxOutConsumedByTxIdCore ::
  HsqlStmt.Statement (Id.TxOutCoreId, Maybe Id.TxId) ()
updateTxOutConsumedByTxIdCore =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    tableN = tableName (Proxy @SVC.TxOutCore)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> tableN
          , " SET consumed_by_tx_id = $2"
          , " WHERE id = $1"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ Id.getTxOutCoreId >$< HsqlE.int8)
        , snd >$< HsqlE.param (HsqlE.nullable $ Id.getTxId >$< HsqlE.int8)
        ]

-- | Statement to update Address TxOut consumed_by_tx_id field by ID
updateTxOutConsumedByTxIdAddress ::
  HsqlStmt.Statement (Id.TxOutAddressId, Maybe Id.TxId) ()
updateTxOutConsumedByTxIdAddress =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    tableN = tableName (Proxy @SVA.TxOutAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> tableN
          , " SET consumed_by_tx_id = $2"
          , " WHERE id = $1"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ Id.getTxOutAddressId >$< HsqlE.int8)
        , snd >$< HsqlE.param (HsqlE.nullable $ Id.getTxId >$< HsqlE.int8)
        ]

--------------------------------------------------------------------------------

-- | Count of TxOuts with null consumed_by_tx_id
queryTxOutConsumedNullCountStmt ::
  forall a.
  (DbInfo a) =>
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
queryTxOutConsumedNullCount :: MonadIO m => TxOutVariantType -> DbAction m Word64
queryTxOutConsumedNullCount = \case
  TxOutVariantCore ->
    runDbSession (mkDbCallStack "queryTxOutConsumedNullCount") $
      HsqlSes.statement () (queryTxOutConsumedNullCountStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runDbSession (mkDbCallStack "queryTxOutConsumedNullCount") $
      HsqlSes.statement () (queryTxOutConsumedNullCountStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- | Count of TxOuts with non-null consumed_by_tx_id
queryTxOutConsumedCountStmt ::
  forall a.
  (DbInfo a) =>
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

queryTxOutConsumedCount :: MonadIO m => TxOutVariantType -> DbAction m Word64
queryTxOutConsumedCount = \case
  TxOutVariantCore ->
    runDbSession (mkDbCallStack "queryTxOutConsumedCount") $
      HsqlSes.statement () (queryTxOutConsumedCountStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runDbSession (mkDbCallStack "queryTxOutConsumedCount") $
      HsqlSes.statement () (queryTxOutConsumedCountStmt @SVA.TxOutAddress)

--------------------------------------------------------------------------------

-- | Statement for querying TxOuts where consumed_by_tx_id equals tx_id
queryWrongConsumedByStmt ::
  forall a.
  (DbInfo a) =>
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
queryWrongConsumedBy :: MonadIO m => TxOutVariantType -> DbAction m Word64
queryWrongConsumedBy = \case
  TxOutVariantCore ->
    runDbSession (mkDbCallStack "queryWrongConsumedBy") $
      HsqlSes.statement () (queryWrongConsumedByStmt @SVC.TxOutCore)
  TxOutVariantAddress ->
    runDbSession (mkDbCallStack "queryWrongConsumedBy") $
      HsqlSes.statement () (queryWrongConsumedByStmt @SVA.TxOutAddress)
