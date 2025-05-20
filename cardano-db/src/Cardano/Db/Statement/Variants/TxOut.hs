{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Variants.TxOut where

import Cardano.Prelude (ByteString, Int64, MonadError (..), MonadIO, Proxy (..), Text, Word64, fromMaybe)
import Control.Monad.Extra (whenJust)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants (CollateralTxOutIdW (..), CollateralTxOutW (..), MaTxOutIdW (..), MaTxOutW (..), TxOutIdW (..), TxOutVariantType (..), TxOutW (..), UtxoQueryResult (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Delete (deleteAllCount, parameterisedDeleteWhere)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk)
import Cardano.Db.Statement.Function.Query (countAll)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (Ada (..), DbAction, DbCallInfo (..), DbLovelace, DbWord64, dbLovelaceDecoder)

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- INSERTS ---------------------------------------------------------------------

insertTxOutCoreStmt :: HsqlStmt.Statement SVC.TxOutCore (Entity SVC.TxOutCore)
insertTxOutCoreStmt =
  insert
    SVC.txOutCoreEncoder
    (WithResult $ HsqlD.singleRow SVC.entityTxOutCoreDecoder)

insertTxOutAddressStmt :: HsqlStmt.Statement SVA.TxOutAddress (Entity SVA.TxOutAddress)
insertTxOutAddressStmt =
  insert
    SVA.txOutAddressEncoder
    (WithResult $ HsqlD.singleRow SVA.entityTxOutAddressDecoder)

insertTxOut :: MonadIO m => TxOutW -> DbAction m TxOutIdW
insertTxOut txOutW =
  case txOutW of
    VCTxOutW txOut -> do
      txOutId <-
        runDbSession (mkCallInfo "insertTxOutCore") $
          HsqlSes.statement txOut insertTxOutCoreStmt
      pure $ VCTxOutIdW $ entityKey txOutId
    VATxOutW txOut _ -> do
      txOutId <-
        runDbSession (mkCallInfo "insertTxOutAddress") $
          HsqlSes.statement txOut insertTxOutAddressStmt
      pure $ VATxOutIdW $ entityKey txOutId

--------------------------------------------------------------------------------
insertBulkCoreTxOutStmt :: HsqlStmt.Statement [SVC.TxOutCore] [Entity SVC.TxOutCore]
insertBulkCoreTxOutStmt =
  insertBulk
    extractCoreTxOutValues
    SVC.txOutCoreBulkEncoder
    (WithResultBulk $ HsqlD.rowList SVC.entityTxOutCoreDecoder)
  where
    extractCoreTxOutValues ::
      [SVC.TxOutCore] ->
      ( [Text]
      , [Bool]
      , [Maybe ByteString]
      , [Maybe Id.TxId]
      , [Word64]
      , [Maybe Id.DatumId]
      , [Maybe ByteString]
      , [Maybe Id.ScriptId]
      , [Maybe Id.StakeAddressId]
      , [Id.TxId]
      , [DbLovelace]
      )
    extractCoreTxOutValues xs =
      ( map SVC.txOutCoreAddress xs
      , map SVC.txOutCoreAddressHasScript xs
      , map SVC.txOutCoreDataHash xs
      , map SVC.txOutCoreConsumedByTxId xs
      , map SVC.txOutCoreIndex xs
      , map SVC.txOutCoreInlineDatumId xs
      , map SVC.txOutCorePaymentCred xs
      , map SVC.txOutCoreReferenceScriptId xs
      , map SVC.txOutCoreStakeAddressId xs
      , map SVC.txOutCoreTxId xs
      , map SVC.txOutCoreValue xs
      )

insertBulkAddressTxOutStmt :: HsqlStmt.Statement [SVA.TxOutAddress] [Entity SVA.TxOutAddress]
insertBulkAddressTxOutStmt =
  insertBulk
    extractAddressTxOutValues
    SVA.txOutAddressBulkEncoder
    (WithResultBulk $ HsqlD.rowList SVA.entityTxOutAddressDecoder)
  where
    extractAddressTxOutValues ::
      [SVA.TxOutAddress] ->
      ( [Id.TxId]
      , [Word64]
      , [Maybe Id.StakeAddressId]
      , [DbLovelace]
      , [Maybe ByteString]
      , [Maybe Id.DatumId]
      , [Maybe Id.ScriptId]
      , [Maybe Id.TxId]
      , [Id.AddressId]
      )
    extractAddressTxOutValues xs =
      ( map SVA.txOutAddressTxId xs
      , map SVA.txOutAddressIndex xs
      , map SVA.txOutAddressStakeAddressId xs
      , map SVA.txOutAddressValue xs
      , map SVA.txOutAddressDataHash xs
      , map SVA.txOutAddressInlineDatumId xs
      , map SVA.txOutAddressReferenceScriptId xs
      , map SVA.txOutAddressConsumedByTxId xs
      , map SVA.txOutAddressAddressId xs
      )

insertBulkTxOut :: MonadIO m => Bool -> [TxOutW] -> DbAction m [TxOutIdW]
insertBulkTxOut disInOut txOutWs =
  if disInOut
    then pure []
    else case txOutWs of
      [] -> pure []
      txOuts@(txOutW : _) ->
        case txOutW of
          VCTxOutW _ -> do
            let coreTxOuts = map extractCoreTxOut txOuts
            ids <-
              runDbSession (mkCallInfo "insertBulkTxOutCore") $
                HsqlSes.statement coreTxOuts insertBulkCoreTxOutStmt
            pure $ map (VCTxOutIdW . entityKey) ids
          VATxOutW _ _ -> do
            let variantTxOuts = map extractVariantTxOut txOuts
            ids <-
              runDbSession (mkCallInfo "insertBulkTxOutAddress") $
                HsqlSes.statement variantTxOuts insertBulkAddressTxOutStmt
            pure $ map (VATxOutIdW . entityKey) ids
  where
    extractCoreTxOut :: TxOutW -> SVC.TxOutCore
    extractCoreTxOut (VCTxOutW txOut) = txOut
    extractCoreTxOut (VATxOutW _ _) = error "Unexpected VATxOutW in CoreTxOut list"

    extractVariantTxOut :: TxOutW -> SVA.TxOutAddress
    extractVariantTxOut (VATxOutW txOut _) = txOut
    extractVariantTxOut (VCTxOutW _) = error "Unexpected VCTxOutW in VariantTxOut list"

-- | QUERIES -------------------------------------------------------------------
queryTxOutCount :: MonadIO m => TxOutVariantType -> DbAction m Word64
queryTxOutCount txOutVariantType =
  case txOutVariantType of
    TxOutVariantCore ->
      runDbSession (mkCallInfo "queryTxOutCountCore") $
        HsqlSes.statement () (countAll @SVC.TxOutCore)
    TxOutVariantAddress ->
      runDbSession (mkCallInfo "queryTxOutCountAddress") $
        HsqlSes.statement () (countAll @SVA.TxOutAddress)

--------------------------------------------------------------------------------
queryTxOutValueStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Id.TxId, DbLovelace))
queryTxOutValueStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.tx_id, tx_out.value"
          , " FROM tx INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]
    -- Parameter encoder for (hash, index)
    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    -- Result decoder for (TxId, DbLovelace)
    decoder =
      HsqlD.rowMaybe
        ( (,)
            <$> Id.idDecoder Id.TxId
            <*> dbLovelaceDecoder
        )

-- | Query the value of a TxOut by its hash and index,
-- this works the same for both variations of TxOut
queryTxOutValue ::
  MonadIO m =>
  (ByteString, Word64) ->
  DbAction m (Id.TxId, DbLovelace)
queryTxOutValue hashIndex@(hash, _) = do
  result <- runDbSession callInfo $ HsqlSes.statement hashIndex queryTxOutValueStmt
  case result of
    Just value -> pure value
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryTxOutValue"
    errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

--------------------------------------------------------------------------------
queryTxOutIdStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Id.TxId, Int64))
queryTxOutIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.tx_id, tx_out.id"
          , " FROM tx INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]

    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder =
      HsqlD.rowMaybe
        ( (,)
            <$> Id.idDecoder Id.TxId
            <*> HsqlD.column (HsqlD.nonNullable HsqlD.int8)
        )

queryTxOutId ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Id.TxId, TxOutIdW)
queryTxOutId txOutVariantType hashIndex@(hash, _) = do
  result <- runDbSession callInfo $ HsqlSes.statement hashIndex queryTxOutIdStmt
  case result of
    Just (txId, rawId) ->
      pure $ case txOutVariantType of
        TxOutVariantCore -> (txId, VCTxOutIdW (Id.TxOutCoreId rawId))
        TxOutVariantAddress -> (txId, VATxOutIdW (Id.TxOutAddressId rawId))
    Nothing ->
      throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryTxOutId"
    errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

--------------------------------------------------------------------------------
queryTxOutIdValueStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Id.TxId, Int64, DbLovelace))
queryTxOutIdValueStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.tx_id, tx_out.id, tx_out.value"
          , " FROM tx INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]

    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder =
      HsqlD.rowMaybe
        ( (,,)
            <$> Id.idDecoder Id.TxId
            <*> HsqlD.column (HsqlD.nonNullable HsqlD.int8)
            <*> dbLovelaceDecoder
        )

queryTxOutIdValue ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Id.TxId, TxOutIdW, DbLovelace)
queryTxOutIdValue txOutVariantType hashIndex@(hash, _) = do
  let callInfo = mkCallInfo "queryTxOutIdValue"
      errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

  result <- runDbSession callInfo $ HsqlSes.statement hashIndex queryTxOutIdValueStmt
  case result of
    Just (txId, rawId, value) ->
      pure $ case txOutVariantType of
        TxOutVariantCore -> (txId, VCTxOutIdW (Id.TxOutCoreId rawId), value)
        TxOutVariantAddress -> (txId, VATxOutIdW (Id.TxOutAddressId rawId), value)
    Nothing ->
      throwError $ DbError (dciCallSite callInfo) errorMsg Nothing

--------------------------------------------------------------------------------
queryTxOutCredentialsCoreStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Maybe ByteString))
queryTxOutCredentialsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.payment_cred, tx_out.address_has_script"
          , " FROM tx INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]

    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder =
      HsqlD.rowMaybe $ HsqlD.column (HsqlD.nullable HsqlD.bytea)


--------------------------------------------------------------------------------
queryTxOutCredentialsVariantStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Maybe ByteString))
queryTxOutCredentialsVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT addr.payment_cred, addr.address_has_script"
          , " FROM tx"
          , " INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " INNER JOIN address addr ON tx_out.address_id = addr.id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]

    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder =
      HsqlD.rowMaybe $ HsqlD.column (HsqlD.nullable HsqlD.bytea)

queryTxOutCredentials ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Maybe ByteString)
queryTxOutCredentials txOutVariantType hashIndex@(hash, _) = do
  let callInfo = mkCallInfo "queryTxOutCredentials"
      errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

  result <- case txOutVariantType of
    TxOutVariantCore ->
      runDbSession callInfo $ HsqlSes.statement hashIndex queryTxOutCredentialsCoreStmt
    TxOutVariantAddress ->
      runDbSession callInfo $ HsqlSes.statement hashIndex queryTxOutCredentialsVariantStmt

  case result of
    Just credentials -> pure credentials
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing

--------------------------------------------------------------------------------
queryTotalSupplyStmt :: HsqlStmt.Statement () Ada
queryTotalSupplyStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(value), 0)::bigint"
          , " FROM tx_out"
          , " WHERE NOT EXISTS ("
          , "   SELECT 1 FROM tx_in"
          , "   WHERE tx_in.tx_out_id = tx_out.tx_id"
          , "     AND tx_in.tx_out_index = tx_out.index"
          , " )"
          ]

    decoder =
      HsqlD.singleRow $
        fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))

-- | Get the current total supply of Lovelace. This only returns the on-chain supply which
-- does not include staking rewards that have not yet been withdrawn. Before wihdrawal
-- rewards are part of the ledger state and hence not on chain.
queryTotalSupply :: MonadIO m => TxOutVariantType -> DbAction m Ada
queryTotalSupply _ =
  runDbSession (mkCallInfo "queryTotalSupply") $
    HsqlSes.statement () queryTotalSupplyStmt

--------------------------------------------------------------------------------
-- DELETES

-- Statement for deleting MaTxOutCore and TxOutVariantCore records after specific IDs
deleteMaTxOutCoreAfterIdStmt :: HsqlStmt.Statement Id.MaTxOutCoreId ()
deleteMaTxOutCoreAfterIdStmt =
  parameterisedDeleteWhere @SVC.MaTxOutCore
    "id"
    ">= $1"
    (Id.idEncoder Id.getMaTxOutCoreId)

deleteTxOutCoreAfterIdStmt :: HsqlStmt.Statement Id.TxOutCoreId ()
deleteTxOutCoreAfterIdStmt =
  parameterisedDeleteWhere @SVC.TxOutCore
    "id"
    ">= $1"
    (Id.idEncoder Id.getTxOutCoreId)

-- Function that uses the core delete statements
deleteCoreTxOutTablesAfterTxId :: MonadIO m => Maybe Id.TxOutCoreId -> Maybe Id.MaTxOutCoreId -> DbAction m ()
deleteCoreTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  let callInfo = mkCallInfo "deleteCoreTxOutTablesAfterTxId"

  -- Delete MaTxOut entries if ID provided
  whenJust mmaTxOutId $ \maTxOutId ->
    runDbSession callInfo $ HsqlSes.statement maTxOutId deleteMaTxOutCoreAfterIdStmt

  -- Delete TxOut entries if ID provided
  whenJust mtxOutId $ \txOutId ->
    runDbSession callInfo $ HsqlSes.statement txOutId deleteTxOutCoreAfterIdStmt

--------------------------------------------------------------------------------
-- Statement for deleting MaTxOutAddress and TxOutAddress records after specific IDs
deleteMaTxOutAddressAfterIdStmt :: HsqlStmt.Statement Id.MaTxOutAddressId ()
deleteMaTxOutAddressAfterIdStmt =
  parameterisedDeleteWhere @SVA.MaTxOutAddress
    "id"
    ">= $1"
    (Id.idEncoder Id.getMaTxOutAddressId)

deleteTxOutAddressAfterIdStmt :: HsqlStmt.Statement Id.TxOutAddressId ()
deleteTxOutAddressAfterIdStmt =
  parameterisedDeleteWhere @SVA.TxOutAddress
    "id"
    ">= $1"
    (Id.idEncoder Id.getTxOutAddressId)

-- Function that uses the address variant delete statements
deleteVariantTxOutTablesAfterTxId :: MonadIO m => Maybe Id.TxOutAddressId -> Maybe Id.MaTxOutAddressId -> DbAction m ()
deleteVariantTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  let callInfo = mkCallInfo "deleteVariantTxOutTablesAfterTxId"

  -- Delete MaTxOut entries if ID provided
  whenJust mmaTxOutId $ \maTxOutId ->
    runDbSession callInfo $ HsqlSes.statement maTxOutId deleteMaTxOutAddressAfterIdStmt

  -- Delete TxOut entries if ID provided
  whenJust mtxOutId $ \txOutId ->
    runDbSession callInfo $ HsqlSes.statement txOutId deleteTxOutAddressAfterIdStmt

--------------------------------------------------------------------------------
-- Statements for deleting all records and returning counts
deleteTxOutCoreAllCountStmt :: HsqlStmt.Statement () Int64
deleteTxOutCoreAllCountStmt = deleteAllCount @SVC.TxOutCore

deleteTxOutAddressAllCountStmt :: HsqlStmt.Statement () Int64
deleteTxOutAddressAllCountStmt = deleteAllCount @SVA.TxOutAddress

-- Function that uses the delete all count statements
deleteTxOut :: MonadIO m => TxOutVariantType -> DbAction m Int64
deleteTxOut = \case
  TxOutVariantCore ->
    runDbSession (mkCallInfo "deleteTxOutCore") $
      HsqlSes.statement () deleteTxOutCoreAllCountStmt
  TxOutVariantAddress ->
    runDbSession (mkCallInfo "deleteTxOutAddress") $
      HsqlSes.statement () deleteTxOutAddressAllCountStmt

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
insertAddressStmt :: HsqlStmt.Statement SVA.Address (Entity SVA.Address)
insertAddressStmt =
  insert
    SVA.addressEncoder
    (WithResult $ HsqlD.singleRow SVA.entityAddressDecoder)

insertAddress :: MonadIO m => SVA.Address -> DbAction m Id.AddressId
insertAddress address = do
  addrId <-
    runDbSession (mkCallInfo "insertAddress") $
      HsqlSes.statement address insertAddressStmt
  pure $ entityKey addrId

queryAddressIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.AddressId)
queryAddressIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT address.id"
          , " FROM address"
          , " WHERE address.raw = $1"
          ]
    encoder = HsqlE.param $ HsqlE.nonNullable HsqlE.bytea
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.AddressId)

queryAddressId :: MonadIO m => ByteString -> DbAction m (Maybe Id.AddressId)
queryAddressId addrRaw =
  runDbSession (mkCallInfo "queryAddressId") $
    HsqlSes.statement addrRaw queryAddressIdStmt

--------------------------------------------------------------------------------
-- MaTxOut
--------------------------------------------------------------------------------
insertBulkCoreMaTxOutStmt :: HsqlStmt.Statement [SVC.MaTxOutCore] [Entity SVC.MaTxOutCore]
insertBulkCoreMaTxOutStmt =
  insertBulk
    extractCoreMaTxOutValues
    SVC.maTxOutCoreBulkEncoder
    (WithResultBulk $ HsqlD.rowList SVC.entityMaTxOutCoreDecoder)
  where
    extractCoreMaTxOutValues ::
      [SVC.MaTxOutCore] ->
      ( [Id.MultiAssetId]
      , [DbWord64]
      , [Id.TxOutCoreId]
      )
    extractCoreMaTxOutValues xs =
      ( map SVC.maTxOutCoreIdent xs
      , map SVC.maTxOutCoreQuantity xs
      , map SVC.maTxOutCoreTxOutId xs
      )

insertBulkAddressMaTxOutStmt :: HsqlStmt.Statement [SVA.MaTxOutAddress] [Entity SVA.MaTxOutAddress]
insertBulkAddressMaTxOutStmt =
  insertBulk
    extractAddressMaTxOutValues
    SVA.maTxOutAddressBulkEncoder
    (WithResultBulk $ HsqlD.rowList SVA.entityMaTxOutAddressDecoder)
  where
    extractAddressMaTxOutValues ::
      [SVA.MaTxOutAddress] ->
      ( [Id.MultiAssetId]
      , [DbWord64]
      , [Id.TxOutAddressId]
      )
    extractAddressMaTxOutValues xs =
      ( map SVA.maTxOutAddressIdent xs
      , map SVA.maTxOutAddressQuantity xs
      , map SVA.maTxOutAddressTxOutId xs
      )

insertBulkMaTxOut :: MonadIO m => [MaTxOutW] -> DbAction m [MaTxOutIdW]
insertBulkMaTxOut maTxOutWs =
  case maTxOutWs of
    [] -> pure []
    maTxOuts@(maTxOutW : _) ->
      case maTxOutW of
        CMaTxOutW _ -> do
          let coreMaTxOuts = map extractCoreMaTxOut maTxOuts
          ids <-
            runDbSession (mkCallInfo "insertBulkCoreMaTxOut") $
              HsqlSes.statement coreMaTxOuts insertBulkCoreMaTxOutStmt
          pure $ map (CMaTxOutIdW . entityKey) ids
        VMaTxOutW _ -> do
          let addressMaTxOuts = map extractVariantMaTxOut maTxOuts
          ids <-
            runDbSession (mkCallInfo "insertBulkAddressMaTxOut") $
              HsqlSes.statement addressMaTxOuts insertBulkAddressMaTxOutStmt
          pure $ map (VMaTxOutIdW . entityKey) ids
  where
    extractCoreMaTxOut :: MaTxOutW -> SVC.MaTxOutCore
    extractCoreMaTxOut (CMaTxOutW maTxOut) = maTxOut
    extractCoreMaTxOut (VMaTxOutW _) = error "Unexpected VMaTxOutW in CoreMaTxOut list"

    extractVariantMaTxOut :: MaTxOutW -> SVA.MaTxOutAddress
    extractVariantMaTxOut (VMaTxOutW maTxOut) = maTxOut
    extractVariantMaTxOut (CMaTxOutW _) = error "Unexpected CMaTxOutW in VariantMaTxOut list"

--------------------------------------------------------------------------------
-- CollateralTxOut
--------------------------------------------------------------------------------
insertCollateralTxOutCoreStmt :: HsqlStmt.Statement SVC.CollateralTxOutCore (Entity SVC.CollateralTxOutCore)
insertCollateralTxOutCoreStmt =
  insert
    SVC.collateralTxOutCoreEncoder
    (WithResult $ HsqlD.singleRow SVC.entityCollateralTxOutCoreDecoder)

insertCollateralTxOutAddressStmt :: HsqlStmt.Statement SVA.CollateralTxOutAddress (Entity SVA.CollateralTxOutAddress)
insertCollateralTxOutAddressStmt =
  insert
    SVA.collateralTxOutAddressEncoder
    (WithResult $ HsqlD.singleRow SVA.entityCollateralTxOutAddressDecoder)

insertCollateralTxOut :: MonadIO m => CollateralTxOutW -> DbAction m CollateralTxOutIdW
insertCollateralTxOut collateralTxOutW =
  case collateralTxOutW of
    CCollateralTxOutW txOut -> do
      txOutId <-
        runDbSession (mkCallInfo "insertCollateralTxOutCore") $
          HsqlSes.statement txOut insertCollateralTxOutCoreStmt
      pure $ CCollateralTxOutIdW $ entityKey txOutId
    VCollateralTxOutW txOut -> do
      txOutId <-
        runDbSession (mkCallInfo "insertCollateralTxOutAddress") $
          HsqlSes.statement txOut insertCollateralTxOutAddressStmt
      pure $ VCollateralTxOutIdW $ entityKey txOutId

--------------------------------------------------------------------------------
-- Testing or validating. Queries below are not used in production
--------------------------------------------------------------------------------
queryTxOutUnspentCountStmt :: HsqlStmt.Statement () Word64
queryTxOutUnspentCountStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM tx_out"
          , " WHERE NOT EXISTS ("
          , "   SELECT 1 FROM tx_in"
          , "   WHERE tx_in.tx_out_id = tx_out.tx_id"
          , "     AND tx_in.tx_out_index = tx_out.index"
          , " )"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryTxOutUnspentCount :: MonadIO m => TxOutVariantType -> DbAction m Word64
queryTxOutUnspentCount _ =
  runDbSession (mkCallInfo "queryTxOutUnspentCount") $
    HsqlSes.statement () queryTxOutUnspentCountStmt

--------------------------------------------------------------------------------
utxoAtBlockIdWhereClause :: Text
utxoAtBlockIdWhereClause =
  Text.concat
    [ " WHERE txout.tx_id IN ("
    , "   SELECT tx.id FROM tx"
    , "   WHERE tx.block_id IN ("
    , "     SELECT block.id FROM block"
    , "     WHERE block.id <= $1"
    , "   )"
    , " )"
    , " AND (blk.block_no IS NULL OR blk.id > $1)"
    , " AND tx2.hash IS NOT NULL" -- Filter out NULL hashes
    ]

queryUtxoAtBlockIdCoreStmt :: HsqlStmt.Statement Id.BlockId [UtxoQueryResult]
queryUtxoAtBlockIdCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*, txout.address, tx2.hash"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , utxoAtBlockIdWhereClause
          ]

    encoder = Id.idEncoder Id.getBlockId

    decoder = HsqlD.rowList $ do
      txOut <- SVC.txOutCoreDecoder
      address <- HsqlD.column (HsqlD.nonNullable HsqlD.text)
      txHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea) -- Now non-nullable
      pure $
        UtxoQueryResult
          { utxoTxOutW = VCTxOutW txOut
          , utxoAddress = address
          , utxoTxHash = txHash
          }

queryUtxoAtBlockIdVariantStmt :: HsqlStmt.Statement Id.BlockId [UtxoQueryResult]
queryUtxoAtBlockIdVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*, addr.*, tx2.hash"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , " INNER JOIN address addr ON txout.address_id = addr.id"
          , utxoAtBlockIdWhereClause
          ]

    encoder = Id.idEncoder Id.getBlockId

    decoder = HsqlD.rowList $ do
      txOut <- SVA.txOutAddressDecoder
      addr <- SVA.addressDecoder
      txHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea) -- Now non-nullable
      pure $
        UtxoQueryResult
          { utxoTxOutW = VATxOutW txOut (Just addr)
          , utxoAddress = SVA.addressAddress addr
          , utxoTxHash = txHash
          }

--------------------------------------------------------------------------------
-- Query to get block ID at a specific slot
queryBlockIdAtSlotStmt :: HsqlStmt.Statement Word64 (Maybe Id.BlockId)
queryBlockIdAtSlotStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM block"
          , " WHERE slot_no = $1"
          ]

    encoder = HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8
    decoder = HsqlD.rowMaybe $ Id.idDecoder Id.BlockId

-- Shared WHERE clause for address balance queries
addressBalanceWhereClause :: Text
addressBalanceWhereClause =
  Text.concat
    [ " WHERE txout.tx_id IN ("
    , "   SELECT tx.id FROM tx"
    , "   WHERE tx.block_id IN ("
    , "     SELECT block.id FROM block"
    , "     WHERE block.id <= $1"
    , "   )"
    , " )"
    , " AND (blk.block_no IS NULL OR blk.id > $1)"
    ]

-- Query to get address balance for Core variant
queryAddressBalanceAtBlockIdCoreStmt :: HsqlStmt.Statement (Id.BlockId, Text) Ada
queryAddressBalanceAtBlockIdCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(txout.value), 0)::bigint"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , addressBalanceWhereClause
          , " AND txout.address = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getBlockId)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable HsqlE.text)

    decoder =
      HsqlD.singleRow $
        fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))

-- Query to get address balance for Variant variant
queryAddressBalanceAtBlockIdVariantStmt :: HsqlStmt.Statement (Id.BlockId, Text) Ada
queryAddressBalanceAtBlockIdVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(txout.value), 0)::bigint"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , " INNER JOIN address addr ON txout.address_id = addr.id"
          , addressBalanceWhereClause
          , " AND addr.address = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getBlockId)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable HsqlE.text)

    decoder =
      HsqlD.singleRow $
        fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))

-- Main query function
queryAddressBalanceAtSlot :: MonadIO m => TxOutVariantType -> Text -> Word64 -> DbAction m Ada
queryAddressBalanceAtSlot txOutVariantType addr slotNo = do
  let callInfo = mkCallInfo "queryAddressBalanceAtSlot"

  -- First get the block ID for the slot
  mBlockId <-
    runDbSession callInfo $
      HsqlSes.statement slotNo queryBlockIdAtSlotStmt

  -- If no block at that slot, return 0
  case mBlockId of
    Nothing -> pure $ Ada 0
    Just blockId ->
      case txOutVariantType of
        TxOutVariantCore ->
          runDbSession (mkCallInfo "queryAddressBalanceAtBlockIdCore") $
            HsqlSes.statement (blockId, addr) queryAddressBalanceAtBlockIdCoreStmt
        TxOutVariantAddress ->
          runDbSession (mkCallInfo "queryAddressBalanceAtBlockIdVariant") $
            HsqlSes.statement (blockId, addr) queryAddressBalanceAtBlockIdVariantStmt

--------------------------------------------------------------------------------
queryAddressOutputsCoreStmt :: HsqlStmt.Statement Text DbLovelace
queryAddressOutputsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(value), 0)::bigint"
          , " FROM tx_out"
          , " WHERE address = $1"
          ]
    encoder = HsqlE.param $ HsqlE.nonNullable HsqlE.text
    decoder = HsqlD.singleRow dbLovelaceDecoder

queryAddressOutputsVariantStmt :: HsqlStmt.Statement Text DbLovelace
queryAddressOutputsVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(tx_out.value), 0)::bigint"
          , " FROM address"
          , " JOIN tx_out ON tx_out.address_id = address.id"
          , " WHERE address.address = $1"
          ]
    encoder = HsqlE.param $ HsqlE.nonNullable HsqlE.text
    decoder = HsqlD.singleRow dbLovelaceDecoder

queryAddressOutputs :: MonadIO m => TxOutVariantType -> Text -> DbAction m DbLovelace
queryAddressOutputs txOutVariantType addr =
  case txOutVariantType of
    TxOutVariantCore ->
      runDbSession (mkCallInfo "queryAddressOutputsCore") $
        HsqlSes.statement addr queryAddressOutputsCoreStmt
    TxOutVariantAddress ->
      runDbSession (mkCallInfo "queryAddressOutputsVariant") $
        HsqlSes.statement addr queryAddressOutputsVariantStmt

--------------------------------------------------------------------------------
queryScriptOutputsCoreStmt :: HsqlStmt.Statement () [SVC.TxOutCore]
queryScriptOutputsCoreStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM tx_out"
          , " WHERE address_has_script = TRUE"
          ]
    decoder = HsqlD.rowList SVC.txOutCoreDecoder

queryScriptOutputsVariantStmt :: HsqlStmt.Statement () [(SVA.TxOutAddress, SVA.Address)]
queryScriptOutputsVariantStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.*, address.*"
          , " FROM address"
          , " JOIN tx_out ON tx_out.address_id = address.id"
          , " WHERE address.address_has_script = TRUE"
          ]
    decoder = HsqlD.rowList $ (,) <$> SVA.txOutAddressDecoder <*> SVA.addressDecoder

queryScriptOutputs :: MonadIO m => TxOutVariantType -> DbAction m [TxOutW]
queryScriptOutputs txOutVariantType =
  case txOutVariantType of
    TxOutVariantCore -> do
      txOuts <-
        runDbSession (mkCallInfo "queryScriptOutputsCore") $
          HsqlSes.statement () queryScriptOutputsCoreStmt
      pure $ map VCTxOutW txOuts
    TxOutVariantAddress -> do
      results <-
        runDbSession (mkCallInfo "queryScriptOutputsVariant") $
          HsqlSes.statement () queryScriptOutputsVariantStmt
      pure $ map (\(txOut, addr) -> VATxOutW txOut (Just addr)) results

--------------------------------------------------------------------------------
-- UPDATES
--------------------------------------------------------------------------------

-- Batch update statement
setNullTxOutConsumedBatchStmt ::
  forall a.
  (DbInfo a) =>
  HsqlStmt.Statement Id.TxId Int64
setNullTxOutConsumedBatchStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    tableN = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH updated AS ("
          , "  UPDATE " <> tableN
          , "  SET consumed_by_tx_id = NULL"
          , "  WHERE consumed_by_tx_id >= $1"
          , "  RETURNING 1"
          , ")"
          , "SELECT COUNT(*)::bigint FROM updated"
          ]
    encoder = Id.idEncoder Id.getTxId
    decoder = HsqlD.singleRow (HsqlD.column (HsqlD.nonNullable HsqlD.int8))

-- Main function to set NULL for tx_out consumed_by_tx_id
querySetNullTxOut ::
  MonadIO m =>
  TxOutVariantType ->
  Maybe Id.TxId ->
  DbAction m (Text.Text, Int64)
querySetNullTxOut txOutVariantType mMinTxId = do
  case mMinTxId of
    Nothing -> pure ("No tx_out to set to null (no TxId provided)", 0)
    Just txId -> do
      let callInfo = mkCallInfo "querySetNullTxOut"
      -- Decide which table to use based on the TxOutVariantType
      updatedCount <- case txOutVariantType of
        TxOutVariantCore ->
          runDbSession callInfo $
            HsqlSes.statement txId (setNullTxOutConsumedBatchStmt @SVC.TxOutCore)
        TxOutVariantAddress ->
          runDbSession callInfo $
            HsqlSes.statement txId (setNullTxOutConsumedBatchStmt @SVA.TxOutAddress)
      -- Return result
      if updatedCount == 0
        then pure ("No tx_out to set to null (no matching records found)", 0)
        else pure ("tx_out.consumed_by_tx_id", updatedCount)
