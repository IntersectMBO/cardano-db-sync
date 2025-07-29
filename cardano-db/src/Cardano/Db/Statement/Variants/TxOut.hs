{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Variants.TxOut where

import Cardano.Prelude (ByteString, Int64, MonadIO (..), Proxy (..), Text, Word64, fromMaybe, textShow, throwIO)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.Base as SVC
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants (CollateralTxOutIdW (..), CollateralTxOutW (..), MaTxOutIdW (..), MaTxOutW (..), TxOutIdW (..), TxOutVariantType (..), TxOutW (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSessionMain)
import Cardano.Db.Statement.Function.Delete (deleteAllCount)
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Function.Query (adaDecoder, countAll)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (entityVal))
import Cardano.Db.Types (Ada (..), DbAction, DbLovelace, DbWord64, dbLovelaceDecoder)

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- INSERTS ---------------------------------------------------------------------

insertTxOutCoreStmt :: HsqlStmt.Statement SVC.TxOutCore Id.TxOutCoreId
insertTxOutCoreStmt =
  insert
    SVC.txOutCoreEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxOutCoreId)

insertTxOutAddressStmt :: HsqlStmt.Statement SVA.TxOutAddress Id.TxOutAddressId
insertTxOutAddressStmt =
  insert
    SVA.txOutAddressEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxOutAddressId)

insertTxOut :: MonadIO m => TxOutW -> DbAction m TxOutIdW
insertTxOut txOutW =
  case txOutW of
    VCTxOutW txOut -> do
      txOutId <-
        runDbSessionMain (mkDbCallStack "insertTxOutCore") $
          HsqlSes.statement txOut insertTxOutCoreStmt
      pure $ VCTxOutIdW txOutId
    VATxOutW txOut _ -> do
      txOutId <-
        runDbSessionMain (mkDbCallStack "insertTxOutAddress") $
          HsqlSes.statement txOut insertTxOutAddressStmt
      pure $ VATxOutIdW txOutId

--------------------------------------------------------------------------------
insertBulkCoreTxOutStmt :: HsqlStmt.Statement [SVC.TxOutCore] [Id.TxOutCoreId]
insertBulkCoreTxOutStmt =
  insertBulk
    extractCoreTxOutValues
    SVC.txOutCoreBulkEncoder
    (WithResultBulk $ HsqlD.rowList $ Id.idDecoder Id.TxOutCoreId)
  where
    extractCoreTxOutValues ::
      [SVC.TxOutCore] ->
      ( [Id.TxId]
      , [Word64]
      , [Text]
      , [Bool]
      , [Maybe ByteString]
      , [Maybe Id.StakeAddressId]
      , [DbLovelace]
      , [Maybe ByteString]
      , [Maybe Id.DatumId]
      , [Maybe Id.ScriptId]
      , [Maybe Id.TxId]
      )
    extractCoreTxOutValues xs =
      ( map SVC.txOutCoreTxId xs
      , map SVC.txOutCoreIndex xs
      , map SVC.txOutCoreAddress xs
      , map SVC.txOutCoreAddressHasScript xs
      , map SVC.txOutCorePaymentCred xs
      , map SVC.txOutCoreStakeAddressId xs
      , map SVC.txOutCoreValue xs
      , map SVC.txOutCoreDataHash xs
      , map SVC.txOutCoreInlineDatumId xs
      , map SVC.txOutCoreReferenceScriptId xs
      , map SVC.txOutCoreConsumedByTxId xs
      )

insertBulkAddressTxOutStmt :: HsqlStmt.Statement [SVA.TxOutAddress] [Id.TxOutAddressId]
insertBulkAddressTxOutStmt =
  insertBulk
    extractAddressTxOutValues
    SVA.txOutAddressBulkEncoder
    (WithResultBulk $ HsqlD.rowList $ Id.idDecoder Id.TxOutAddressId)
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
              runDbSessionMain (mkDbCallStack "insertBulkTxOutCore") $
                HsqlSes.statement coreTxOuts insertBulkCoreTxOutStmt
            pure $ map VCTxOutIdW ids
          VATxOutW _ _ -> do
            let variantTxOuts = map extractVariantTxOut txOuts
            ids <-
              runDbSessionMain (mkDbCallStack "insertBulkTxOutAddress") $
                HsqlSes.statement variantTxOuts insertBulkAddressTxOutStmt
            pure $ map VATxOutIdW ids
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
      runDbSessionMain (mkDbCallStack "queryTxOutCountCore") $
        HsqlSes.statement () (countAll @SVC.TxOutCore)
    TxOutVariantAddress ->
      runDbSessionMain (mkDbCallStack "queryTxOutCountAddress") $
        HsqlSes.statement () (countAll @SVA.TxOutAddress)

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
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int2)

    decoder =
      HsqlD.rowMaybe
        ( (,)
            <$> Id.idDecoder Id.TxId
            <*> HsqlD.column (HsqlD.nonNullable HsqlD.int8)
        )

queryTxOutIdEither ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Either DbError (Id.TxId, TxOutIdW))
queryTxOutIdEither txOutVariantType hashIndex@(hash, _) = do
  result <- runDbSessionMain dbCallStack $ HsqlSes.statement hashIndex queryTxOutIdStmt
  case result of
    Just (txId, rawId) ->
      pure $ Right $ case txOutVariantType of
        TxOutVariantCore -> (txId, VCTxOutIdW (Id.TxOutCoreId rawId))
        TxOutVariantAddress -> (txId, VATxOutIdW (Id.TxOutAddressId rawId))
    Nothing ->
      pure $ Left $ DbError dbCallStack errorMsg Nothing
  where
    dbCallStack = mkDbCallStack "queryTxOutIdEither"
    errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

queryTxOutId ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Id.TxId, TxOutIdW)
queryTxOutId txOutVariantType hashIndex@(hash, _) = do
  result <- runDbSessionMain dbCallStack $ HsqlSes.statement hashIndex queryTxOutIdStmt
  case result of
    Just (txId, rawId) ->
      pure $ case txOutVariantType of
        TxOutVariantCore -> (txId, VCTxOutIdW (Id.TxOutCoreId rawId))
        TxOutVariantAddress -> (txId, VATxOutIdW (Id.TxOutAddressId rawId))
    Nothing ->
      liftIO $ throwIO $ DbError dbCallStack errorMsg Nothing
  where
    dbCallStack = mkDbCallStack "queryTxOutId"
    errorMsg = "TxOut not found for hash: " <> Text.pack (show hash)

--------------------------------------------------------------------------------

queryTxOutIdByTxIdStmt :: HsqlStmt.Statement (Id.TxId, Word64) (Maybe Int64)
queryTxOutIdByTxIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.id"
          , " FROM tx_out"
          , " WHERE tx_out.tx_id = $1 AND tx_out.index = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getTxId)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

resolveInputTxOutIdFromTxId ::
  MonadIO m =>
  Id.TxId ->
  Word64 ->
  DbAction m (Either DbError TxOutIdW)
resolveInputTxOutIdFromTxId txId index = do
  result <-
    runDbSessionMain (mkDbCallStack "resolveInputTxOutIdFromTxId") $
      HsqlSes.statement (txId, index) queryTxOutIdByTxIdStmt
  case result of
    Just txOutId -> pure $ Right $ VCTxOutIdW (Id.TxOutCoreId txOutId) -- Adjust based on your variant
    Nothing ->
      pure $
        Left $
          DbError
            (mkDbCallStack "resolveInputTxOutIdFromTxId")
            ("TxOut not found for txId: " <> textShow txId <> ", index: " <> textShow index)
            Nothing

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

queryTxOutIdValueEither ::
  MonadIO m =>
  TxOutVariantType ->
  (ByteString, Word64) ->
  DbAction m (Either DbError (Id.TxId, TxOutIdW, DbLovelace))
queryTxOutIdValueEither txOutVariantType hashIndex@(hash, _) = do
  result <-
    runDbSessionMain (mkDbCallStack "queryTxOutIdValue") $
      HsqlSes.statement hashIndex queryTxOutIdValueStmt
  pure $ case result of
    Just (txId, rawId, value) ->
      Right $ case txOutVariantType of
        TxOutVariantCore -> (txId, VCTxOutIdW (Id.TxOutCoreId rawId), value)
        TxOutVariantAddress -> (txId, VATxOutIdW (Id.TxOutAddressId rawId), value)
    Nothing ->
      Left $ DbError (mkDbCallStack "queryTxOutIdValueEither") ("TxOut not found for hash: " <> Text.pack (show hash)) Nothing

--------------------------------------------------------------------------------
queryTxOutCredentialsCoreStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Maybe ByteString))
queryTxOutCredentialsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_out.payment_cred"
          , " FROM tx INNER JOIN tx_out ON tx.id = tx_out.tx_id"
          , " WHERE tx_out.index = $2 AND tx.hash = $1"
          ]

    encoder =
      contramap fst (HsqlE.param $ HsqlE.nonNullable HsqlE.bytea)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder =
      HsqlD.rowMaybe $ HsqlD.column (HsqlD.nullable HsqlD.bytea)

queryTxOutCredentialsVariantStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe (Maybe ByteString))
queryTxOutCredentialsVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT addr.payment_cred"
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
queryTxOutCredentials txOutVariantType hashIndex = do
  -- Just return Nothing when not found, don't throw
  result <- case txOutVariantType of
    TxOutVariantCore ->
      runDbSessionMain (mkDbCallStack "queryTxOutCredentials") $
        HsqlSes.statement hashIndex queryTxOutCredentialsCoreStmt
    TxOutVariantAddress ->
      runDbSessionMain (mkDbCallStack "queryTxOutCredentials") $
        HsqlSes.statement hashIndex queryTxOutCredentialsVariantStmt

  case result of
    Just mPaamentCred -> pure mPaamentCred -- Extract the inner Maybe ByteString
    Nothing -> pure Nothing

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
  runDbSessionMain (mkDbCallStack "queryTotalSupply") $
    HsqlSes.statement () queryTotalSupplyStmt

queryGenesisSupplyStmt :: Text -> HsqlStmt.Statement () Ada
queryGenesisSupplyStmt txOutTableName =
  HsqlStmt.Statement sql HsqlE.noParams (HsqlD.singleRow adaDecoder) True
  where
    txTable = tableName (Proxy @SVC.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> txOutTableName <> ".value), 0)::bigint"
          , " FROM " <> txTable
          , " INNER JOIN " <> txOutTableName <> " ON tx.id = " <> txOutTableName <> ".tx_id"
          , " INNER JOIN block ON tx.block_id = block.id"
          , " WHERE block.previous_id IS NULL"
          ]

queryGenesisSupply :: MonadIO m => TxOutVariantType -> DbAction m Ada
queryGenesisSupply txOutVariantType = do
  case txOutVariantType of
    TxOutVariantCore ->
      runDbSessionMain (mkDbCallStack "queryGenesisSupplyCore") $
        HsqlSes.statement () (queryGenesisSupplyStmt (tableName (Proxy @SVC.TxOutCore)))
    TxOutVariantAddress ->
      runDbSessionMain (mkDbCallStack "queryGenesisSupplyAddress") $
        HsqlSes.statement () (queryGenesisSupplyStmt (tableName (Proxy @SVA.TxOutAddress)))

--------------------------------------------------------------------------------
queryShelleyGenesisSupplyStmt :: Text -> HsqlStmt.Statement () Ada
queryShelleyGenesisSupplyStmt txOutTableName =
  HsqlStmt.Statement sql HsqlE.noParams (HsqlD.singleRow adaDecoder) True
  where
    txTable = tableName (Proxy @SVC.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> txOutTableName <> ".value), 0)::bigint"
          , " FROM " <> txOutTableName
          , " INNER JOIN " <> txTable <> " ON " <> txOutTableName <> ".tx_id = tx.id"
          , " INNER JOIN block ON tx.block_id = block.id"
          , " WHERE block.previous_id IS NOT NULL"
          , " AND block.epoch_no IS NULL"
          ]

queryShelleyGenesisSupply :: MonadIO m => TxOutVariantType -> DbAction m Ada
queryShelleyGenesisSupply txOutVariantType = do
  case txOutVariantType of
    TxOutVariantCore ->
      runDbSessionMain (mkDbCallStack "queryShelleyGenesisSupplyCore") $
        HsqlSes.statement () (queryShelleyGenesisSupplyStmt (tableName (Proxy @SVC.TxOutCore)))
    TxOutVariantAddress ->
      runDbSessionMain (mkDbCallStack "queryShelleyGenesisSupplyAddress") $
        HsqlSes.statement () (queryShelleyGenesisSupplyStmt (tableName (Proxy @SVA.TxOutAddress)))

--------------------------------------------------------------------------------
-- DELETES

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
    runDbSessionMain (mkDbCallStack "deleteTxOutCore") $
      HsqlSes.statement () deleteTxOutCoreAllCountStmt
  TxOutVariantAddress ->
    runDbSessionMain (mkDbCallStack "deleteTxOutAddress") $
      HsqlSes.statement () deleteTxOutAddressAllCountStmt

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
insertAddressStmt :: HsqlStmt.Statement SVA.Address Id.AddressId
insertAddressStmt =
  insert
    SVA.addressEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.AddressId)

insertAddress :: MonadIO m => SVA.Address -> DbAction m Id.AddressId
insertAddress address =
  runDbSessionMain (mkDbCallStack "insertAddress") $
    HsqlSes.statement address insertAddressStmt

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
  runDbSessionMain (mkDbCallStack "queryAddressId") $
    HsqlSes.statement addrRaw queryAddressIdStmt

--------------------------------------------------------------------------------
-- MaTxOut
--------------------------------------------------------------------------------
insertBulkCoreMaTxOutStmt :: HsqlStmt.Statement [SVC.MaTxOutCore] [Id.MaTxOutCoreId]
insertBulkCoreMaTxOutStmt =
  insertBulk
    extractCoreMaTxOutValues
    SVC.maTxOutCoreBulkEncoder
    (WithResultBulk $ HsqlD.rowList $ Id.idDecoder Id.MaTxOutCoreId)
  where
    extractCoreMaTxOutValues ::
      [SVC.MaTxOutCore] ->
      ( [DbWord64]
      , [Id.TxOutCoreId]
      , [Id.MultiAssetId]
      )
    extractCoreMaTxOutValues xs =
      ( map SVC.maTxOutCoreQuantity xs
      , map SVC.maTxOutCoreTxOutId xs
      , map SVC.maTxOutCoreIdent xs
      )

insertBulkAddressMaTxOutStmt :: HsqlStmt.Statement [SVA.MaTxOutAddress] [Id.MaTxOutAddressId]
insertBulkAddressMaTxOutStmt =
  insertBulk
    extractAddressMaTxOutValues
    SVA.maTxOutAddressBulkEncoder
    (WithResultBulk $ HsqlD.rowList $ Id.idDecoder Id.MaTxOutAddressId)
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
            runDbSessionMain (mkDbCallStack "insertBulkCoreMaTxOut") $
              HsqlSes.statement coreMaTxOuts insertBulkCoreMaTxOutStmt
          pure $ map CMaTxOutIdW ids
        VMaTxOutW _ -> do
          let addressMaTxOuts = map extractVariantMaTxOut maTxOuts
          ids <-
            runDbSessionMain (mkDbCallStack "insertBulkAddressMaTxOut") $
              HsqlSes.statement addressMaTxOuts insertBulkAddressMaTxOutStmt
          pure $ map VMaTxOutIdW ids
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
insertCollateralTxOutCoreStmt :: HsqlStmt.Statement SVC.CollateralTxOutCore Id.CollateralTxOutCoreId
insertCollateralTxOutCoreStmt =
  insert
    SVC.collateralTxOutCoreEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CollateralTxOutCoreId)

insertCollateralTxOutAddressStmt :: HsqlStmt.Statement SVA.CollateralTxOutAddress Id.CollateralTxOutAddressId
insertCollateralTxOutAddressStmt =
  insert
    SVA.collateralTxOutAddressEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CollateralTxOutAddressId)

insertCollateralTxOut :: MonadIO m => CollateralTxOutW -> DbAction m CollateralTxOutIdW
insertCollateralTxOut collateralTxOutW =
  case collateralTxOutW of
    VCCollateralTxOutW txOut -> do
      txOutId <-
        runDbSessionMain (mkDbCallStack "insertCollateralTxOutCore") $
          HsqlSes.statement txOut insertCollateralTxOutCoreStmt
      pure $ VCCollateralTxOutIdW txOutId
    VACollateralTxOutW txOut -> do
      txOutId <-
        runDbSessionMain (mkDbCallStack "insertCollateralTxOutAddress") $
          HsqlSes.statement txOut insertCollateralTxOutAddressStmt
      pure $ VACollateralTxOutIdW txOutId

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
  runDbSessionMain (mkDbCallStack "queryTxOutUnspentCount") $
    HsqlSes.statement () queryTxOutUnspentCountStmt

--------------------------------------------------------------------------------
queryAddressOutputsCoreStmt :: HsqlStmt.Statement Text DbLovelace
queryAddressOutputsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(value), 0)"
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
          [ "SELECT COALESCE(SUM(tx_out.value), 0)"
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
      runDbSessionMain (mkDbCallStack "queryAddressOutputsCore") $
        HsqlSes.statement addr queryAddressOutputsCoreStmt
    TxOutVariantAddress ->
      runDbSessionMain (mkDbCallStack "queryAddressOutputsVariant") $
        HsqlSes.statement addr queryAddressOutputsVariantStmt

--------------------------------------------------------------------------------
queryScriptOutputsCoreStmt :: HsqlStmt.Statement () [Entity SVC.TxOutCore]
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
    decoder = HsqlD.rowList SVC.entityTxOutCoreDecoder

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
        runDbSessionMain (mkDbCallStack "queryScriptOutputsCore") $
          HsqlSes.statement () queryScriptOutputsCoreStmt
      pure $ map (VCTxOutW . entityVal) txOuts
    TxOutVariantAddress -> do
      results <-
        runDbSessionMain (mkDbCallStack "queryScriptOutputsVariant") $
          HsqlSes.statement () queryScriptOutputsVariantStmt
      pure $ map (\(txOut, addr) -> VATxOutW txOut (Just addr)) results

--------------------------------------------------------------------------------
-- UPDATES
--------------------------------------------------------------------------------

-- Batch update statement
setNullTxOutConsumedBatchStmt ::
  forall a.
  DbInfo a =>
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
      let dbCallStack = mkDbCallStack "querySetNullTxOut"
      -- Decide which table to use based on the TxOutVariantType
      updatedCount <- case txOutVariantType of
        TxOutVariantCore ->
          runDbSessionMain dbCallStack $
            HsqlSes.statement txId (setNullTxOutConsumedBatchStmt @SVC.TxOutCore)
        TxOutVariantAddress ->
          runDbSessionMain dbCallStack $
            HsqlSes.statement txId (setNullTxOutConsumedBatchStmt @SVA.TxOutAddress)
      -- Return result
      if updatedCount == 0
        then pure ("No tx_out to set to null (no matching records found)", 0)
        else pure ("tx_out.consumed_by_tx_id", updatedCount)
