{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cardano.Db.Operations.TxOut.TxOutInsert where

import Cardano.Db.Operations.Insert (insertMany', insertUnchecked)
import Cardano.Db.Operations.Types (CollateralTxOutIdW (..), CollateralTxOutW (..), MaTxOutIdW (..), MaTxOutW (..), TxOutIdW (..), TxOutW (..))
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (
  SqlBackend,
 )

--------------------------------------------------------------------------------
-- insertManyTxOut - Insert a list of TxOut into the database.
--------------------------------------------------------------------------------
insertManyTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  Bool ->
  [TxOutW] ->
  ReaderT SqlBackend m [TxOutIdW]
insertManyTxOut disInOut txOutWs = do
  if disInOut
    then pure []
    else case txOutWs of
      [] -> pure []
      txOuts@(txOutW : _) ->
        case txOutW of
          CTxOutW _ -> do
            vals <- insertMany' "insertManyTxOutC" (map extractCoreTxOut txOuts)
            pure $ map CTxOutIdW vals
          VTxOutW _ _ -> do
            vals <- insertMany' "insertManyTxOutV" (map extractVariantTxOut txOuts)
            pure $ map VTxOutIdW vals
  where
    extractCoreTxOut :: TxOutW -> C.TxOut
    extractCoreTxOut (CTxOutW txOut) = txOut
    extractCoreTxOut (VTxOutW _ _) = error "Unexpected VTxOutW in CoreTxOut list"

    extractVariantTxOut :: TxOutW -> V.TxOut
    extractVariantTxOut (VTxOutW txOut _) = txOut
    extractVariantTxOut (CTxOutW _) = error "Unexpected CTxOutW in VariantTxOut list"

--------------------------------------------------------------------------------
-- insertTxOut - Insert a TxOut into the database.
--------------------------------------------------------------------------------
insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOutW -> ReaderT SqlBackend m TxOutIdW
insertTxOut txOutW = do
  case txOutW of
    CTxOutW txOut -> do
      val <- insertUnchecked "insertTxOutC" txOut
      pure $ CTxOutIdW val
    VTxOutW txOut _ -> do
      val <- insertUnchecked "insertTxOutV" txOut
      pure $ VTxOutIdW val

--------------------------------------------------------------------------------
-- insertAddress - Insert a Address into the database.
--------------------------------------------------------------------------------
insertAddress :: (MonadBaseControl IO m, MonadIO m) => V.Address -> ReaderT SqlBackend m V.AddressId
insertAddress = insertUnchecked "insertAddress"

--------------------------------------------------------------------------------
-- insertManyMaTxOut - Insert a list of MultiAsset TxOut into the database.
--------------------------------------------------------------------------------
insertManyMaTxOut :: (MonadBaseControl IO m, MonadIO m) => [MaTxOutW] -> ReaderT SqlBackend m [MaTxOutIdW]
insertManyMaTxOut maTxOutWs = do
  case maTxOutWs of
    [] -> pure []
    maTxOuts@(maTxOutW : _) ->
      case maTxOutW of
        CMaTxOutW _ -> do
          vals <- insertMany' "Many Variant MaTxOut" (map extractCoreMaTxOut maTxOuts)
          pure $ map CMaTxOutIdW vals
        VMaTxOutW _ -> do
          vals <- insertMany' "Many Variant MaTxOut" (map extractVariantMaTxOut maTxOuts)
          pure $ map VMaTxOutIdW vals
  where
    extractCoreMaTxOut :: MaTxOutW -> C.MaTxOut
    extractCoreMaTxOut (CMaTxOutW maTxOut) = maTxOut
    extractCoreMaTxOut (VMaTxOutW _) = error "Unexpected VMaTxOutW in CoreMaTxOut list"

    extractVariantMaTxOut :: MaTxOutW -> V.MaTxOut
    extractVariantMaTxOut (VMaTxOutW maTxOut) = maTxOut
    extractVariantMaTxOut (CMaTxOutW _) = error "Unexpected CMaTxOutW in VariantMaTxOut list"

insertCollateralTxOut :: (MonadBaseControl IO m, MonadIO m) => CollateralTxOutW -> ReaderT SqlBackend m CollateralTxOutIdW
insertCollateralTxOut collateralTxOutW =
  case collateralTxOutW of
    CCollateralTxOutW txOut -> do
      val <- insertUnchecked "CollateralTxOut" txOut
      pure $ CCollateralTxOutIdW val
    VCollateralTxOutW txOut -> do
      val <- insertUnchecked "CollateralTxOut" txOut
      pure $ VCollateralTxOutIdW val
