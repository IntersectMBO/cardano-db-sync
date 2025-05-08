{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cardano.Db.Operations.TxOut.TxOutInsert where

-- import Cardano.Db.Operations.Insert (insertMany', insertUnchecked)
-- import Cardano.Db.Operations.Types (CollateralTxOutIdW (..), CollateralTxOutW (..), MaTxOutIdW (..), MaTxOutW (..), TxOutIdW (..), TxOutW (..))
-- import qualified Cardano.Db.Schema.Variants.TxOutAddress as V
-- import qualified Cardano.Db.Schema.Variants.TxOutCore as C
-- import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Control.Monad.Trans.Reader (ReaderT)
-- import Database.Persist.Sql (
--   SqlBackend,
--  )

-- insertCollateralTxOut :: MonadIO m => CollateralTxOutW -> DB.DbAction m CollateralTxOutIdW
-- insertCollateralTxOut collateralTxOutW =
--   case collateralTxOutW of
--     CCollateralTxOutW txOut -> do
--       val <- insertUnchecked "CollateralTxOut" txOut
--       pure $ CCollateralTxOutIdW val
--     VCollateralTxOutW txOut -> do
--       val <- insertUnchecked "CollateralTxOut" txOut
--       pure $ VCollateralTxOutIdW val
