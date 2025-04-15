{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.MultiAsset where

import Cardano.Db.Schema.Core.MultiAsset (MaTxMint)
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction, DbInt65)
import Cardano.Prelude (MonadIO)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlS

--------------------------------------------------------------------------------
-- MultiAsset
--------------------------------------------------------------------------------

-- | INSERT
insertMultiAssetStmt :: HsqlS.Statement SMA.MultiAsset (Entity SMA.MultiAsset)
insertMultiAssetStmt =
  insert
    SMA.multiAssetEncoder
    (WithResult $ HsqlD.singleRow SMA.entityMultiAssetDecoder)

insertMultiAsset :: MonadIO m => SMA.MultiAsset -> DbAction m Id.MultiAssetId
insertMultiAsset multiAsset = do
  entity <-
    runDbSession (mkCallInfo "insertMultiAsset") $
      HsqlSes.statement multiAsset insertMultiAssetStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- MaTxMint
--------------------------------------------------------------------------------
insertMaTxMintStmt :: HsqlS.Statement SMA.MaTxMint (Entity SMA.MaTxMint)
insertMaTxMintStmt =
  insert
    SMA.maTxMintEncoder
    (WithResult $ HsqlD.singleRow SMA.entityMaTxMintDecoder)

insertMaTxMint :: MonadIO m => SMA.MaTxMint -> DbAction m Id.MaTxMintId
insertMaTxMint maTxMint = do
  entity <- runDbSession (mkCallInfo "insertMaTxMint") $ HsqlSes.statement maTxMint insertMaTxMintStmt
  pure $ entityKey entity

insertBulkMaTxMintStmt :: HsqlS.Statement [SMA.MaTxMint] [Entity MaTxMint]
insertBulkMaTxMintStmt =
  insertBulk
    extractMaTxMint
    SMA.maTxMintBulkEncoder
    (WithResultBulk (HsqlD.rowList SMA.entityMaTxMintDecoder))
  where
    extractMaTxMint :: [MaTxMint] -> ([DbInt65], [Id.MultiAssetId], [Id.TxId])
    extractMaTxMint xs =
      ( map SMA.maTxMintQuantity xs
      , map SMA.maTxMintIdent xs
      , map SMA.maTxMintTxId xs
      )

insertBulkMaTxMint :: MonadIO m => [SMA.MaTxMint] -> DbAction m [Id.MaTxMintId]
insertBulkMaTxMint maTxMints = do
  ids <-
    runDbSession (mkCallInfo "insertBulkMaTxMint") $
      HsqlSes.statement maTxMints insertBulkMaTxMintStmt
  pure $ map entityKey ids

-- These tables handle multi-asset (native token) data.

-- multi_asset
-- ma_tx_mint
-- ma_tx_out
