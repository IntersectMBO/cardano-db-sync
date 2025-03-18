module Cardano.Db.Statement.MultiAsset where

import qualified Hasql.Transaction as HsqlT

import Cardano.Db (DbWord64)
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbTransMode (..))

--------------------------------------------------------------------------------
-- | MultiAsset
--------------------------------------------------------------------------------
insertMultiAsset :: MonadIO m => SMA.MultiAsset -> DbAction m Id.MultiAssetId
insertMultiAsset multiAsset =
  runDbT TransWrite $ mkDbTransaction "insertMultiAsset" $ do
    entity <- insert
      SMA.multiAssetEncoder
      (WithResult $ HsqlD.singleRow SMA.entityMultiAssetDecoder)
      multiAsset
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | MaTxMint
--------------------------------------------------------------------------------
insertMaTxMint :: MonadIO m => MaTxMint -> DbAction m MaTxMintId
insertMaTxMint maTxMint =
  runDbT TransWrite $ mkDbTransaction "insertMaTxMint" $ do
    entity <- insert
      maTxMint
      (WithResult $ HsqlD.singleRow SMA.entityMaTxMintDecoder)
      maTxMint
    pure (entityKey entity)

bulkInsertMaTxMint :: MonadIO m => [SMA.MaTxMint] -> DbAction m [Id.MaTxMintId]
bulkInsertMaTxMint maTxMints =
  runDbT TransWrite $ mkDbTransaction "bulkInsertTxInMetadata" $ do
    entity <- bulkInsert
      extractMaTxMint
      SMA.maTxMintBulkEncoder
      (HsqlD.rowList SMA.entityMaTxMintDecoder)
      maTxMints
    pure (map entityKey entity)
  where
    extractMaTxMint :: [MaTxMint] -> ([DbInt65], [MultiAssetId], [TxId])
    extractMaTxMint xs =
      ( map maTxMintQuantity xs
      , map maTxMintIdent xs
      , map maTxMintTxId xs
      )

-- These tables handle multi-asset (native token) data.

-- multi_asset
-- ma_tx_mint
-- ma_tx_out
