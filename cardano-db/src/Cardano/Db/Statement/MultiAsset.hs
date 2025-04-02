module Cardano.Db.Statement.MultiAsset where

import Cardano.Db (DbWord64)
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbTransMode (..))

--------------------------------------------------------------------------------

-- | MultiAsset

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
      HsqlS.statement multiAsset insertMultiAssetStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | MaTxMint

--------------------------------------------------------------------------------
insertMaTxMintStmt :: HsqlS.Statement SMA.MaTxMint (Entity SMA.MaTxMint)
insertMaTxMintStmt =
  insert
    SMA.maTxMintEncoder
    (WithResult $ HsqlD.singleRow SMA.entityMaTxMintDecoder)

insertMaTxMint :: MonadIO m => SMA.MaTxMint -> DbAction m Id.MaTxMintId
insertMaTxMint maTxMint = do
  entity <- runDbSession (mkCallInfo "insertMaTxMint") $ HsqlS.statement maTxMint insertMaTxMintStmt
  pure $ entityKey entity

bulkInsertMaTxMint :: MonadIO m => [SMA.MaTxMint] -> DbAction m [Id.MaTxMintId]
bulkInsertMaTxMint maTxMints =
  runDbT TransWrite $ mkDbTransaction "bulkInsertTxInMetadata" $ do
    entity <-
      bulkInsert
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
