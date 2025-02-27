module Cardano.Db.Statement.MultiAsset where

import Cardano.Db.Schema.Core.MultiAsset (MaTxMint(..))
import Cardano.Db.Types (DbAction, DbTransMode (..))
import Cardano.Db.Schema.Ids (MaTxMintId)
import qualified Hasql.Transaction as HsqlT
import Cardano.Db (DbWord64)

--------------------------------------------------------------------------------
-- | MultiAsset
--------------------------------------------------------------------------------
insertMultiAsset :: MonadIO m => MultiAsset -> DbAction m MultiAssetId
insertMultiAsset multiAsset = runDbT TransWrite $ mkDbTransaction "insertMultiAsset" $
  insert
    multiAssetEncoder
    (WithResult (HsqlD.singleRow $ idDecoder MultiAssetId))
    multiAsset

--------------------------------------------------------------------------------
-- | MaTxMint
--------------------------------------------------------------------------------
insertManyMaTxMint :: MonadIO m => [MaTxMint] -> DbAction m [MaTxMintId]
insertManyMaTxMint maTxMints = runDbT TransWrite $ mkDbTransaction "insertManyTxInMetadata" $
  bulkInsertReturnIds
    extractMaTxMint
    maTxMintEncoderMany
    (HsqlD.rowList $ idDecoder MaTxMintId)
    maTxMints
  where
    extractMaTxMint :: [MaTxMint] -> ([DbInt65], [MultiAssetId], [TxId])
    extractMaTxMint xs =
      ( map maTxMintQuantity xs
      , map maTxMintIdent xs
      , map maTxMintTxId xs
      )

insertMaTxMint :: MonadIO m => MaTxMint -> DbAction m MaTxMintId
insertMaTxMint maTxMint = runDbT TransWrite $ mkDbTransaction "insertMaTxMint" $
  insert
    maTxMint
    (WithResult (HsqlD.singleRow $ idDecoder MaTxMintId))
    maTxMint

-- These tables handle multi-asset (native token) data.

-- multi_asset
-- ma_tx_mint
-- ma_tx_out
