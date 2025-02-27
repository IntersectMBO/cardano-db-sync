module Cardano.Db.Statement.MultiAsset where

import Cardano.Db.Schema.Core.MultiAsset (MaTxMint(..))
import Cardano.Db.Types (DbAction)
import Cardano.Db.Schema.Ids (MaTxMintId)
import qualified Hasql.Transaction as HsqlT
import Cardano.Db (DbWord64)


insertManyMaTxMint :: MonadIO m => [MaTxMint] -> DbAction m [MaTxMintId]
insertManyMaTxMint maTxMints = runDbT Write $ mkDbTransaction "insertManyTxInMetadata" $
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

-- These tables handle multi-asset (native token) data.

-- multi_asset
-- ma_tx_mint
-- ma_tx_out
