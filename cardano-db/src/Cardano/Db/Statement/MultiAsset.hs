{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.MultiAsset where

import Cardano.Prelude (ByteString, MonadIO)
import Data.Functor.Contravariant (Contravariant (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Schema.Core.MultiAsset (MaTxMint)
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction, DbInt65)

--------------------------------------------------------------------------------
-- MultiAsset
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertMultiAssetStmt :: HsqlStmt.Statement SMA.MultiAsset (Entity SMA.MultiAsset)
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

-- | QUERY -------------------------------------------------------------------
queryMultiAssetIdStmt :: HsqlStmt.Statement (ByteString, ByteString) (Maybe Id.MultiAssetId)
queryMultiAssetIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM multi_asset"
          , " WHERE policy = $1 AND name = $2"
          ]

    encoder =
      contramap fst (HsqlE.param (HsqlE.nonNullable HsqlE.bytea))
        <> contramap snd (HsqlE.param (HsqlE.nonNullable HsqlE.bytea))

    decoder = HsqlD.rowMaybe (Id.idDecoder Id.MultiAssetId)

queryMultiAssetId :: MonadIO m => ByteString -> ByteString -> DbAction m (Maybe Id.MultiAssetId)
queryMultiAssetId policy assetName =
  runDbSession (mkCallInfo "queryMultiAssetId") $
    HsqlSes.statement (policy, assetName) queryMultiAssetIdStmt

--------------------------------------------------------------------------------
-- MaTxMint
--------------------------------------------------------------------------------
insertMaTxMintStmt :: HsqlStmt.Statement SMA.MaTxMint (Entity SMA.MaTxMint)
insertMaTxMintStmt =
  insert
    SMA.maTxMintEncoder
    (WithResult $ HsqlD.singleRow SMA.entityMaTxMintDecoder)

insertMaTxMint :: MonadIO m => SMA.MaTxMint -> DbAction m Id.MaTxMintId
insertMaTxMint maTxMint = do
  entity <- runDbSession (mkCallInfo "insertMaTxMint") $ HsqlSes.statement maTxMint insertMaTxMintStmt
  pure $ entityKey entity

insertBulkMaTxMintStmt :: HsqlStmt.Statement [SMA.MaTxMint] [Entity MaTxMint]
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
