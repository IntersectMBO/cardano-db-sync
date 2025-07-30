{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSessionMain, runDbSessionPool)
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Types (DbAction, DbInt65)

--------------------------------------------------------------------------------
-- MultiAsset
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertMultiAssetStmt :: HsqlStmt.Statement SMA.MultiAsset Id.MultiAssetId
insertMultiAssetStmt =
  insert
    SMA.multiAssetEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.MultiAssetId)

insertMultiAsset :: MonadIO m => SMA.MultiAsset -> DbAction m Id.MultiAssetId
insertMultiAsset multiAsset =
  runDbSessionMain (mkDbCallStack "insertMultiAsset") $
    HsqlSes.statement multiAsset insertMultiAssetStmt

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
  runDbSessionMain (mkDbCallStack "queryMultiAssetId") $
    HsqlSes.statement (policy, assetName) queryMultiAssetIdStmt

--------------------------------------------------------------------------------
-- MaTxMint
--------------------------------------------------------------------------------

insertBulkMaTxMintStmt :: HsqlStmt.Statement [SMA.MaTxMint] [Id.MaTxMintId]
insertBulkMaTxMintStmt =
  insertBulk
    extractMaTxMint
    SMA.maTxMintBulkEncoder
    (WithResultBulk (HsqlD.rowList $ Id.idDecoder Id.MaTxMintId))
  where
    extractMaTxMint :: [MaTxMint] -> ([DbInt65], [Id.TxId], [Id.MultiAssetId])
    extractMaTxMint xs =
      ( map SMA.maTxMintQuantity xs
      , map SMA.maTxMintTxId xs
      , map SMA.maTxMintIdent xs
      )

insertBulkMaTxMint :: MonadIO m => [SMA.MaTxMint] -> DbAction m [Id.MaTxMintId]
insertBulkMaTxMint maTxMints =
  runDbSessionMain (mkDbCallStack "insertBulkMaTxMint") $
    HsqlSes.statement maTxMints insertBulkMaTxMintStmt

-- | Pool version for parallel operations
parallelInsertBulkMaTxMint :: MonadIO m => [SMA.MaTxMint] -> DbAction m [Id.MaTxMintId]
parallelInsertBulkMaTxMint maTxMints =
  runDbSessionPool (mkDbCallStack "parallelInsertBulkMaTxMint") $
    HsqlSes.statement maTxMints insertBulkMaTxMintStmt
