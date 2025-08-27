{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.MultiAsset where

import Cardano.Prelude (ByteString, for)
import Data.Functor.Contravariant (Contravariant (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Schema.Core.MultiAsset (MaTxMint)
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), runSession)
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Types (DbInt65, DbM)

--------------------------------------------------------------------------------
-- MultiAsset
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertMultiAssetStmt :: HsqlStmt.Statement SMA.MultiAsset Id.MultiAssetId
insertMultiAssetStmt =
  insert
    SMA.multiAssetEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.MultiAssetId)

insertMultiAsset :: SMA.MultiAsset -> DbM Id.MultiAssetId
insertMultiAsset multiAsset =
  runSession $ HsqlSes.statement multiAsset insertMultiAssetStmt

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

queryMultiAssetId :: ByteString -> ByteString -> DbM (Maybe Id.MultiAssetId)
queryMultiAssetId policy assetName =
  runSession $
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

insertBulkMaTxMintPiped :: [[SMA.MaTxMint]] -> DbM [Id.MaTxMintId]
insertBulkMaTxMintPiped maTxMintChunks =
  concat
    <$> runSession
      ( HsqlSes.pipeline $
          for maTxMintChunks $ \chunk ->
            HsqlP.statement chunk insertBulkMaTxMintStmt
      )
