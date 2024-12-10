{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.TypesTest (tests) where

import Cardano.DbSync.Era.Shelley.Generic (TxOutMultiAsset (..), fromMultiAssetMap)
import qualified Cardano.DbSync.Gen as Gen
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Shelley.Generic.Tx.Types"
      [ ("TxOutMultiAssets roundtrip", prop_txOutMultiAssetsRoundTrip)
      , ("fromMultiAssetMapCount", prop_fromMultiAssetMapCount)
      , ("fromMultiAssetMap roundtrip", prop_fromMultiAssetRoundTrip)
      ]

prop_txOutMultiAssetsRoundTrip :: Property
prop_txOutMultiAssetsRoundTrip = property $ do
  multiAsset <- forAll Gen.txOutMultiAsset

  tripping multiAsset Aeson.encode Aeson.decode

-- Length of multi-assets should match the nested lengths of the multi-asset map
prop_fromMultiAssetMapCount :: Property
prop_fromMultiAssetMapCount = property $ do
  (MultiAsset multiAssets) <- forAll Gen.multiAsset

  sum (fmap length multiAssets) === length (fromMultiAssetMap multiAssets)

-- Should be able to construct multi-asset map from multi-assets
prop_fromMultiAssetRoundTrip :: Property
prop_fromMultiAssetRoundTrip = property $ do
  (MultiAsset multiAssets) <- forAll Gen.multiAsset
  tripping multiAssets fromMultiAssetMap (Just . toMultiAssetMap)
  where
    toMultiAssetMap =
      foldr
        ( \(TxOutMultiAsset pol asset amt) xs ->
            Map.insertWith Map.union pol (Map.singleton asset amt) xs
        )
        Map.empty
