{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.TypesTest (tests) where

import qualified Cardano.DbSync.Gen as Gen
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Shelley.Generic.Tx.Types"
      [("TxOutMultiAssets roundtrip", prop_txOutMultiAssetsRoundTrip)]

prop_txOutMultiAssetsRoundTrip :: Property
prop_txOutMultiAssetsRoundTrip = property $ do
  multiAsset <- forAll Gen.txOutMultiAsset

  tripping multiAsset Aeson.encode Aeson.decode
