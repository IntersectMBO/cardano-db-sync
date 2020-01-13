{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Explorer.Web.Property.Types
  ( tests
  ) where

import           Explorer.Web.ClientTypes (adaToCCoin, cCoinToAda)

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Property.Explorer.DB.Types (genAda)


prop_roundtrip_ada_to_ccoin :: Property
prop_roundtrip_ada_to_ccoin =
  H.withTests 1000 . H.property $ do
    ada <- H.forAll genAda
    H.tripping ada adaToCCoin (Just . cCoinToAda)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
