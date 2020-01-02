{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Upstream
  ( tests
  ) where

-- Test things that come from upstream packages that have at any stage gone wrong
-- or for which

import           Cardano.Chain.Common (decodeAddressBase58, isRedeemAddress)

import           Hedgehog (Property, (===), discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen


prop_isRedeemAddress :: Property
prop_isRedeemAddress =
  H.withTests 2 . H.property $ do
    addr <- H.forAll $ Gen.element
                        [ "Ae2tdPwUPEYycJ77DaXcWEdM3jaSBg2HgmK9jT1HmBzeLJ94x8mRw33xpBM"
                        , "Ae2tdPwUPEZ5bWquLHSYARmM5qgmCg1cjAPb7C4tVkRNQXN2BoX2Han8xKj"
                        , "Ae2tdPwUPEZMB92JqgxAEWfXJo6Ex7wWLoS7REmh81Ue6GgsNrDNs3MeQKA"
                        , "Ae2tdPwUPEZ43NhMYvw1bkcGnEzdDbm9QTWiRux6Xpy8sorgfSJfazneEsP"
                        ]
    fmap isRedeemAddress (decodeAddressBase58 addr) === Right True

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
