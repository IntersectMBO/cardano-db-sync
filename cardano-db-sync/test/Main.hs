module Main where

import qualified Cardano.DbSync.CardanoUtilTest as CardanoUtils
import Cardano.Prelude
import Hedgehog.Main
import Prelude ()

main :: IO ()
main = defaultMain [CardanoUtils.tests]
