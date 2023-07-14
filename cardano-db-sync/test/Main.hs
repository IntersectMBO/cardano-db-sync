module Main where

import qualified Cardano.DbSync.Util.Bech32Test as Bech32
import Cardano.Prelude
import Hedgehog.Main
import Prelude ()

main :: IO ()
main = defaultMain [Bech32.tests]
