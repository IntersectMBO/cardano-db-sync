module Main where

import qualified Cardano.DbSync.Era.Shelley.Generic.ScriptDataTest as ScriptData
import qualified Cardano.DbSync.Era.Shelley.Generic.ScriptTest as Script
import qualified Cardano.DbSync.Util.AddressTest as Address
import qualified Cardano.DbSync.Util.Bech32Test as Bech32
import qualified Cardano.DbSync.Util.CborTest as Cbor
import Cardano.Prelude
import Hedgehog.Main
import Prelude ()

main :: IO ()
main =
  defaultMain
    [ Bech32.tests
    , Address.tests
    , Cbor.tests
    , Script.tests
    , ScriptData.tests
    ]
