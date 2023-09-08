{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Config.CardanoTest (tests) where

import Cardano.DbSync.Config.Conway
import Cardano.DbSync.Config.Types
import Cardano.Ledger.Conway.Genesis
import Cardano.Prelude
import Data.Default.Class (Default (..))
import Hedgehog
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Config.Cardano"
      [("Parsed ConwayGenesis = ConwayGenesis def", prop_conwayGenesis_eq)]

prop_conwayGenesis_eq :: Property
prop_conwayGenesis_eq = withTests 1 . property $ do
  genesis <- readGenesis' (GenesisFile genesisFilePath) Nothing
  ConwayGenesis def === genesis
  where
    readGenesis' file hash =
      evalEither =<< liftIO (runExceptT $ readGenesis file hash)

genesisFilePath :: FilePath
genesisFilePath = "../cardano-chain-gen/test/testfiles/config-conway/genesis.conway.json"
