{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era (
  module X,
  insertValidateGenesisDist,
) where

import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import Cardano.DbSync.Era.Shelley.Offline as X
import Cardano.DbSync.Error
import Cardano.Prelude

insertValidateGenesisDist ::
  SyncEnv ->
  NetworkName ->
  GenesisConfig ->
  Bool ->
  ExceptT SyncNodeError IO ()
insertValidateGenesisDist syncEnv nname genCfg shelleyInitiation =
  case genCfg of
    GenesisCardano _ bCfg sCfg _aCfg _ -> do
      Byron.insertValidateGenesisDist syncEnv nname bCfg
      Shelley.insertValidateGenesisDist syncEnv (unNetworkName nname) (scConfig sCfg) shelleyInitiation
