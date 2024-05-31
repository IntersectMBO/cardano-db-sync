{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era (
  insertValidateGenesisDist,
) where

import Cardano.DbSync.AppT (App)
import Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley

insertValidateGenesisDist ::
  NetworkName ->
  GenesisConfig ->
  Bool ->
  App ()
insertValidateGenesisDist nname genCfg shelleyInitiation =
  case genCfg of
    GenesisCardano _ bCfg sCfg _aCfg _ -> do
      Byron.insertValidateGenesisDist nname bCfg
      Shelley.insertValidateGenesisDist (unNetworkName nname) (scConfig sCfg) shelleyInitiation
