{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era (
  insertValidateGenesisDist,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import Cardano.DbSync.Error
import Cardano.Prelude

insertValidateGenesisDist ::
  SyncEnv ->
  DB.DbEnv ->
  NetworkName ->
  GenesisConfig ->
  Bool ->
  ExceptT SyncNodeError IO ()
insertValidateGenesisDist syncEnv dbEnv nname genCfg shelleyInitiation =
  case genCfg of
    GenesisCardano _ bCfg sCfg _aCfg _ _ -> do
      Byron.insertValidateByronGenesisDist syncEnv dbEnv nname bCfg
      Shelley.insertValidateShelleyGenesisDist syncEnv dbEnv (unNetworkName nname) (scConfig sCfg) shelleyInitiation
