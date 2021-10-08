{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( module X
  , insertValidateGenesisDist
  ) where

import           Cardano.Prelude

import           Cardano.BM.Data.Trace (Trace)

import           Cardano.DbSync.Config
import           Cardano.DbSync.Error

import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import           Cardano.DbSync.Era.Shelley.Offline as X

import           Database.Persist.Sql (SqlBackend)

insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> NetworkName -> GenesisConfig -> Bool
    -> ExceptT SyncNodeError IO ()
insertValidateGenesisDist backend trce nname genCfg shelleyInitiation =
  case genCfg of
    GenesisCardano _ bCfg sCfg _aCfg -> do
      Byron.insertValidateGenesisDist backend trce (unNetworkName nname) bCfg
      Shelley.insertValidateGenesisDist backend trce (unNetworkName nname) (scConfig sCfg) shelleyInitiation
