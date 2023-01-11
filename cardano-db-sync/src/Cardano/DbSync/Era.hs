{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era (
  module X,
  insertValidateGenesisDist,
) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import Cardano.DbSync.Era.Shelley.Offline as X
import Cardano.DbSync.Error
import Cardano.Prelude
import Database.Persist.Sql (SqlBackend)

insertValidateGenesisDist ::
  Trace IO Text ->
  SqlBackend ->
  NetworkName ->
  GenesisConfig ->
  Bool ->
  ExceptT SyncNodeError IO ()
insertValidateGenesisDist trce backend nname genCfg shelleyInitiation =
  case genCfg of
    GenesisCardano _ bCfg sCfg _aCfg -> do
      Byron.insertValidateGenesisDist backend trce nname bCfg
      Shelley.insertValidateGenesisDist backend trce (unNetworkName nname) (scConfig sCfg) shelleyInitiation
