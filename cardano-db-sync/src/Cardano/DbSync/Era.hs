{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( insertValidateGenesisDist
  ) where

import           Cardano.BM.Data.Trace (Trace)

import           Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import           Cardano.DbSync.Error

import           Control.Monad.Trans.Except (ExceptT)

import           Data.Text (Text)

insertValidateGenesisDist
        :: Trace IO Text -> NetworkName -> GenesisConfig
        -> ExceptT DbSyncNodeError IO ()
insertValidateGenesisDist trce nname genCfg =
  case genCfg of
    GenesisCardano bCfg sCfg -> do
      Byron.insertValidateGenesisDist trce (unNetworkName nname) bCfg
      Shelley.insertValidateGenesisDist trce (unNetworkName nname) (scConfig sCfg)
