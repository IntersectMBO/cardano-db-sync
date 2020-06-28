{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Insert
  ( insertCardanoBlock
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Plugin.Default.Byron.Insert as Byron
import qualified Cardano.DbSync.Plugin.Default.Shelley.Insert as Shelley
import           Cardano.DbSync.Types


insertCardanoBlock
    :: Trace IO Text -> DbSyncEnv -> CardanoBlockTip
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertCardanoBlock tracer env blkTip = do
  case blkTip of
    ByronBlockTip blk tip ->
      Byron.insertByronBlock tracer blk tip
    ShelleyBlockTip blk tip ->
      Shelley.insertShelleyBlock tracer env blk tip
