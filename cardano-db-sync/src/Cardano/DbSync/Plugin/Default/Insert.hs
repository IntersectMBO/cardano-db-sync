{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Insert
  ( insertDefaultBlock
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


insertDefaultBlock
    :: Trace IO Text -> DbSyncEnv -> BlockDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertDefaultBlock tracer env blkTip = do
  case blkTip of
    ByronBlockDetails blk details ->
      Byron.insertByronBlock tracer blk details
    ShelleyBlockDetails blk details ->
      Shelley.insertShelleyBlock tracer env blk details
