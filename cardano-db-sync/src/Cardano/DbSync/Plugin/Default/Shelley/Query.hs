{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Plugin.Default.Shelley.Query
  ( queryStakePoolKeyHash
  ) where


import           Cardano.Db
import           Cardano.DbSync.Types
import           Cardano.DbSync.Era.Shelley.Util (unKeyHashBS)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (listToMaybe)

import           Database.Esqueleto (Value (..),  (^.), (==.), from, select, val, where_)
import           Database.Persist.Sql (SqlBackend)


queryStakePoolKeyHash :: MonadIO m => ShelleyStakePoolKeyHash -> ReaderT SqlBackend m (Either LookupFail PoolId)
queryStakePoolKeyHash kh = do
  res <- select . from $ \ pool -> do
            where_ (pool ^. PoolHash ==. val (unKeyHashBS kh))
            pure (pool ^. PoolId)
  pure $ maybeToEither (DbLookupMessage "StakePoolKeyHash") unValue (listToMaybe res)

