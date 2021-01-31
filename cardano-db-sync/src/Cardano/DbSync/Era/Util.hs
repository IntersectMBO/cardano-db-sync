{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.DbSync.Era.Util
    ( liftLookupFail
    ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Cardano.Db as DB

import           Cardano.Sync.Error

liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT DbSyncNodeError m a
liftLookupFail loc =
  firstExceptT (\lf -> NEError $ loc <> DB.renderLookupFail lf) . newExceptT

