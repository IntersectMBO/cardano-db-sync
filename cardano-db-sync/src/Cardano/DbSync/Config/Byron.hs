{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Config.Byron (
  readByronGenesisConfig,
) where

import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (decodeAbstractHash)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)

readByronGenesisConfig ::
  SyncNodeConfig ->
  ExceptT SyncNodeError IO Byron.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ dncByronGenesisFile enc
  genHash <-
    firstExceptT SNErrDefault
      . hoistEither
      $ decodeAbstractHash (unGenesisHashByron $ dncByronGenesisHash enc)
  firstExceptT (SNErrByronConfig file) $
    Byron.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash
