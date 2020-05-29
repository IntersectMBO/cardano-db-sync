{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.DbSync.Era
  ( GenesisEra (..)
  , MkConsensusConfig (..)
  , mkByronConsensusConfig
  , readByronGenesisConfig
  ) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (decodeAbstractHash)

import           Cardano.DbSync.Config
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano (Protocol (..), protocolInfo)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)


data GenesisEra
  = ByronGenesis !Byron.Config
  | ShelleyGenesis



readByronGenesisConfig :: DbSyncNodeParams -> DbSyncNodeConfig -> ExceptT DbSyncNodeError IO Byron.Config
readByronGenesisConfig enp enc = do
    genHash <- firstExceptT NEError
                  . hoistEither $ decodeAbstractHash (unGenesisHash $ encGenesisHash enc)
    firstExceptT BEByronConfig
                  $ Byron.mkConfigFromFile (encRequiresNetworkMagic enc)
                      (unGenesisFile $ enpGenesisFile enp) genHash


mkByronConsensusConfig :: Byron.Config -> TopLevelConfig ByronBlock
mkByronConsensusConfig bgc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT bgc Nothing (Byron.ProtocolVersion 0 2 0)
      (Byron.SoftwareVersion (Byron.ApplicationName "cardano-sl") 1) Nothing


class MkConsensusConfig cfg blk where
  mkConsensusConfig :: cfg -> TopLevelConfig blk

instance MkConsensusConfig Byron.Config ByronBlock where
  mkConsensusConfig = mkByronConsensusConfig
