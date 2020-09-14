{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Config.Cardano
  ( GenesisConfig (..)
  , cardanoLedgerConfig
  , genesisConfigToEnv
  , genesisProtocolMagicId
  , mkTopLevelConfig
  , readCardanoGenesisConfig
  ) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.DbSync.Config.Byron
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Control.Monad.Trans.Except (ExceptT)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Cardano (Nonce (..), Protocol (..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano.CanHardFork (TriggerHardFork (..))
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.PParams as Shelley


-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !Byron.Config !ShelleyConfig

genesisConfigToEnv :: GenesisConfig -> Either DbSyncNodeError DbSyncEnv
genesisConfigToEnv genCfg =
    case genCfg of
      GenesisCardano bCfg sCfg
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= sgNetworkMagic (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", textShow (sgNetworkMagic $ scConfig sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= sgSystemStart (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", textShow (sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            Right $ DbSyncEnv
                  { envProtocol = DbSyncProtocolCardano
                  , envNetwork = sgNetworkId (scConfig sCfg)
                  , envNetworkMagic = NetworkMagic (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                  , envSystemStart = SystemStart (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                  }

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisCardano _bCfg sCfg -> shelleyProtocolMagicId (scConfig sCfg)
  where
    shelleyProtocolMagicId :: ShelleyGenesis StandardShelley -> ProtocolMagicId
    shelleyProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readCardanoGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case encProtocol enc of
    DbSyncProtocolCardano ->
      GenesisCardano <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

-- -------------------------------------------------------------------------------------------------

cardanoLedgerConfig :: GenesisConfig -> LedgerConfig CardanoBlock
cardanoLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig CardanoBlock
mkTopLevelConfig = Consensus.pInfoConfig . mkProtocolInfoCardano

-- Need a concrete type for 'm' ('IO') to make the type checker happy.
mkProtocolInfoCardano :: GenesisConfig -> ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano = Consensus.protocolInfo . mkProtocolCardano

-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol

mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol
mkProtocolCardano ge =
    case ge of
      GenesisCardano byronGenesis shelleyGenesis ->
        ProtocolCardano
          -- Byron parameters
          byronGenesis
          Nothing                                       -- Maybe PBftSignatureThreshold
          (Byron.ProtocolVersion 2 0 0)                 -- Update.ProtocolVersion
          (Byron.SoftwareVersion                        -- Update.SoftwareVersion
            (Byron.ApplicationName "cardano-sl")
            1
            )
          Nothing                                       -- Maybe ByronLeaderCredentials

          -- Shelley parameters
          (scConfig shelleyGenesis)
          (shelleyPraosNonce shelleyGenesis)
          (Shelley.ProtVer 2 0)
          (Consensus.MaxMajorProtVer 1)
          Nothing                                       -- Maybe (TPraosLeaderCredentials StandardShelley)

          -- Hard fork parameters
          (Just 180)                                    -- Maybe lower bound on first Shelley epoch
          (TriggerHardForkAtEpoch 2)                    -- TriggerHardFork default

  where
    shelleyPraosNonce :: ShelleyConfig -> Nonce
    shelleyPraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)
