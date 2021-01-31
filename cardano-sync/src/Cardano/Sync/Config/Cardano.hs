{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Sync.Config.Cardano
  ( GenesisConfig (..)
  , cardanoLedgerConfig
  , genesisConfigToEnv
  , genesisProtocolMagicId
  , mkTopLevelConfig
  , mkProtocolInfoCardano
  , readCardanoGenesisConfig
  ) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Sync.Config.Byron
import           Cardano.Sync.Config.Shelley
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Error
import           Cardano.Sync.Util

import           Control.Monad.Trans.Except (ExceptT)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Cardano (Nonce (..), Protocol (..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.PParams as Shelley


-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !DbSyncNodeConfig !Byron.Config !ShelleyConfig

genesisConfigToEnv :: DbSyncNodeParams -> GenesisConfig -> Either DbSyncNodeError DbSyncEnv
genesisConfigToEnv enp genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg
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
                  , envLedgerStateDir = enpLedgerStateDir enp
                  }

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisCardano _cfg _bCfg sCfg -> shelleyProtocolMagicId (scConfig sCfg)
  where
    shelleyProtocolMagicId :: ShelleyGenesis StandardShelley -> ProtocolMagicId
    shelleyProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readCardanoGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case dncProtocol enc of
    DbSyncProtocolCardano ->
      GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

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
    GenesisCardano dnc byronGenesis shelleyGenesis ->
        Consensus.ProtocolCardano
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = dncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = dncByronSoftwareVersion dnc
            , Consensus.byronLeaderCredentials = Nothing
            }
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
            , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , Consensus.shelleyBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsShelley
            { Consensus.shelleyProtVer = shelleyProtVer dnc
            }
          Consensus.ProtocolParamsAllegra
            { Consensus.allegraProtVer = shelleyProtVer dnc
            }
          Consensus.ProtocolParamsMary
            { Consensus.maryProtVer = shelleyProtVer dnc
            }
          (dncByronToShelley dnc)
          (dncShelleyToAllegra dnc)
          (dncAllegraToMary dnc)


shelleyPraosNonce :: ShelleyConfig -> Nonce
shelleyPraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: DbSyncNodeConfig -> Shelley.ProtVer
shelleyProtVer dnc =
  let bver = dncByronProtocolVersion dnc in
  Shelley.ProtVer (fromIntegral $ Byron.pvMajor bver) (fromIntegral $ Byron.pvMinor bver)

