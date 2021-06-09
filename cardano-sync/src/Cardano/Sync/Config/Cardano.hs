{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Sync.Config.Cardano
  ( GenesisConfig (..)
  , cardanoLedgerConfig
  , genesisProtocolMagicId
  , mkTopLevelConfig
  , mkProtocolInfoCardano
  , readCardanoGenesisConfig
  ) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Ledger.Alonzo.Genesis

import           Cardano.Sync.Config.Alonzo
import           Cardano.Sync.Config.Byron
import           Cardano.Sync.Config.Shelley
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Error

import           Control.Monad.Trans.Except (ExceptT)

import           Ouroboros.Consensus.Cardano (Nonce (..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

import qualified Shelley.Spec.Ledger.PParams as Shelley


-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !SyncNodeConfig !Byron.Config !ShelleyConfig !AlonzoGenesis

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisCardano _cfg _bCfg sCfg _aCfg -> shelleyProtocolMagicId (scConfig sCfg)
  where
    shelleyProtocolMagicId :: ShelleyGenesis StandardShelley -> ProtocolMagicId
    shelleyProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readCardanoGenesisConfig
        :: SyncNodeConfig
        -> ExceptT SyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case dncProtocol enc of
    SyncProtocolCardano ->
      GenesisCardano enc <$> readByronGenesisConfig enc
                         <*> readShelleyGenesisConfig enc
                         <*> readAlonzoGenesisConfig enc

-- -------------------------------------------------------------------------------------------------

cardanoLedgerConfig :: GenesisConfig -> LedgerConfig CardanoBlock
cardanoLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig CardanoBlock
mkTopLevelConfig = Consensus.pInfoConfig . mkProtocolInfoCardano

-- Need a concrete type for 'm' ('IO') to make the type checker happy.
-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol
mkProtocolInfoCardano :: GenesisConfig -> ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano ge =
  case ge of
    GenesisCardano dnc byronGenesis shelleyGenesis alonzoGenesis ->
        Consensus.protocolInfoCardano
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
          Consensus.ProtocolParamsAlonzo
            { Consensus.alonzoProtVer = shelleyProtVer dnc
            }
          (Consensus.ProtocolTransitionParamsShelleyBased () $ dncShelleyHardFork dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased () $ dncAllegraHardFork dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased () $ dncMaryHardFork dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis $ dncAlonzoHardFork dnc)

shelleyPraosNonce :: ShelleyConfig -> Nonce
shelleyPraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: SyncNodeConfig -> Shelley.ProtVer
shelleyProtVer dnc =
  let bver = dncByronProtocolVersion dnc in
  Shelley.ProtVer (fromIntegral $ Byron.pvMajor bver) (fromIntegral $ Byron.pvMinor bver)

