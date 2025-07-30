{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Config.Cardano (
  GenesisConfig (..),
  genesisProtocolMagicId,
  mkTopLevelConfig,
  mkProtocolInfoCardano,
  readCardanoGenesisConfig,
) where

import qualified Cardano.Chain.Genesis as Byron
import Cardano.Chain.Update (ApplicationName (..), SoftwareVersion (..))
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Cardano.DbSync.Config.Alonzo
import Cardano.DbSync.Config.Byron
import Cardano.DbSync.Config.Conway (readConwayGenesisConfig)
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import qualified Cardano.Ledger.Api.Transition as Ledger
import Cardano.Ledger.Binary.Version
import Cardano.Ledger.Conway.Genesis
import Control.Monad.Trans.Except (ExceptT)
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Cardano (Nonce (..), ProtVer (ProtVer))
import qualified Ouroboros.Consensus.Cardano as Consensus
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config (TopLevelConfig (..), emptyCheckpointsMap)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano
      !SyncNodeConfig
      !Byron.Config
      !ShelleyConfig
      !AlonzoGenesis
      !ConwayGenesis

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
  case ge of
    GenesisCardano _cfg _bCfg sCfg _aCfg _cCfg -> shelleyProtocolMagicId (scConfig sCfg)
  where
    shelleyProtocolMagicId :: ShelleyGenesis -> ProtocolMagicId
    shelleyProtocolMagicId sCfg = ProtocolMagicId (sgNetworkMagic sCfg)

readCardanoGenesisConfig ::
  SyncNodeConfig ->
  ExceptT SyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case dncProtocol enc of
    SyncProtocolCardano ->
      GenesisCardano enc
        <$> readByronGenesisConfig enc
        <*> readShelleyGenesisConfig enc
        <*> readAlonzoGenesisConfig enc
        <*> readConwayGenesisConfig enc

-- -------------------------------------------------------------------------------------------------

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig CardanoBlock
mkTopLevelConfig cfg = Consensus.pInfoConfig $ fst $ mkProtocolInfoCardano cfg []

-- Need a concrete type for 'm' ('IO') to make the type checker happy.

-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol
mkProtocolInfoCardano ::
  GenesisConfig ->
  [Consensus.ShelleyLeaderCredentials StandardCrypto] -> -- this is not empty only in tests
  (ProtocolInfo CardanoBlock, IO [BlockForging IO CardanoBlock])
mkProtocolInfoCardano genesisConfig shelleyCred =
  protocolInfoCardano $
    CardanoProtocolParams
      { byronProtocolParams =
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = bGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = dncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = mkByronSoftwareVersion
            , Consensus.byronLeaderCredentials = Nothing
            }
      , shelleyBasedProtocolParams =
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedInitialNonce = shelleyPraosNonce genesisHash
            , Consensus.shelleyBasedLeaderCredentials = shelleyCred
            }
      , cardanoProtocolVersion = ProtVer (natVersion @10) 0
      , cardanoLedgerTransitionConfig =
          Ledger.mkLatestTransitionConfig
            shelleyGenesis
            alonzoGenesis
            conwayGenesis
      , cardanoHardForkTriggers =
          Consensus.CardanoHardForkTriggers'
            { triggerHardForkShelley = dncShelleyHardFork dnc
            , triggerHardForkAllegra = dncAllegraHardFork dnc
            , triggerHardForkMary = dncMaryHardFork dnc
            , triggerHardForkAlonzo = dncAlonzoHardFork dnc
            , triggerHardForkBabbage = dncBabbageHardFork dnc
            , triggerHardForkConway = dncConwayHardFork dnc
            }
      , cardanoCheckpoints = emptyCheckpointsMap
      }
  where
    GenesisCardano
      dnc
      bGenesis
      (ShelleyConfig shelleyGenesis genesisHash)
      alonzoGenesis
      conwayGenesis = genesisConfig

shelleyPraosNonce :: GenesisHashShelley -> Nonce
shelleyPraosNonce hsh = Nonce (Crypto.castHash . unGenesisHashShelley $ hsh)

mkByronSoftwareVersion :: SoftwareVersion
mkByronSoftwareVersion =
  SoftwareVersion name ver
  where
    name = ApplicationName "cardano-sl"
    ver = 1
