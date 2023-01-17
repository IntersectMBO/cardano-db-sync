{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Config.Cardano (
  GenesisConfig (..),
  cardanoLedgerConfig,
  genesisProtocolMagicId,
  mkTopLevelConfig,
  mkProtocolInfoCardano,
  readCardanoGenesisConfig,
) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Cardano.DbSync.Config.Alonzo
import Cardano.DbSync.Config.Byron
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import Control.Monad.Trans.Except (ExceptT)
import Ouroboros.Consensus.Cardano (Nonce (..), ProtVer (ProtVer))
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto, StandardShelley)
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Consensus

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

-- -------------------------------------------------------------------------------------------------

cardanoLedgerConfig :: GenesisConfig -> LedgerConfig CardanoBlock
cardanoLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

mkTopLevelConfig :: GenesisConfig -> TopLevelConfig CardanoBlock
mkTopLevelConfig cfg = Consensus.pInfoConfig $ mkProtocolInfoCardano cfg []

-- Need a concrete type for 'm' ('IO') to make the type checker happy.

-- | The vast majority of the following struct fields are *COMPLETELY IRRELEVANT* to the
-- operation of db-sync, but I have no idea at all what happens of any of these are
-- wrong. This really needs to be a done a different way.
-- mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol
mkProtocolInfoCardano ::
  GenesisConfig ->
  [Consensus.ShelleyLeaderCredentials StandardCrypto] -> -- this is not empty only in tests
  ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano ge shelleyCred =
  case ge of
    GenesisCardano dnc byronGenesis shelleyGenesis alonzoGenesis ->
      Consensus.protocolInfoCardano
        Consensus.ProtocolParamsByron
          { Consensus.byronGenesis = byronGenesis
          , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
          , Consensus.byronProtocolVersion = dncByronProtocolVersion dnc
          , Consensus.byronSoftwareVersion = dncByronSoftwareVersion dnc
          , Consensus.byronLeaderCredentials = Nothing
          , Consensus.byronMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        Consensus.ProtocolParamsShelleyBased
          { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
          , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
          , Consensus.shelleyBasedLeaderCredentials = shelleyCred
          }
        Consensus.ProtocolParamsShelley
          { Consensus.shelleyProtVer = ProtVer 3 0
          , Consensus.shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        Consensus.ProtocolParamsAllegra
          { Consensus.allegraProtVer = ProtVer 4 0
          , Consensus.allegraMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        Consensus.ProtocolParamsMary
          { Consensus.maryProtVer = ProtVer 5 0
          , Consensus.maryMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        Consensus.ProtocolParamsAlonzo
          { Consensus.alonzoProtVer = ProtVer 6 0
          , Consensus.alonzoMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        Consensus.ProtocolParamsBabbage
          { Consensus.babbageProtVer = ProtVer 7 0
          , Consensus.babbageMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        (Consensus.ProtocolTransitionParamsShelleyBased () $ dncShelleyHardFork dnc)
        (Consensus.ProtocolTransitionParamsShelleyBased () $ dncAllegraHardFork dnc)
        (Consensus.ProtocolTransitionParamsShelleyBased () $ dncMaryHardFork dnc)
        (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis $ dncAlonzoHardFork dnc)
        (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis $ dncBabbageHardFork dnc)

shelleyPraosNonce :: ShelleyConfig -> Nonce
shelleyPraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)
