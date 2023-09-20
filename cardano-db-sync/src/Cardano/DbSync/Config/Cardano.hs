{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Binary.Version
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Shelley.Translation (emptyFromByronTranslationContext)
import Control.Monad.Trans.Except (ExceptT)
import Data.Default.Class (Default (def))
import Data.Word (Word64)
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Cardano (Nonce (..), ProtVer (ProtVer))
import qualified Ouroboros.Consensus.Cardano as Consensus
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import qualified Ouroboros.Consensus.Mempool.Capacity as TxLimits
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano
      !SyncNodeConfig
      !Byron.Config
      !ShelleyConfig
      !AlonzoGenesis
      !(ConwayGenesis StandardCrypto)

genesisProtocolMagicId :: GenesisConfig -> ProtocolMagicId
genesisProtocolMagicId ge =
  case ge of
    GenesisCardano _cfg _bCfg sCfg _aCfg _cCfg -> shelleyProtocolMagicId (scConfig sCfg)
  where
    shelleyProtocolMagicId :: ShelleyGenesis StandardCrypto -> ProtocolMagicId
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

cardanoLedgerConfig :: GenesisConfig -> LedgerConfig CardanoBlock
cardanoLedgerConfig = topLevelConfigLedger . mkTopLevelConfig

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
      { paramsByron =
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = bGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = dncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = mkByronSoftwareVersion
            , Consensus.byronLeaderCredentials = Nothing
            , Consensus.byronMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsShelleyBased =
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
            , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , Consensus.shelleyBasedLeaderCredentials = shelleyCred
            }
      , paramsShelley =
          Consensus.ProtocolParamsShelley
            { Consensus.shelleyProtVer = mkProtVer 3 0
            , Consensus.shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsAllegra =
          Consensus.ProtocolParamsAllegra
            { Consensus.allegraProtVer = mkProtVer 4 0
            , Consensus.allegraMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsMary =
          Consensus.ProtocolParamsMary
            { Consensus.maryProtVer = mkProtVer 5 0
            , Consensus.maryMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsAlonzo =
          Consensus.ProtocolParamsAlonzo
            { Consensus.alonzoProtVer = mkProtVer 7 0
            , Consensus.alonzoMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsBabbage =
          Consensus.ProtocolParamsBabbage
            { Consensus.babbageProtVer = mkProtVer 9 0
            , Consensus.babbageMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
      , paramsConway =
          Consensus.ProtocolParamsConway
            { Consensus.conwayProtVer = mkProtVer 10 0
            , Consensus.conwayMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure -- TODO: Conway
            }
      , transitionParamsByronToShelley =
          Consensus.ProtocolTransitionParamsByronToShelley emptyFromByronTranslationContext (dncShelleyHardFork dnc) -- TODO: Conway Fix
      , transitionParamsShelleyToAllegra =
          Consensus.ProtocolTransitionParamsIntraShelley () (dncAllegraHardFork dnc)
      , transitionParamsAllegraToMary =
          Consensus.ProtocolTransitionParamsIntraShelley () (dncMaryHardFork dnc)
      , transitionParamsMaryToAlonzo =
          Consensus.ProtocolTransitionParamsIntraShelley alonzoGenesis (dncAlonzoHardFork dnc)
      , transitionParamsAlonzoToBabbage =
          Consensus.ProtocolTransitionParamsIntraShelley () (dncBabbageHardFork dnc)
      , transitionParamsBabbageToConway =
          Consensus.ProtocolTransitionParamsIntraShelley (ConwayGenesis def) (dncConwayHardFork dnc) -- TODO: Conway Fix
      }
  where
    GenesisCardano
      dnc
      bGenesis
      shelleyGenesis
      alonzoGenesis
      _conwayGenesis = genesisConfig

shelleyPraosNonce :: ShelleyConfig -> Nonce
shelleyPraosNonce sCfg = Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

mkProtVer :: Word64 -> Word64 -> ProtVer
mkProtVer a b =
  case mkVersion64 a of
    Nothing -> error $ "Impossible: Invalid version generated: " <> show a
    Just v -> ProtVer v (fromIntegral b)

mkByronSoftwareVersion :: SoftwareVersion
mkByronSoftwareVersion =
  SoftwareVersion name ver
  where
    name = ApplicationName "cardano-sl"
    ver = 1
