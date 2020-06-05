{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( GenesisEra (..)
  , MkConsensusConfig (..)
  , genesisProtocolMagic
  , insertValidateGenesisDist
  , mkByronTopLevelConfig
  , mkShelleyTopLevelConfig
  , readByronGenesisConfig
  , readGenesisConfig
  , readShelleyGenesisConfig
  ) where

import           Cardano.Binary (Annotated (..))
import           Cardano.BM.Data.Trace (Trace)

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (decodeAbstractHash)
import           Cardano.Crypto.ProtocolMagic (AProtocolMagic (..), ProtocolMagic,
                    RequiresNetworkMagic (..))

import           Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types

import           Cardano.Config.Shelley.Orphans ()

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT, runExceptT)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano (protocolInfoByron)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Node (ProtocolInfo (..), pInfoConfig)
import qualified Ouroboros.Consensus.Shelley.Genesis as Shelley
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..), protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.BaseTypes (Network (..))
import           Shelley.Spec.Ledger.PParams (ProtVer (..))

data GenesisEra
  = GenesisByron !Byron.Config
  | GenesisShelley !(ShelleyGenesis TPraosStandardCrypto)


class ConvertRawHash blk => MkConsensusConfig cfg blk where
  mkConsensusConfig :: cfg -> TopLevelConfig blk

instance MkConsensusConfig Byron.Config ByronBlock where
  mkConsensusConfig = mkByronTopLevelConfig

instance MkConsensusConfig (ShelleyGenesis TPraosStandardCrypto) ShelleyBlock where
  mkConsensusConfig = mkShelleyTopLevelConfig


genesisProtocolMagic :: GenesisEra -> ProtocolMagic
genesisProtocolMagic ge =
  case ge of
    GenesisByron bg -> Byron.configProtocolMagic bg
    GenesisShelley sg -> mkShelleyProtocolMagic sg

insertValidateGenesisDist
        :: Trace IO Text.Text -> NetworkName -> GenesisEra
        -> ExceptT DbSyncNodeError IO ()
insertValidateGenesisDist trce nname genCfg =
  case genCfg of
    GenesisByron bCfg ->
      Byron.insertValidateGenesisDist trce (unNetworkName nname) bCfg
    GenesisShelley sCfg ->
      Shelley.insertValidateGenesisDist trce (unNetworkName nname) sCfg

-- -----------------------------------------------------------------------------

mkByronTopLevelConfig :: Byron.Config -> TopLevelConfig ByronBlock
mkByronTopLevelConfig bgc =
    pInfoConfig byronInfo
  where
    byronInfo :: ProtocolInfo IO ByronBlock
    byronInfo =
      protocolInfoByron bgc Nothing (Byron.ProtocolVersion 0 2 0)
        (Byron.SoftwareVersion (Byron.ApplicationName "cardano-sl") 1) Nothing


mkShelleyTopLevelConfig :: ShelleyGenesis TPraosStandardCrypto -> TopLevelConfig ShelleyBlock
mkShelleyTopLevelConfig sgc =
    pInfoConfig shelleyInfo
  where
    -- Can use Nothing for the last field because that field will be dropped
    -- by 'pInfoConfig' anyway.
    shelleyInfo :: ProtocolInfo IO ShelleyBlock
    shelleyInfo = protocolInfoShelley sgc (ProtVer 0 0) Nothing

mkShelleyProtocolMagic :: ShelleyGenesis TPraosStandardCrypto -> ProtocolMagic
mkShelleyProtocolMagic sg =
  AProtocolMagic
    { getAProtocolMagicId = Annotated (Shelley.sgProtocolMagicId sg) ()
    , getRequiresNetworkMagic =
        if Shelley.sgNetworkId sg == Mainnet
          then RequiresNoMagic
          else RequiresMagic
    }

-- -----------------------------------------------------------------------------

readGenesisConfig
        :: DbSyncNodeParams -> DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisEra
readGenesisConfig enp enc = do
  res <- liftIO $ runExceptT (readByronGenesisConfig enp enc)
  case res of
    Right gb -> pure $ GenesisByron gb
    Left _ -> GenesisShelley <$> readShelleyGenesisConfig enp enc


readByronGenesisConfig
        :: DbSyncNodeParams -> DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO Byron.Config
readByronGenesisConfig enp enc = do
  let file = unGenesisFile $ enpGenesisFile enp
  genHash <- firstExceptT NEError
                . hoistEither $ decodeAbstractHash (unGenesisHash $ encGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Byron.mkConfigFromFile (encRequiresNetworkMagic enc) file genHash

readShelleyGenesisConfig
        :: DbSyncNodeParams -> DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesisConfig enp _enc = do
  let file = unGenesisFile $ enpGenesisFile enp
  firstExceptT (NEShelleyConfig file . Text.pack)
    . newExceptT $ Aeson.eitherDecodeFileStrict' file
