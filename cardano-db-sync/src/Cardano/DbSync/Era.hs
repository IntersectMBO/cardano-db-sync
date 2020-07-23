{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( GenesisEra (..)
  , genesisConfigToEnv
  , genesisNetworkMagic
  , genesisProtocolMagicId
  , insertValidateGenesisDist
  , readByronGenesisConfig
  , readGenesisConfig
  , readShelleyGenesisConfig
  ) where

import           Cardano.BM.Data.Trace (Trace)

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto (decodeAbstractHash)
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))

import           Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley

data GenesisEra
  = GenesisByron !Byron.Config
  | GenesisShelley !(ShelleyGenesis TPraosStandardCrypto)
  | GenesisCardano !Byron.Config !(ShelleyGenesis TPraosStandardCrypto)


genesisNetworkMagic :: GenesisEra -> NetworkMagic
genesisNetworkMagic ge =
  case ge of
    GenesisByron bg ->
      NetworkMagic $ unProtocolMagicId (Byron.configProtocolMagicId bg)
    GenesisShelley sg ->
      NetworkMagic $ Shelley.sgNetworkMagic sg
    GenesisCardano _bg sg ->
      NetworkMagic $ Shelley.sgNetworkMagic sg

genesisProtocolMagicId :: GenesisEra -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisByron bg -> Byron.configProtocolMagicId bg
      GenesisShelley sg -> shelleyProtocolMagicId sg
      GenesisCardano _ sg -> shelleyProtocolMagicId sg
  where
    shelleyProtocolMagicId :: ShelleyGenesis TPraosStandardCrypto -> ProtocolMagicId
    shelleyProtocolMagicId sg = ProtocolMagicId (Shelley.sgNetworkMagic sg)

insertValidateGenesisDist
        :: Trace IO Text.Text -> NetworkName -> GenesisEra
        -> ExceptT DbSyncNodeError IO ()
insertValidateGenesisDist trce nname genCfg =
  case genCfg of
    GenesisByron bCfg ->
      Byron.insertValidateGenesisDist trce (unNetworkName nname) bCfg
    GenesisShelley sCfg ->
      Shelley.insertValidateGenesisDist trce (unNetworkName nname) sCfg
    GenesisCardano bCfg sCfg -> do
      Byron.insertValidateGenesisDist trce (unNetworkName nname) bCfg
      Shelley.insertValidateGenesisDist trce (unNetworkName nname) sCfg

-- -----------------------------------------------------------------------------

genesisConfigToEnv :: GenesisEra -> Either DbSyncNodeError DbSyncEnv
genesisConfigToEnv genCfg =
    case genCfg of
      GenesisByron bCfg ->
        Right $ DbSyncEnv
                  { envProtocol = DbSyncProtocolByron
                  , envNetwork = byronProtocolMagicIdToShelleyNetwok (Byron.configProtocolMagicId bCfg)
                  , envNetworkMagic = NetworkMagic $ (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                  , envSystemStart = SystemStart (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                  }
      GenesisShelley sCfg ->
        Right $ DbSyncEnv
                  { envProtocol = DbSyncProtocolShelley
                  , envNetwork = Shelley.sgNetworkId sCfg
                  , envNetworkMagic = NetworkMagic (Shelley.sgNetworkMagic sCfg)
                  , envSystemStart = SystemStart $ Shelley.sgSystemStart sCfg
                  }
      GenesisCardano bCfg sCfg
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic sCfg ->
            Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagcId ", textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.sgNetworkMagic sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart sCfg ->
            Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", textShow (Shelley.sgSystemStart sCfg)
                ]
        | otherwise ->
            Right $ DbSyncEnv
                  { envProtocol = DbSyncProtocolCardano
                  , envNetwork = Shelley.sgNetworkId sCfg
                  , envNetworkMagic = NetworkMagic (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                  , envSystemStart = SystemStart (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                  }
  where
    byronProtocolMagicIdToShelleyNetwok :: ProtocolMagicId -> Shelley.Network
    byronProtocolMagicIdToShelleyNetwok pmid =
      if pmid == Byron.mainnetProtocolMagicId
        then Shelley.Mainnet
        else Shelley.Testnet

readGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisEra
readGenesisConfig enc =
  case encProtocol enc of
    DbSyncProtocolByron ->
      GenesisByron <$> readByronGenesisConfig enc
    DbSyncProtocolShelley ->
      GenesisShelley <$> readShelleyGenesisConfig enc
    DbSyncProtocolCardano ->
      GenesisCardano <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

readByronGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO Byron.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ encByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither $ decodeAbstractHash (unGenesisHash $ encByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Byron.mkConfigFromFile (encRequiresNetworkMagic enc) file genHash

readShelleyGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ encShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . Text.pack)
    . newExceptT $ Aeson.eitherDecodeFileStrict' file
