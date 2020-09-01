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

import qualified Shelley.Spec.Ledger.Genesis as Shelley

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisEra
  = GenesisCardano !Byron.Config !(ShelleyGenesis TPraosStandardCrypto)


genesisNetworkMagic :: GenesisEra -> NetworkMagic
genesisNetworkMagic ge =
  case ge of
    GenesisCardano _bg sg ->
      NetworkMagic $ Shelley.sgNetworkMagic sg

genesisProtocolMagicId :: GenesisEra -> ProtocolMagicId
genesisProtocolMagicId ge =
    case ge of
      GenesisCardano _ sg -> shelleyProtocolMagicId sg
  where
    shelleyProtocolMagicId :: ShelleyGenesis TPraosStandardCrypto -> ProtocolMagicId
    shelleyProtocolMagicId sg = ProtocolMagicId (Shelley.sgNetworkMagic sg)

insertValidateGenesisDist
        :: Trace IO Text.Text -> NetworkName -> GenesisEra
        -> ExceptT DbSyncNodeError IO ()
insertValidateGenesisDist trce nname genCfg =
  case genCfg of
    GenesisCardano bCfg sCfg -> do
      Byron.insertValidateGenesisDist trce (unNetworkName nname) bCfg
      Shelley.insertValidateGenesisDist trce (unNetworkName nname) sCfg

-- -----------------------------------------------------------------------------

genesisConfigToEnv :: GenesisEra -> Either DbSyncNodeError DbSyncEnv
genesisConfigToEnv genCfg =
    case genCfg of
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

readGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisEra
readGenesisConfig enc =
  case encProtocol enc of
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
