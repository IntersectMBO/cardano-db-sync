{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( GenesisEra (..)
  , genesisNetworkMagic
  , genesisProtocolMagic
  , insertValidateGenesisDist
  , readByronGenesisConfig
  , readGenesisConfig
  , readShelleyGenesisConfig
  ) where

import           Cardano.Binary (Annotated (..))
import           Cardano.BM.Data.Trace (Trace)

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto (decodeAbstractHash)
import           Cardano.Crypto.ProtocolMagic (AProtocolMagic (..), ProtocolMagic,
                    ProtocolMagicId (..), RequiresNetworkMagic (..))

import           Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Genesis as Byron
import qualified Cardano.DbSync.Era.Shelley.Genesis as Shelley
import           Cardano.DbSync.Error

import           Cardano.Config.Shelley.Orphans ()

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Shelley.Spec.Ledger.BaseTypes (Network (..))
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

genesisProtocolMagic :: GenesisEra -> ProtocolMagic
genesisProtocolMagic ge =
    case ge of
      GenesisByron bg -> Byron.configProtocolMagic bg
      GenesisShelley sg -> mkShelleyProtocolMagic sg
      GenesisCardano _ sg -> mkShelleyProtocolMagic sg
  where
    mkShelleyProtocolMagic :: ShelleyGenesis TPraosStandardCrypto -> ProtocolMagic
    mkShelleyProtocolMagic sg =
      AProtocolMagic
        { getAProtocolMagicId = Annotated (Shelley.sgProtocolMagicId sg) ()
        , getRequiresNetworkMagic =
            if Shelley.sgNetworkId sg == Mainnet
              then RequiresNoMagic
              else RequiresMagic
        }

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
