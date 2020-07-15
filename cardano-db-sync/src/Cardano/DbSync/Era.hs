{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era
  ( GenesisEra (..)
  , genesisEnv
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
import           Cardano.DbSync.Types

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT, runExceptT)

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
  -- | GenesisCardano !Byron.Config !(ShelleyGenesis TPraosStandardCrypto)


genesisEnv :: GenesisEra -> DbSyncEnv
genesisEnv ge =
  case ge of
    GenesisByron _ -> ByronEnv
    GenesisShelley sCfg -> ShelleyEnv $ Shelley.sgNetworkId sCfg

genesisNetworkMagic :: GenesisEra -> NetworkMagic
genesisNetworkMagic ge =
  case ge of
    GenesisByron bg ->
      NetworkMagic $ unProtocolMagicId (Byron.configProtocolMagicId bg)
    GenesisShelley sg -> NetworkMagic $ Shelley.sgNetworkMagic sg

genesisProtocolMagic :: GenesisEra -> ProtocolMagic
genesisProtocolMagic ge =
    case ge of
      GenesisByron bg -> Byron.configProtocolMagic bg
      GenesisShelley sg -> mkShelleyProtocolMagic sg
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
