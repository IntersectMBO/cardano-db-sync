{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Config (
  ConfigFile (..),
  GenesisConfig (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  ShelleyConfig (..),
  SocketPath (..),
  SyncCommand (..),
  SyncProtocol (..),
  SyncNodeConfig (..),
  SyncNodeParams (..),
  cardanoLedgerConfig,
  genesisProtocolMagicId,
  readCardanoGenesisConfig,
  readSyncNodeConfig,
  configureLogging,
  plutusMultiAssetWhitelistCheck,
) where

import qualified Cardano.BM.Configuration.Model as Logging
import qualified Cardano.BM.Setup as Logging
import Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as Logging
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv, SyncOptions (..), envOptions)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Node (NodeConfig (..), parseNodeConfig, parseSyncPreConfig, readByteStringFromFile)
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Prelude
import Data.Map (keys)
import System.FilePath (takeDirectory, (</>))

configureLogging :: SyncNodeConfig -> Text -> IO (Trace IO Text)
configureLogging syncNodeConfig loggingName = do
  if not (dncEnableLogging syncNodeConfig)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ dncLoggingConfig syncNodeConfig) loggingName

readSyncNodeConfig :: ConfigFile -> IO SyncNodeConfig
readSyncNodeConfig (ConfigFile fp) = do
  pcfg <- (adjustNodeFilePath . parseSyncPreConfig) =<< readByteStringFromFile fp "DbSync"
  ncfg <- parseNodeConfig =<< readByteStringFromFile (pcNodeConfigFilePath pcfg) "node"
  coalesceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    adjustNodeFilePath :: IO SyncPreConfig -> IO SyncPreConfig
    adjustNodeFilePath spc = do
      cfg <- spc
      pure $ cfg {pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg)}

coalesceConfig ::
  SyncPreConfig ->
  NodeConfig ->
  (FilePath -> FilePath) ->
  IO SyncNodeConfig
coalesceConfig pcfg ncfg adjustGenesisPath = do
  lc <- Logging.setupFromRepresentation $ pcLoggingConfig pcfg
  pure $
    SyncNodeConfig
      { dncNetworkName = pcNetworkName pcfg
      , dncLoggingConfig = lc
      , dncNodeConfigFile = pcNodeConfigFile pcfg
      , dncProtocol = ncProtocol ncfg
      , dncRequiresNetworkMagic = ncRequiresNetworkMagic ncfg
      , dncEnableLogging = pcEnableLogging pcfg
      , dncEnableMetrics = pcEnableMetrics pcfg
      , dncPrometheusPort = pcPrometheusPort pcfg
      , dncPBftSignatureThreshold = ncPBftSignatureThreshold ncfg
      , dncByronGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncByronGenesisFile ncfg)
      , dncByronGenesisHash = ncByronGenesisHash ncfg
      , dncShelleyGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncShelleyGenesisFile ncfg)
      , dncShelleyGenesisHash = ncShelleyGenesisHash ncfg
      , dncAlonzoGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncAlonzoGenesisFile ncfg)
      , dncAlonzoGenesisHash = ncAlonzoGenesisHash ncfg
      , dncConwayGenesisFile =
          adjustGenesisFilePath adjustGenesisPath <$> ncConwayGenesisFile ncfg
      , dncConwayGenesisHash = ncConwayGenesisHash ncfg
      , dncByronProtocolVersion = ncByronProtocolVersion ncfg
      , dncShelleyHardFork = ncShelleyHardFork ncfg
      , dncAllegraHardFork = ncAllegraHardFork ncfg
      , dncMaryHardFork = ncMaryHardFork ncfg
      , dncAlonzoHardFork = ncAlonzoHardFork ncfg
      , dncBabbageHardFork = ncBabbageHardFork ncfg
      , dncConwayHardFork = ncConwayHardFork ncfg
      }

mkAdjustPath :: SyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

-- check both whitelist but also checking plutus Maybes first
plutusMultiAssetWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
plutusMultiAssetWhitelistCheck syncEnv txOuts =
  plutusWhitelistCheck syncEnv txOuts || multiAssetWhitelistCheck syncEnv txOuts

plutusWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
plutusWhitelistCheck syncEnv txOuts = do
  -- first check the config option
  case ioPlutusExtra iopts of
    PlutusEnable -> True
    PlutusDisable -> True
    PlutusWhitelistScripts plutusWhitelist -> plutuswhitelistCheck plutusWhitelist
  where
    iopts = soptInsertOptions $ envOptions syncEnv
    plutuswhitelistCheck :: NonEmpty ByteString -> Bool
    plutuswhitelistCheck whitelist =
      any (\txOut -> isScriptHashWhitelisted whitelist txOut || isAddressWhitelisted whitelist txOut) txOuts
    -- check if the script hash is in the whitelist
    isScriptHashWhitelisted :: NonEmpty ByteString -> Generic.TxOut -> Bool
    isScriptHashWhitelisted whitelist txOut =
      maybe False ((`elem` whitelist) . Generic.txScriptHash) (Generic.txOutScript txOut)
    -- check if the address is in the whitelist
    isAddressWhitelisted :: NonEmpty ByteString -> Generic.TxOut -> Bool
    isAddressWhitelisted whitelist txOut =
      maybe False (`elem` whitelist) (Generic.maybePaymentCred $ Generic.txOutAddress txOut)

multiAssetWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
multiAssetWhitelistCheck syncEnv txOuts = do
  let iopts = soptInsertOptions $ envOptions syncEnv
  case ioMultiAssets iopts of
    MultiAssetEnable -> True
    MultiAssetDisable -> True
    MultiAssetWhitelistPolicies multiAssetWhitelist ->
      or multiAssetwhitelistCheck
      where
        -- txOutMaValue is a Map and we want to check if any of the keys match our whitelist
        multiAssetwhitelistCheck :: [Bool]
        multiAssetwhitelistCheck =
          ( \txout ->
              any (checkMAValueMap multiAssetWhitelist) (keys $ Generic.txOutMaValue txout)
          )
            <$> txOuts

        checkMAValueMap :: NonEmpty ByteString -> PolicyID StandardCrypto -> Bool
        checkMAValueMap maWhitelist policyId =
          Generic.unScriptHash (policyID policyId) `elem` maWhitelist
