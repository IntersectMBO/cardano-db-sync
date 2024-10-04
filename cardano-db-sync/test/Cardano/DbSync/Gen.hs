{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Gen (
  -- * Config/Api Type generators
  syncNodeParams,
  syncPreConfig,
  syncNodeConfig,
  syncInsertConfig,
  syncInsertOptions,
  shelleyConfig,
  multiAssetConfig,
  metadataConfig,
  plutusConfig,
  multiAssetPolicy,
  metadataKey,
  scriptHash,
  addr,
  networkName,

  -- * Utility generators
  hashBlake2b_256,
  filePath,
  protocolVersion,
  triggerHardFork,
) where

import qualified Cardano.BM.Configuration.Model as Logging (Configuration ())
import qualified Cardano.BM.Data.Configuration as Logging
import qualified Cardano.BM.Data.Severity as Logging
import Cardano.Chain.Update (ProtocolVersion (..))
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Crypto.Hash (Blake2b_256 (), Hash ())
import Cardano.Crypto.Hash.Class (HashAlgorithm (..), hashFromBytes)
import Cardano.Db (PGPassSource (..))
import Cardano.DbSync
import Cardano.DbSync.Config.Types
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Prelude
import Data.ByteString.Short (ShortByteString (), toShort)
import Data.Maybe (fromJust)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Ouroboros.Consensus.Cardano.CanHardFork (TriggerHardFork (..))

syncPreConfig :: Gen SyncPreConfig
syncPreConfig =
  SyncPreConfig
    <$> networkName
    <*> pure loggingRepresentation
    <*> (NodeConfigFile <$> filePath)
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.int (Range.linear 0 10000)
    <*> syncInsertConfig
    <*> Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 100) Gen.unicode)

syncNodeParams :: MonadGen m => m SyncNodeParams
syncNodeParams =
  SyncNodeParams
    <$> (ConfigFile <$> filePath)
    <*> (SocketPath <$> filePath)
    <*> Gen.maybe (LedgerStateDir <$> filePath)
    <*> (MigrationDir <$> filePath)
    <*> Gen.constant PGPassDefaultEnv
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.word64 (Range.linear 0 1000)
    <*> Gen.word64 (Range.linear 0 1000)
    <*> pure Nothing

syncNodeConfig :: Logging.Configuration -> Gen SyncNodeConfig
syncNodeConfig loggingCfg =
  SyncNodeConfig
    <$> networkName
    <*> pure loggingCfg
    <*> (NodeConfigFile <$> filePath)
    <*> Gen.constant SyncProtocolCardano
    <*> Gen.element [RequiresNoMagic, RequiresMagic]
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.int (Range.linear 0 10000)
    <*> Gen.maybe (Gen.double (Range.linearFrac 0 1))
    <*> (GenesisFile <$> filePath)
    <*> (GenesisHashByron <$> Gen.text (Range.linear 0 100) Gen.alphaNum)
    <*> (GenesisFile <$> filePath)
    <*> (GenesisHashShelley . fromJust <$> hashBlake2b_256)
    <*> (GenesisFile <$> filePath)
    <*> (GenesisHashAlonzo . fromJust <$> hashBlake2b_256)
    <*> Gen.maybe (GenesisFile <$> filePath)
    <*> Gen.maybe (GenesisHashConway . fromJust <$> hashBlake2b_256)
    <*> protocolVersion
    <*> triggerHardFork
    <*> triggerHardFork
    <*> triggerHardFork
    <*> triggerHardFork
    <*> triggerHardFork
    <*> triggerHardFork
    <*> syncInsertOptions
    <*> pure []

syncInsertConfig :: Gen SyncInsertConfig
syncInsertConfig =
  Gen.choice
    [ pure $ SyncInsertConfig (Just FullInsertPreset) fullInsertOptions
    , pure $ SyncInsertConfig (Just OnlyUTxOInsertPreset) onlyUTxOInsertOptions
    , pure $ SyncInsertConfig (Just OnlyGovInsertPreset) onlyGovInsertOptions
    , pure $ SyncInsertConfig (Just DisableAllInsertPreset) disableAllInsertOptions
    , SyncInsertConfig Nothing <$> syncInsertOptions
    ]

syncInsertOptions :: Gen SyncInsertOptions
syncInsertOptions =
  (SyncInsertOptions . TxCBORConfig <$> Gen.bool)
    <*> txOutConfig
    <*> Gen.element [LedgerEnable, LedgerDisable, LedgerIgnore]
    <*> shelleyConfig
    <*> pure (RewardsConfig True)
    <*> multiAssetConfig
    <*> metadataConfig
    <*> plutusConfig
    <*> (GovernanceConfig <$> Gen.bool)
    <*> (OffchainPoolDataConfig <$> Gen.bool)
    <*> (PoolStatsConfig <$> Gen.bool)
    <*> Gen.element [JsonTypeText, JsonTypeJsonb, JsonTypeDisable]
    <*> (RemoveJsonbFromSchemaConfig <$> Gen.bool)

txOutConfig :: Gen TxOutConfig
txOutConfig =
  Gen.choice
    [ TxOutEnable . UseTxOutAddress <$> Gen.bool
    , pure TxOutDisable
    , (TxOutConsumed . ForceTxIn <$> Gen.bool) <*> (UseTxOutAddress <$> Gen.bool)
    , (TxOutConsumedPrune . ForceTxIn <$> Gen.bool) <*> (UseTxOutAddress <$> Gen.bool)
    , (TxOutConsumedBootstrap . ForceTxIn <$> Gen.bool) <*> (UseTxOutAddress <$> Gen.bool)
    ]

shelleyConfig :: Gen ShelleyInsertConfig
shelleyConfig = do
  Gen.choice
    [ pure ShelleyEnable
    , pure ShelleyDisable
    , ShelleyStakeAddrs <$> Gen.nonEmpty (Range.linear 1 5) addr
    ]

multiAssetConfig :: Gen MultiAssetConfig
multiAssetConfig =
  Gen.choice
    [ pure MultiAssetEnable
    , pure MultiAssetDisable
    , MultiAssetPolicies <$> Gen.nonEmpty (Range.linear 1 5) multiAssetPolicy
    ]

metadataConfig :: Gen MetadataConfig
metadataConfig =
  Gen.choice
    [ pure MetadataEnable
    , pure MetadataDisable
    , MetadataKeys <$> Gen.nonEmpty (Range.linear 1 5) metadataKey
    ]

plutusConfig :: Gen PlutusConfig
plutusConfig =
  Gen.choice
    [ pure PlutusEnable
    , pure PlutusDisable
    , PlutusScripts <$> Gen.nonEmpty (Range.linear 1 5) scriptHash
    ]

addr :: Gen ShortByteString
addr = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

multiAssetPolicy :: Gen ShortByteString
multiAssetPolicy = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

metadataKey :: Gen Word
metadataKey = Gen.word (Range.linear minBound maxBound)

scriptHash :: Gen ShortByteString
scriptHash = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

networkName :: Gen NetworkName
networkName = NetworkName <$> Gen.text (Range.linear 0 100) Gen.alpha

hashBlake2b_256 :: MonadGen m => m (Maybe (Hash Blake2b_256 ByteString))
hashBlake2b_256 = serialiseHash <$> Gen.bytes (Range.linear 0 100)
  where
    serialiseHash :: ByteString -> Maybe (Hash Blake2b_256 ByteString)
    serialiseHash = hashFromBytes . digest (Proxy @Blake2b_256)

filePath :: MonadGen m => m FilePath
filePath = Gen.string (Range.linear 0 100) Gen.unicode

protocolVersion :: MonadGen m => m ProtocolVersion
protocolVersion = ProtocolVersion <$> word16 <*> word16 <*> word8
  where
    word16 = Gen.word16 (Range.linear minBound maxBound)
    word8 = Gen.word8 (Range.linear minBound maxBound)

triggerHardFork :: MonadGen m => m TriggerHardFork
triggerHardFork =
  Gen.choice
    [ Gen.constant TriggerHardForkNotDuringThisExecution
    , TriggerHardForkAtEpoch . EpochNo <$> Gen.word64 (Range.linear minBound maxBound)
    , TriggerHardForkAtVersion <$> Gen.word16 (Range.linear minBound maxBound)
    ]

-- | @Logging.Representation@ is not useful for our testing, so we just generate a minimal example
loggingRepresentation :: Logging.Representation
loggingRepresentation =
  Logging.Representation
    { Logging.minSeverity = Logging.Info
    , Logging.rotation = Nothing
    , Logging.setupScribes = []
    , Logging.defaultScribes = []
    , Logging.setupBackends = []
    , Logging.defaultBackends = []
    , Logging.hasEKG = Nothing
    , Logging.hasGraylog = Nothing
    , Logging.hasPrometheus = Nothing
    , Logging.hasGUI = Nothing
    , Logging.traceForwardTo = Nothing
    , Logging.forwardDelay = Nothing
    , Logging.traceAcceptAt = Nothing
    , Logging.options = mempty
    }
