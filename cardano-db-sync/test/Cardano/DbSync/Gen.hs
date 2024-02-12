{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Gen (
  -- * Config/Api Type generators
  syncNodeParams,
  syncNodeConfig,
  syncInsertConfig,
  shelleyConfig,
  multiAssetConfig,
  metadataConfig,
  plutusConfig,
  multiAssetPolicy,
  metadataKey,
  scriptHash,
  addr,

  -- * Utility generators
  hashBlake2b_256,
  filePath,
  protocolVersion,
  triggerHardFork,
) where

import qualified Cardano.BM.Configuration.Model as Logging
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
    <*> Gen.bool

syncNodeConfig :: Logging.Configuration -> Gen SyncNodeConfig
syncNodeConfig loggingCfg =
  SyncNodeConfig
    <$> (NetworkName <$> Gen.text (Range.linear 0 100) Gen.alpha)
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
    <*> syncInsertConfig

syncInsertConfig :: Gen SyncInsertConfig
syncInsertConfig =
  SyncInsertConfig
    <$> Gen.element [TxOutEnable, TxOutDisable, TxOutConsumed, TxOutPrune, TxOutBootstrap]
    <*> Gen.element [LedgerEnable, LedgerDisable, LedgerIgnore]
    <*> shelleyConfig
    <*> multiAssetConfig
    <*> metadataConfig
    <*> plutusConfig
    <*> (GovernanceConfig <$> Gen.bool)
    <*> (OffchainPoolDataConfig <$> Gen.bool)
    <*> Gen.element [JsonTypeText, JsonTypeJsonb, JsonTypeDisable]

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
