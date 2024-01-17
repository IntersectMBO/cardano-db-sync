{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.DbSync.Config.TypesTest (tests) where

import Cardano.DbSync.Config.Types
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Short (ShortByteString (), toShort)
import Data.Default.Class (Default (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Config.Types"
      [ ("SyncInsertConfig FromJSON", prop_syncInsertConfigFromJSON)
      , ("SyncInsertConfig roundtrip", prop_syncInsertConfigRoundtrip)
      , ("isTxEnabled", prop_isTxEnabled)
      , ("isLedgerEnabled", prop_isLedgerEnabled)
      , ("isShelleyEnabled", prop_isShelleyEnabled)
      , ("isMultiAssetEnabled", prop_isMultiAssetEnabled)
      , ("isMetadataEnabled", prop_isMetadataEnabled)
      , ("isPlutusEnabled", prop_isPlutusEnabled)
      ]

prop_syncInsertConfigFromJSON :: Property
prop_syncInsertConfigFromJSON = property $ do
  json <- forAll genDefaultJson

  Aeson.fromJSON json === Aeson.Success (def :: SyncInsertConfig)

prop_syncInsertConfigRoundtrip :: Property
prop_syncInsertConfigRoundtrip = property $ do
  cfg <- forAll genSyncInsertConfig

  tripping cfg Aeson.encode Aeson.decode

prop_isTxEnabled :: Property
prop_isTxEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let txOutCfg = spcTxOut cfg

  -- TxOut is enabled if it is not TxOutDisable
  isTxOutEnabled txOutCfg === (txOutCfg /= TxOutDisable)

prop_isLedgerEnabled :: Property
prop_isLedgerEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let ledgerCfg = spcLedger cfg

  -- Ledger is enabled if it is not LedgerDisable
  isLedgerEnabled ledgerCfg === (ledgerCfg /= LedgerDisable)

prop_isShelleyEnabled :: Property
prop_isShelleyEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let shelleyCfg = spcShelley cfg

  -- Shelley is enabled if it is not ShelleyDisable
  isShelleyEnabled shelleyCfg === (shelleyCfg /= ShelleyDisable)

prop_isMultiAssetEnabled :: Property
prop_isMultiAssetEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let multiAssetCfg = spcMultiAsset cfg

  -- MultiAsset is enabled if it is not MultiAssetDisable
  isMultiAssetEnabled multiAssetCfg === (multiAssetCfg /= MultiAssetDisable)

prop_isMetadataEnabled :: Property
prop_isMetadataEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let metadataCfg = spcMetadata cfg

  -- Metadata is enabled if it is not MetadataDisable
  isMetadataEnabled metadataCfg === (metadataCfg /= MetadataDisable)

prop_isPlutusEnabled :: Property
prop_isPlutusEnabled = property $ do
  cfg <- forAll genSyncInsertConfig
  let plutusCfg = spcPlutus cfg

  -- Plutus is enabled if it is not PlutusDisable
  isPlutusEnabled plutusCfg === (plutusCfg /= PlutusDisable)

genSyncInsertConfig :: Gen SyncInsertConfig
genSyncInsertConfig =
  SyncInsertConfig
    <$> Gen.element [TxOutEnable, TxOutDisable, TxOutConsumed, TxOutPrune, TxOutBootstrap]
    <*> Gen.element [LedgerEnable, LedgerDisable, LedgerIgnore]
    <*> genShelleyConfig
    <*> genMultiAssetConfig
    <*> genMetadataConfig
    <*> genPlutusConfig
    <*> (GovernanceConfig <$> Gen.bool)
    <*> (OffchainPoolDataConfig <$> Gen.bool)
    <*> Gen.element [JsonTypeText, JsonTypeJsonb, JsonTypeDisable]

genShelleyConfig :: Gen ShelleyInsertConfig
genShelleyConfig = do
  Gen.choice
    [ pure ShelleyEnable
    , pure ShelleyDisable
    , ShelleyStakeAddrs <$> Gen.nonEmpty (Range.linear 1 5) genAddr
    ]

genMultiAssetConfig :: Gen MultiAssetConfig
genMultiAssetConfig =
  Gen.choice
    [ pure MultiAssetEnable
    , pure MultiAssetDisable
    , MultiAssetPolicies <$> Gen.nonEmpty (Range.linear 1 5) genPolicy
    ]

genMetadataConfig :: Gen MetadataConfig
genMetadataConfig =
  Gen.choice
    [ pure MetadataEnable
    , pure MetadataDisable
    , MetadataKeys <$> Gen.nonEmpty (Range.linear 1 5) genKey
    ]

genPlutusConfig :: Gen PlutusConfig
genPlutusConfig =
  Gen.choice
    [ pure PlutusEnable
    , pure PlutusDisable
    , PlutusScripts <$> Gen.nonEmpty (Range.linear 1 5) genScriptHash
    ]

genAddr :: Gen ShortByteString
genAddr = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

genPolicy :: Gen ShortByteString
genPolicy = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

genKey :: Gen Word
genKey = Gen.word (Range.linear minBound maxBound)

genScriptHash :: Gen ShortByteString
genScriptHash = toShort <$> Gen.utf8 (Range.linear 1 5) Gen.unicode

-- | Various JSON values that should generate the default config
genDefaultJson :: Gen Aeson.Value
genDefaultJson =
  Gen.element
    [ [aesonQQ|
        {
          "tx_out": "enable",
          "ledger": "enable",
          "shelley": {
            "enable": true,
            "stake_addresses": null
          },
          "multi_asset": {
            "enable": true,
            "policies": null
          },
          "metadata": {
            "enable": true,
            "keys": null
          },
          "plutus": {
            "enable": true,
            "script_hashes": null
          },
          "governance": "enable",
          "offchain_pool_data": "enable",
          "json_type": "text"
        }
      |]
    , [aesonQQ|
        { }
      |]
    , [aesonQQ|
        {
          "tx_out": "enable",
          "ledger": "enable",
          "shelley": {
            "enable": true
          },
          "multi_asset": {
            "enable": true
          },
          "metadata": {
            "enable": true
          },
          "plutus": {
            "enable": true
          },
          "governance": "enable",
          "offchain_pool_data": "enable",
          "json_type": "text"
        }
      |]
    ]
