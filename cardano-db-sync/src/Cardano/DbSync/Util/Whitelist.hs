module Cardano.DbSync.Util.Whitelist where

import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Config.Types (MultiAssetConfig (..), PlutusConfig (..), ShelleyInsertConfig (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Prelude (ByteString, NonEmpty)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Map (keys)

-- check both whitelist but also checking plutus Maybes first
plutusMultiAssetWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
plutusMultiAssetWhitelistCheck syncEnv txOuts =
  plutusWhitelistCheck syncEnv txOuts || multiAssetWhitelistCheck syncEnv txOuts

plutusWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
plutusWhitelistCheck syncEnv txOuts = do
  -- first check the config option
  case ioPlutus iopts of
    PlutusEnable -> True
    PlutusDisable -> True
    PlutusScripts plutusWhitelist -> plutuswhitelistCheck plutusWhitelist
  where
    iopts = soptInsertOptions $ envOptions syncEnv
    plutuswhitelistCheck :: NonEmpty ShortByteString -> Bool
    plutuswhitelistCheck whitelist =
      any (\txOut -> isScriptHashWhitelisted whitelist txOut || isAddressWhitelisted whitelist txOut) txOuts
    -- check if the script hash is in the whitelist
    isScriptHashWhitelisted :: NonEmpty ShortByteString -> Generic.TxOut -> Bool
    isScriptHashWhitelisted whitelist txOut =
      maybe False ((`elem` whitelist) . toShort . Generic.txScriptHash) (Generic.txOutScript txOut)
    -- check if the address is in the whitelist
    isAddressWhitelisted :: NonEmpty ShortByteString -> Generic.TxOut -> Bool
    isAddressWhitelisted whitelist txOut =
      maybe False ((`elem` whitelist) . toShort) (Generic.maybePaymentCred $ Generic.txOutAddress txOut)

multiAssetWhitelistCheck :: SyncEnv -> [Generic.TxOut] -> Bool
multiAssetWhitelistCheck syncEnv txOuts = do
  let iopts = soptInsertOptions $ envOptions syncEnv
  case ioMultiAssets iopts of
    MultiAssetEnable -> True
    MultiAssetDisable -> True
    MultiAssetPolicies multiAssetWhitelist ->
      or multiAssetwhitelistCheck
      where
        -- txOutMaValue is a Map and we want to check if any of the keys match our whitelist
        multiAssetwhitelistCheck :: [Bool]
        multiAssetwhitelistCheck =
          ( \txout ->
              any (checkMAValueMap multiAssetWhitelist) (keys $ Generic.txOutMaValue txout)
          )
            <$> txOuts

        checkMAValueMap :: NonEmpty ShortByteString -> PolicyID StandardCrypto -> Bool
        checkMAValueMap maWhitelist policyId =
          toShort (Generic.unScriptHash (policyID policyId)) `elem` maWhitelist

shelleyInsertWhitelistCheck :: ShelleyInsertConfig -> ByteString -> Bool
shelleyInsertWhitelistCheck shelleyInsertOpts stakeAddress = do
  case shelleyInsertOpts of
    ShelleyEnable -> True
    ShelleyDisable -> True
    ShelleyStakeAddrs shelleyWhitelist -> toShort stakeAddress `elem` shelleyWhitelist
