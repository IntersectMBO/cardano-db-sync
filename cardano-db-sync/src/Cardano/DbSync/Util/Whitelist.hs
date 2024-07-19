{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.Whitelist where

import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Config.Types (MultiAssetConfig (..), PlutusConfig (..), ShelleyInsertConfig (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error (shortBsBase16Encode)
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (MultiAsset (..), PolicyID (..))
import Cardano.Prelude (NonEmpty)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Map (keys)

-- check both whitelist but also checking plutus Maybes first
plutusMultiAssetWhitelistCheck ::
  SyncEnv ->
  -- | TxMint
  MultiAsset StandardCrypto ->
  -- | TxOuts
  [Generic.TxOut] ->
  Bool
plutusMultiAssetWhitelistCheck syncEnv txMints txOuts =
  isPlutusScriptHashesInWhitelist syncEnv txOuts || isMAPoliciesInWhitelist syncEnv txMints txOuts

-- | Check if any script hash or address is in the whitelist
isPlutusScriptHashesInWhitelist :: SyncEnv -> [Generic.TxOut] -> Bool
isPlutusScriptHashesInWhitelist syncEnv txOuts = do
  case ioPlutus iopts of
    PlutusEnable -> True
    PlutusDisable -> False
    PlutusScripts whitelist ->
      any (\txOut -> isScriptHashWhitelisted whitelist txOut || isAddressWhitelisted whitelist txOut) txOuts
  where
    iopts = soptInsertOptions $ envOptions syncEnv
    -- check if the script hash is in the whitelist
    isScriptHashWhitelisted :: NonEmpty ShortByteString -> Generic.TxOut -> Bool
    isScriptHashWhitelisted whitelist txOut =
      maybe False ((`elem` whitelist) . toShort . Generic.txScriptHash) (Generic.txOutScript txOut)
    -- check if the address is in the whitelist
    isAddressWhitelisted :: NonEmpty ShortByteString -> Generic.TxOut -> Bool
    isAddressWhitelisted whitelist txOut =
      maybe False ((`elem` whitelist) . toShort) (Generic.maybePaymentCred $ Generic.txOutAddress txOut)

isSimplePlutusScriptHashInWhitelist :: SyncEnv -> ByteString -> Bool
isSimplePlutusScriptHashInWhitelist syncEnv scriptHash = do
  case ioPlutus iopts of
    PlutusEnable -> True
    PlutusDisable -> False
    PlutusScripts plutusWhitelist -> toShort scriptHash `elem` plutusWhitelist
  where
    iopts = soptInsertOptions $ envOptions syncEnv

isMAPoliciesInWhitelist ::
  SyncEnv ->
  -- | TxMint
  MultiAsset StandardCrypto ->
  -- | TxOuts
  [Generic.TxOut] ->
  Bool
isMAPoliciesInWhitelist syncEnv (MultiAsset mintMap) txOuts = do
  let iopts = soptInsertOptions $ envOptions syncEnv
  case ioMultiAssets iopts of
    MultiAssetEnable -> True
    MultiAssetDisable -> True
    MultiAssetPolicies multiAssetWhitelist ->
      mintPoliciesCheck || txOutPoliciesCheck
      where
        mintPoliciesCheck :: Bool
        mintPoliciesCheck = any (checkMAValueMap multiAssetWhitelist) mintPolicies

        txOutPoliciesCheck :: Bool
        txOutPoliciesCheck =
          any
            ( \txout ->
                any (checkMAValueMap multiAssetWhitelist) (keys $ Generic.txOutMaValue txout)
            )
            txOuts

        checkMAValueMap :: NonEmpty ShortByteString -> PolicyID StandardCrypto -> Bool
        checkMAValueMap maWhitelist policyId =
          toShort (Generic.unScriptHash (policyID policyId)) `elem` maWhitelist

        mintPolicies :: [PolicyID StandardCrypto]
        mintPolicies = keys mintMap

shelleyStkAddrWhitelistCheckWithAddr ::
  SyncEnv ->
  Ledger.Addr StandardCrypto ->
  Bool
shelleyStkAddrWhitelistCheckWithAddr syncEnv addr = do
  case addr of
    Ledger.AddrBootstrap {} -> False
    Ledger.Addr network _pcred stakeRef ->
      case stakeRef of
        Ledger.StakeRefBase cred -> shelleyStakeAddrWhitelistCheck syncEnv $ Ledger.RewardAccount network cred
        Ledger.StakeRefPtr _ -> True
        Ledger.StakeRefNull -> True

-- | This allows ShelleyDisabled to also pass through for specific cases.
shelleyCustomStakeWhitelistCheck :: SyncEnv -> Ledger.RewardAccount StandardCrypto -> Bool
shelleyCustomStakeWhitelistCheck syncEnv rwdAcc = do
  case ioShelley iopts of
    ShelleyDisable -> True
    ShelleyEnable -> True
    ShelleyStakeAddrs shelleyWhitelist -> checkShelleyWhitelist shelleyWhitelist rwdAcc
  where
    iopts = soptInsertOptions $ envOptions syncEnv

shelleyStakeAddrWhitelistCheck :: SyncEnv -> Ledger.RewardAccount StandardCrypto -> Bool
shelleyStakeAddrWhitelistCheck syncEnv rwdAcc = do
  case ioShelley iopts of
    ShelleyDisable -> False
    ShelleyEnable -> True
    ShelleyStakeAddrs shelleyWhitelist -> checkShelleyWhitelist shelleyWhitelist rwdAcc
  where
    iopts = soptInsertOptions $ envOptions syncEnv

-- | Check Shelley is enabled and if the stake address is in the whitelist
checkShelleyWhitelist :: NonEmpty ShortByteString -> Ledger.RewardAccount StandardCrypto -> Bool
checkShelleyWhitelist shelleyWhitelist rwdAcc = do
  shortBsBase16Encode stakeAddress `elem` shelleyWhitelist
  where
    network = Ledger.raNetwork rwdAcc
    rewardCred = Ledger.raCredential rwdAcc
    stakeAddress = Ledger.serialiseRewardAccount (Ledger.RewardAccount network rewardCred)
