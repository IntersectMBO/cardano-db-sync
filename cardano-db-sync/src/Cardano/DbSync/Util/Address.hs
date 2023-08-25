{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.Address (
  serialiseAddress,
  deserialiseByronAddress,
  deserialiseShelleyAddress,
  serialiseRewardAcnt,
  deserialiseRewardAcnt,
) where

import Cardano.DbSync.Util.Bech32 (deserialiseFromBech32, serialiseToBech32)
import qualified Cardano.Ledger.Address as Address
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Credential (PaymentCredential (), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto ())
import Cardano.Prelude
import Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58, encodeBase58)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import Prelude ()

-- | Serialise a UTxO address. Byron era addresses serialise to base58 and
--   Shelley era addresses serialise to bech32
serialiseAddress :: Address.Addr StandardCrypto -> Text
serialiseAddress (Address.AddrBootstrap addr) = serialiseByronAddress addr
serialiseAddress (Address.Addr net payCred stakeRef) =
  serialiseShelleyAddress net payCred stakeRef

-- | Deserialise a UTxO Byron era address from base58
deserialiseByronAddress :: Crypto c => Text -> Maybe (Address.Addr c)
deserialiseByronAddress base58 = Address.deserialiseAddr =<< rawBytes
  where
    rawBytes = decodeBase58 bitcoinAlphabet $ encodeUtf8 base58

-- | Deserialise a UTxO Shelley era address from bech32
deserialiseShelleyAddress :: Crypto c => Text -> Maybe (Address.Addr c)
deserialiseShelleyAddress bech32 = Address.deserialiseAddr =<< rawBytes
  where
    rawBytes = rightToMaybe $ deserialiseFromBech32 bech32

-- | Serialise a Shelley era stake address to bech32
serialiseRewardAcnt :: Address.RewardAcnt StandardCrypto -> Text
serialiseRewardAcnt acnt@(Address.RewardAcnt net _) =
  serialiseToBech32 (prefix net) (Address.serialiseRewardAcnt acnt)
  where
    prefix Mainnet = "stake"
    prefix Testnet = "stake_test"

-- | Deserialise a Shelley era stake address from bech32
deserialiseRewardAcnt :: Crypto c => Text -> Maybe (Address.RewardAcnt c)
deserialiseRewardAcnt bech32 = Address.deserialiseRewardAcnt =<< rawBytes
  where
    rawBytes = rightToMaybe $ deserialiseFromBech32 bech32

serialiseByronAddress :: Address.BootstrapAddress c -> Text
serialiseByronAddress addr = decodeUtf8 base58
  where
    rawBytes = Address.serialiseAddr $ Address.AddrBootstrap addr
    base58 = encodeBase58 bitcoinAlphabet rawBytes

serialiseShelleyAddress ::
  Network ->
  PaymentCredential c ->
  StakeReference c ->
  Text
serialiseShelleyAddress net payCred stakeRef =
  serialiseToBech32 (prefix net) (Address.serialiseAddr addr)
  where
    prefix :: Network -> Text
    prefix Testnet = "addr_test"
    prefix Mainnet = "addr"

    addr = Address.Addr net payCred stakeRef
