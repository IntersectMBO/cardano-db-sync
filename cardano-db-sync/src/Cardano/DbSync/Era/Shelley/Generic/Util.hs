{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- Need this because both ghc-8.6.5 and ghc-8.10.2 incorrectly warns about a redundant constraint
-- in the definition of renderAddress.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.DbSync.Era.Shelley.Generic.Util
  ( annotateStakingCred
  , coinToDbLovelace
  , getPaymentCred
  , hasCredScript
  , getCredentialScriptHash
  , maybePaymentCred
  , mkSlotLeader
  , nonceToBytes
  , partitionMIRTargets
  , renderAddress
  , renderLanguageCostModel
  , renderRewardAcnt
  , stakingCredHash
  , unitIntervalToDouble
  , unKeyHashRaw
  , unKeyHashView
  , unScriptHash
  , unTxHash
  ) where

import           Cardano.Prelude

import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as Db

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Alonzo.Language (Language)
import           Cardano.Ledger.Alonzo.Scripts (CostModel (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import           Cardano.Ledger.Shelley.Tx (TxId (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import qualified Cardano.Ledger.SafeHash as Ledger

import           Cardano.DbSync.Util

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto, StandardMary,
                   StandardShelley)


annotateStakingCred :: Ledger.Network -> Ledger.StakeCredential era -> Ledger.RewardAcnt era
annotateStakingCred = Shelley.RewardAcnt

coinToDbLovelace :: Coin -> DbLovelace
coinToDbLovelace = DbLovelace . fromIntegral . unCoin

getPaymentCred :: Ledger.Addr StandardCrypto -> Maybe (Ledger.PaymentCredential StandardCrypto)
getPaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref -> Just pcred
    Ledger.AddrBootstrap {} -> Nothing

hasCredScript :: Ledger.Credential kr StandardCrypto -> Bool
hasCredScript pc =
  case pc of
    Ledger.ScriptHashObj _ -> True
    Ledger.KeyHashObj {} -> False

maybePaymentCred :: Ledger.Addr era -> Maybe ByteString
maybePaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref ->
      Just $ LBS.toStrict (Binary.runPut $ Ledger.putCredential pcred)
    Ledger.AddrBootstrap {} ->
      Nothing

getCredentialScriptHash :: Ledger.Credential kr StandardCrypto -> Maybe ByteString
getCredentialScriptHash pc =
  case pc of
    Ledger.ScriptHashObj hash -> Just $ unScriptHash hash
    Ledger.KeyHashObj {} -> Nothing

mkSlotLeader :: ByteString -> Maybe Db.PoolHashId -> Db.SlotLeader
mkSlotLeader slHash mPoolId =
  let short = Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
      slName = case mPoolId of
                Nothing -> "ShelleyGenesis-" <> short
                Just _ -> "Pool-" <> short
  in Db.SlotLeader slHash mPoolId slName

nonceToBytes :: Ledger.Nonce -> Maybe ByteString
nonceToBytes nonce =
  case nonce of
    Ledger.Nonce hash -> Just $ Crypto.hashToBytes hash
    Ledger.NeutralNonce -> Nothing

partitionMIRTargets
    :: [Shelley.MIRTarget StandardCrypto]
    -> ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
partitionMIRTargets =
    List.foldl' foldfunc ([], [])
  where
    foldfunc
        :: ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
        -> Shelley.MIRTarget StandardCrypto
        -> ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
    foldfunc (xs, ys) mt =
      case mt of
        Shelley.StakeAddressesMIR x -> (x : xs, ys)
        Shelley.SendToOppositePotMIR y -> (xs, y : ys)

type family LedgerEraToApiEra ledgerera where
  LedgerEraToApiEra StandardShelley = Api.ShelleyEra
  LedgerEraToApiEra StandardAllegra = Api.AllegraEra
  LedgerEraToApiEra StandardMary = Api.MaryEra

renderAddress
    :: forall era ledgerera.
       LedgerEraToApiEra ledgerera ~ era
    => Api.ShelleyLedgerEra era ~ ledgerera
    => Api.IsShelleyBasedEra era
    => ledgerera ~ StandardShelley
    => Ledger.Addr StandardCrypto -> Text
renderAddress addr = Api.serialiseAddress (Api.fromShelleyAddr addr :: Api.AddressInEra era)

renderCostModel :: CostModel -> Text
renderCostModel (CostModel x) = textShow x

renderLanguageCostModel :: Map Language CostModel -> Text
renderLanguageCostModel mlc = textShow $ Map.map renderCostModel mlc

renderRewardAcnt :: Ledger.RewardAcnt StandardCrypto -> Text
renderRewardAcnt = Api.serialiseAddress . Api.fromShelleyStakeAddr

stakingCredHash :: Ledger.Network -> Ledger.StakeCredential era -> ByteString
stakingCredHash network = Ledger.serialiseRewardAcnt . annotateStakingCred network

unitIntervalToDouble :: Ledger.UnitInterval -> Double
unitIntervalToDouble = fromRational . Ledger.unboundRational

unKeyHashRaw :: Ledger.KeyHash d era -> ByteString
unKeyHashRaw (Ledger.KeyHash kh) = Crypto.hashToBytes kh

unKeyHashView :: Ledger.KeyHash 'Ledger.StakePool StandardCrypto -> Text
unKeyHashView = Api.serialiseToBech32 . Api.StakePoolKeyHash

unScriptHash :: Shelley.ScriptHash StandardCrypto -> ByteString
unScriptHash (Shelley.ScriptHash h) = Crypto.hashToBytes h

unTxHash :: TxId era -> ByteString
unTxHash (TxId txid) = Crypto.hashToBytes $ Ledger.extractHash txid
