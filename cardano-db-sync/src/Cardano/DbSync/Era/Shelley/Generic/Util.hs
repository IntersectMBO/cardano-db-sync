{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Need this because both ghc-8.6.5 and ghc-8.10.2 incorrectly warns about a redundant constraint
-- in the definition of renderAddress.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.DbSync.Era.Shelley.Generic.Util (
  annotateStakingCred,
  coinToDbLovelace,
  getPaymentCred,
  hasCredScript,
  getCredentialScriptHash,
  maybePaymentCred,
  mkSlotLeader,
  nonceToBytes,
  partitionMIRTargets,
  renderAddress,
  renderRewardAcnt,
  stakingCredHash,
  unitIntervalToDouble,
  unKeyHashRaw,
  unKeyHashView,
  unScriptHash,
  unTxHash,
  unAssetName,
  dataHashToBytes,
) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as Db
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import Cardano.Ledger.Shelley.Tx (TxId (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Prelude
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

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
        Nothing -> "ShelleyGenesisKey-" <> short
        Just _ -> "Pool-" <> short
   in Db.SlotLeader slHash mPoolId slName

nonceToBytes :: Ledger.Nonce -> Maybe ByteString
nonceToBytes nonce =
  case nonce of
    Ledger.Nonce hash -> Just $ Crypto.hashToBytes hash
    Ledger.NeutralNonce -> Nothing

partitionMIRTargets ::
  [Shelley.MIRTarget StandardCrypto] ->
  ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
partitionMIRTargets =
  List.foldl' foldfunc ([], [])
  where
    foldfunc ::
      ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin]) ->
      Shelley.MIRTarget StandardCrypto ->
      ([Map (Ledger.Credential 'Ledger.Staking StandardCrypto) DeltaCoin], [Coin])
    foldfunc (xs, ys) mt =
      case mt of
        Shelley.StakeAddressesMIR x -> (x : xs, ys)
        Shelley.SendToOppositePotMIR y -> (xs, y : ys)

renderAddress :: Ledger.Addr StandardCrypto -> Text
renderAddress = Api.serialiseAddress . Api.fromShelleyAddrToAny

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

unAssetName :: AssetName -> ByteString
unAssetName = SBS.fromShort . assetName

dataHashToBytes :: DataHash -> ByteString
dataHashToBytes dataHash = Crypto.hashToBytes (Ledger.extractHash dataHash)
