{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  renderRewardAccount,
  stakingCredHash,
  unitIntervalToDouble,
  unCredentialHash,
  unKeyHashRaw,
  unKeyHashView,
  unScriptHash,
  unTxHash,
  unAssetName,
  dataHashToBytes,
  safeHashToByteString,
  toGovAction,
  toVote,
  toVoterRole,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as Db
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Address (serialiseAddress, serialiseRewardAccount)
import Cardano.DbSync.Util.Bech32 (serialiseStakePoolKeyHashToBech32)
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Hashes (SafeHash, ScriptHash (..), extractHash)
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn
import Cardano.Prelude
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Text.Encoding as Text

annotateStakingCred :: Ledger.Network -> Ledger.StakeCredential -> Ledger.RewardAccount
annotateStakingCred = Shelley.RewardAccount

coinToDbLovelace :: Coin -> DbLovelace
coinToDbLovelace = DbLovelace . fromIntegral . unCoin

getPaymentCred :: Ledger.Addr -> Maybe Ledger.PaymentCredential
getPaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref -> Just pcred
    Ledger.AddrBootstrap {} -> Nothing

hasCredScript :: Ledger.Credential kr -> Bool
hasCredScript pc =
  case pc of
    Ledger.ScriptHashObj _ -> True
    Ledger.KeyHashObj {} -> False

maybePaymentCred :: Ledger.Addr -> Maybe ByteString
maybePaymentCred addr =
  case addr of
    Ledger.Addr _nw pcred _sref ->
      Just $ LBS.toStrict (Binary.runPut $ Ledger.putCredential pcred)
    Ledger.AddrBootstrap {} ->
      Nothing

getCredentialScriptHash :: Ledger.Credential kr -> Maybe ByteString
getCredentialScriptHash pc =
  case pc of
    Ledger.ScriptHashObj hash -> Just $ unScriptHash hash
    Ledger.KeyHashObj {} -> Nothing

mkSlotLeader :: Bool -> ByteString -> Maybe Db.PoolHashId -> Db.SlotLeader
mkSlotLeader hashShelley slHash mPoolId =
  let short = Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
      slName = case mPoolId of
        Nothing | hashShelley -> "ShelleyGenesisKey-" <> short
        _ | hashShelley -> "Pool-" <> short
        _ -> "disable-shelley flag used"
   in Db.SlotLeader slHash mPoolId slName

nonceToBytes :: Ledger.Nonce -> Maybe ByteString
nonceToBytes nonce =
  case nonce of
    Ledger.Nonce hash -> Just $ Crypto.hashToBytes hash
    Ledger.NeutralNonce -> Nothing

partitionMIRTargets ::
  [MIRTarget] ->
  ([Map (Ledger.Credential 'Ledger.Staking) DeltaCoin], [Coin])
partitionMIRTargets =
  List.foldl' foldfunc ([], [])
  where
    foldfunc ::
      ([Map (Ledger.Credential 'Ledger.Staking) DeltaCoin], [Coin]) ->
      MIRTarget ->
      ([Map (Ledger.Credential 'Ledger.Staking) DeltaCoin], [Coin])
    foldfunc (xs, ys) mt =
      case mt of
        StakeAddressesMIR x -> (x : xs, ys)
        SendToOppositePotMIR y -> (xs, y : ys)

renderAddress :: Ledger.Addr -> Text
renderAddress = serialiseAddress

renderRewardAccount :: Ledger.RewardAccount -> Text
renderRewardAccount = serialiseRewardAccount

stakingCredHash :: Ledger.Network -> Ledger.StakeCredential -> ByteString
stakingCredHash network = Ledger.serialiseRewardAccount . annotateStakingCred network

unitIntervalToDouble :: Ledger.UnitInterval -> Double
unitIntervalToDouble = fromRational . Ledger.unboundRational

unCredentialHash :: Ledger.Credential kr -> ByteString
unCredentialHash = \case
  Ledger.ScriptHashObj scriptHash -> unScriptHash scriptHash
  Ledger.KeyHashObj keyHash -> unKeyHashRaw keyHash

unKeyHashRaw :: Ledger.KeyHash d -> ByteString
unKeyHashRaw (Ledger.KeyHash kh) = Crypto.hashToBytes kh

unKeyHashView :: Ledger.KeyHash 'Ledger.StakePool -> Text
unKeyHashView = serialiseStakePoolKeyHashToBech32

unScriptHash :: ScriptHash -> ByteString
unScriptHash (ScriptHash h) = Crypto.hashToBytes h

unTxHash :: TxId -> ByteString
unTxHash (TxId txid) = Crypto.hashToBytes $ extractHash txid

unAssetName :: AssetName -> ByteString
unAssetName = SBS.fromShort . assetNameBytes

dataHashToBytes :: DataHash -> ByteString
dataHashToBytes = Crypto.hashToBytes . extractHash

safeHashToByteString :: SafeHash a -> ByteString
safeHashToByteString = Crypto.hashToBytes . extractHash

toGovAction :: GovAction a -> Db.GovActionType
toGovAction = \case
  ParameterChange {} -> Db.ParameterChange
  HardForkInitiation {} -> Db.HardForkInitiation
  TreasuryWithdrawals {} -> Db.TreasuryWithdrawals
  NoConfidence {} -> Db.NoConfidence
  UpdateCommittee {} -> Db.NewCommitteeType
  NewConstitution {} -> Db.NewConstitution
  InfoAction {} -> Db.InfoAction

toVote :: Vote -> Db.Vote
toVote = \case
  VoteNo -> Db.VoteNo
  VoteYes -> Db.VoteYes
  Abstain -> Db.VoteAbstain

toVoterRole :: Voter -> Db.VoterRole
toVoterRole = \case
  CommitteeVoter {} -> Db.ConstitutionalCommittee
  DRepVoter {} -> Db.DRep
  StakePoolVoter {} -> Db.SPO
