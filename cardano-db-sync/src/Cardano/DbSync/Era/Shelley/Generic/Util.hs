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
  renderRewardAcnt,
  stakingCredHash,
  unitIntervalToDouble,
  unCredentialHash,
  unKeyHashRaw,
  unKeyHashView,
  unScriptHash,
  unTxHash,
  unAssetName,
  dataHashToBytes,
  achorHashToBytes,
  toGovAction,
  toVote,
  toVoterRole,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as Db
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Address (serialiseAddress, serialiseRewardAcnt)
import Cardano.DbSync.Util.Bech32 (serialiseStakePoolKeyHashToBech32)
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Governance as Ledger
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
import Ouroboros.Consensus.Cardano.Block (StandardConway, StandardCrypto)

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
renderAddress = serialiseAddress

renderRewardAcnt :: Ledger.RewardAcnt StandardCrypto -> Text
renderRewardAcnt = serialiseRewardAcnt

stakingCredHash :: Ledger.Network -> Ledger.StakeCredential era -> ByteString
stakingCredHash network = Ledger.serialiseRewardAcnt . annotateStakingCred network

unitIntervalToDouble :: Ledger.UnitInterval -> Double
unitIntervalToDouble = fromRational . Ledger.unboundRational

unCredentialHash :: Ledger.Credential kr StandardCrypto -> ByteString
unCredentialHash = \case
  Ledger.ScriptHashObj scriptHash -> unScriptHash scriptHash
  Ledger.KeyHashObj keyHash -> unKeyHashRaw keyHash

unKeyHashRaw :: Ledger.KeyHash d era -> ByteString
unKeyHashRaw (Ledger.KeyHash kh) = Crypto.hashToBytes kh

unKeyHashView :: Ledger.KeyHash 'Ledger.StakePool StandardCrypto -> Text
unKeyHashView = serialiseStakePoolKeyHashToBech32

unScriptHash :: Shelley.ScriptHash StandardCrypto -> ByteString
unScriptHash (Shelley.ScriptHash h) = Crypto.hashToBytes h

unTxHash :: TxId era -> ByteString
unTxHash (TxId txid) = Crypto.hashToBytes $ Ledger.extractHash txid

unAssetName :: AssetName -> ByteString
unAssetName = SBS.fromShort . assetName

dataHashToBytes :: DataHash -> ByteString
dataHashToBytes = Crypto.hashToBytes . Ledger.extractHash

achorHashToBytes :: Ledger.SafeHash StandardCrypto Ledger.AnchorDataHash -> ByteString
achorHashToBytes = Crypto.hashToBytes . Ledger.extractHash

toGovAction :: GovernanceAction StandardConway -> Db.GovActionType
toGovAction = \case
  ParameterChange {} -> Db.ParameterChange
  HardForkInitiation {} -> Db.HardForkInitiation
  TreasuryWithdrawals {} -> Db.TreasuryWithdrawals
  NoConfidence {} -> Db.NoConfidence
  NewCommittee {} -> Db.NewCommitteeType
  NewConstitution {} -> Db.NewConstitution
  InfoAction {} -> Db.InfoAction

toVote :: Vote -> Db.Vote
toVote = \case
  VoteNo -> Db.VoteNo
  VoteYes -> Db.VoteYes
  Abstain -> Db.VoteAbstain

toVoterRole :: Voter StandardCrypto -> Db.VoterRole
toVoterRole = \case
  CommitteeVoter {} -> Db.ConstitutionalCommittee
  DRepVoter {} -> Db.DRep
  StakePoolVoter {} -> Db.SPO
