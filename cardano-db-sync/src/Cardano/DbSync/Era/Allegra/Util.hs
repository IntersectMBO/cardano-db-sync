{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era.Allegra.Util
  ( annotateStakingCred
  , blockBody
  , blockHash
  , blockNumber
  , blockPrevHash
  , blockProtoVersion
  , blockSize
  , blockTxCount
  , blockTxs
  , blockOpCert
  , blockVrfKeyView
  , blockCreatorPoolHash
  , coinToDbLovelace
  , epochNumber
  , fakeGenesisHash
  , maybePaymentCred
  , mkSlotLeader
  , nonceToBytes
  , renderAddress
  , renderHash
  , renderRewardAcnt
  , slotLeaderHash
  , slotNumber
  , stakingCredHash
  , txFee
  , txHash
  , txCertificates
  , txInputList
  , txInvalidBefore
  , txInvalidHereafter
  , txMetadata
  , txOutputList
  , txOutputSum
  , txParamProposal
  , txWithdrawals
  , txWithdrawalSum
  , txSize
  , unHeaderHash
  , unitIntervalToDouble
  , unKeyHashRaw
  , unKeyHashView
  , unTxHash
  ) where

import           Cardano.Prelude

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Api.Typed as Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES

import           Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as Db
import           Cardano.DbSync.Config
import           Cardano.DbSync.Era.Allegra.Types

import qualified Cardano.Ledger.Core as ShelleyMa
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map
import           Data.MemoBytes (MemoBytes (..))
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Text.Encoding as Text

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import           Ouroboros.Network.Block (BlockNo (..))

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Hashing as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

annotateStakingCred :: DbSyncEnv -> AllegraStakingCred -> Shelley.RewardAcnt StandardAllegra
annotateStakingCred env cred =
  let network =
        case envProtocol env of
          DbSyncProtocolCardano -> envNetwork env
  in Shelley.RewardAcnt network cred

blockBody :: Consensus.ShelleyBlock StandardAllegra -> Shelley.BHBody (Crypto StandardAllegra)
blockBody = Shelley.bhbody . Shelley.bheader . Consensus.shelleyBlockRaw

blockCreatorPoolHash :: Consensus.ShelleyBlock StandardAllegra -> ByteString
blockCreatorPoolHash = unKeyHashRaw . Shelley.issuerIDfromBHBody . blockBody

blockHash :: Consensus.ShelleyBlock StandardAllegra -> ByteString
blockHash =
  Crypto.hashToBytes . Shelley.unHashHeader
    . Consensus.unShelleyHash . Consensus.shelleyBlockHeaderHash

blockNumber :: Consensus.ShelleyBlock StandardAllegra -> Word64
blockNumber = unBlockNo . Shelley.bheaderBlockNo . blockBody

blockPrevHash :: Consensus.ShelleyBlock StandardAllegra -> ByteString
blockPrevHash blk =
  case Shelley.bheaderPrev (Shelley.bhbody . Shelley.bheader $ Consensus.shelleyBlockRaw blk) of
    Shelley.GenesisHash -> fakeGenesisHash
    Shelley.BlockHash h -> Crypto.hashToBytes $ Shelley.unHashHeader h

blockProtoVersion :: Consensus.ShelleyBlock StandardAllegra -> Shelley.ProtVer
blockProtoVersion = Shelley.bprotver . blockBody

blockSize :: Consensus.ShelleyBlock StandardAllegra -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Shelley.bbody . Consensus.shelleyBlockRaw

blockTxCount :: Consensus.ShelleyBlock StandardAllegra -> Word64
blockTxCount = fromIntegral . length . unTxSeq . Shelley.bbody . Consensus.shelleyBlockRaw

blockTxs :: Consensus.ShelleyBlock StandardAllegra -> [Shelley.Tx StandardAllegra]
blockTxs =
    txList . Shelley.bbody . Consensus.shelleyBlockRaw
  where
    txList :: Shelley.TxSeq StandardAllegra -> [Shelley.Tx StandardAllegra]
    txList (Shelley.TxSeq txSeq) = toList txSeq

blockOpCert :: Consensus.ShelleyBlock StandardAllegra -> ByteString
blockOpCert = KES.rawSerialiseVerKeyKES . Shelley.ocertVkHot . Shelley.bheaderOCert . blockBody

blockVrfKeyView :: Consensus.ShelleyBlock StandardAllegra -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey . Shelley.bheaderVrfVk . blockBody

coinToDbLovelace :: Coin -> DbLovelace
coinToDbLovelace = DbLovelace . fromIntegral . unCoin

epochNumber :: Consensus.ShelleyBlock StandardAllegra -> Word64 -> Word64
epochNumber blk slotsPerEpoch = slotNumber blk `div` slotsPerEpoch

-- | This is both the Genesis Hash and the hash of the previous block.
fakeGenesisHash :: ByteString
fakeGenesisHash = BS.take 28 ("GenesisHash " <> BS.replicate 28 '\0')

maybePaymentCred :: Shelley.Addr StandardAllegra -> Maybe ByteString
maybePaymentCred addr =
  case addr of
    Shelley.Addr _nw pcred _sref ->
      Just $ LBS.toStrict (Binary.runPut $ Shelley.putCredential pcred)
    Shelley.AddrBootstrap {} ->
      Nothing

mkSlotLeader :: Consensus.ShelleyBlock StandardAllegra -> Maybe Db.PoolHashId -> Db.SlotLeader
mkSlotLeader blk mPoolId =
  let slHash = slotLeaderHash blk
      short = Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
      slName = case mPoolId of
                Nothing -> "AllegraGenesis-" <> short
                Just _ -> "Pool-" <> short
  in Db.SlotLeader slHash mPoolId slName

nonceToBytes :: Shelley.Nonce -> Maybe ByteString
nonceToBytes nonce =
  case nonce of
    Shelley.Nonce hash -> Just $ Crypto.hashToBytes hash
    Shelley.NeutralNonce -> Nothing

renderAddress :: Shelley.Addr StandardAllegra -> Text
renderAddress a = Api.serialiseAddress (Api.fromShelleyAddr a :: Api.AddressInEra Api.AllegraEra)

renderHash :: ShelleyHash -> Text
renderHash = Text.decodeUtf8 . Base16.encode . unHeaderHash

renderRewardAcnt :: Shelley.RewardAcnt StandardAllegra -> Text
renderRewardAcnt = Api.serialiseAddress . Api.fromShelleyStakeAddr

slotLeaderHash :: Consensus.ShelleyBlock StandardAllegra -> ByteString
slotLeaderHash = unKeyHashRaw . Shelley.issuerIDfromBHBody . blockBody

slotNumber :: Consensus.ShelleyBlock StandardAllegra -> Word64
slotNumber = unSlotNo . Shelley.bheaderSlotNo . blockBody

stakingCredHash :: DbSyncEnv -> AllegraStakingCred -> ByteString
stakingCredHash env = Shelley.serialiseRewardAcnt . annotateStakingCred env

txCertificates :: Shelley.Tx StandardAllegra -> [(Word16, AllegraDCert)]
txCertificates tx =
    zip [0 ..] (toList . ShelleyMa.certs $ unTxBodyRaw tx)

txFee :: Shelley.Tx StandardAllegra -> Word64
txFee = fromIntegral . unCoin . ShelleyMa.txfee . unTxBodyRaw

txHash :: AllegraTx -> ByteString
txHash = Crypto.hashToBytes . Shelley.hashAnnotated . Shelley._body

txInputList :: Shelley.Tx StandardAllegra -> [AllegraTxIn]
txInputList = toList . ShelleyMa.inputs . unTxBodyRaw

txInvalidBefore :: Shelley.Tx StandardAllegra -> Maybe Word64
txInvalidBefore =
  fmap unSlotNo . Shelley.strictMaybeToMaybe . ShelleyMa.validTo . ShelleyMa.vldt . unTxBodyRaw

txInvalidHereafter :: Shelley.Tx StandardAllegra -> Maybe Word64
txInvalidHereafter =
  fmap unSlotNo . Shelley.strictMaybeToMaybe . ShelleyMa.validFrom . ShelleyMa.vldt . unTxBodyRaw

txMetadata :: Shelley.Tx StandardAllegra -> Maybe (ShelleyMa.Metadata StandardAllegra)
txMetadata (Shelley.Tx _body _wit md) = Shelley.strictMaybeToMaybe md

-- Regardless of the type name, this is actually a parameter update *proposal*
-- rather than the update itself.
txParamProposal :: Shelley.Tx StandardAllegra -> Maybe (Shelley.Update StandardAllegra)
txParamProposal = Shelley.strictMaybeToMaybe . ShelleyMa.update . unTxBodyRaw

-- Outputs are ordered, so provide them as such with indices.
txOutputList :: Shelley.Tx StandardAllegra -> [(Word16, AllegraTxOut)]
txOutputList tx =
  zip [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)

txOutputSum :: Shelley.Tx StandardAllegra -> Word64
txOutputSum tx =
    sum $ map outValue (toList . ShelleyMa.outputs $ unTxBodyRaw tx)
  where
    outValue :: Shelley.TxOut StandardAllegra -> Word64
    outValue (Shelley.TxOut _ coin) = fromIntegral $ unCoin coin

txWithdrawals :: Shelley.Tx StandardAllegra -> [(Shelley.RewardAcnt StandardAllegra, Coin)]
txWithdrawals = Map.toList . Shelley.unWdrl . ShelleyMa.wdrls . unTxBodyRaw

txWithdrawalSum :: Shelley.Tx StandardAllegra -> Word64
txWithdrawalSum =
  fromIntegral . sum . map (unCoin . snd) . Map.toList . Shelley.unWdrl
    . ShelleyMa.wdrls . unTxBodyRaw

txSize :: Shelley.Tx StandardAllegra -> Word64
txSize (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = fromIntegral (BSS.length $ memobytes txBody)

unHeaderHash :: Consensus.ShelleyHash StandardAllegra -> ByteString
unHeaderHash = Crypto.hashToBytes . Shelley.unHashHeader . Consensus.unShelleyHash

unitIntervalToDouble :: Shelley.UnitInterval -> Double
unitIntervalToDouble = fromRational . Shelley.unitIntervalToRational

unKeyHashRaw :: Shelley.KeyHash d era -> ByteString
unKeyHashRaw (Shelley.KeyHash kh) = Crypto.hashToBytes kh

unKeyHashView :: Shelley.KeyHash 'Shelley.StakePool StandardCrypto -> Text
unKeyHashView = Api.serialiseToBech32 . Api.StakePoolKeyHash

-- unTxBody :: Shelley.Tx StandardAllegra -> ShelleyMa.TxBody StandardAllegra
-- unTxBody (Shelley.Tx txBody _wit _md) = txBody

unTxBodyRaw :: Shelley.Tx StandardAllegra -> ShelleyMa.TxBodyRaw StandardAllegra
unTxBodyRaw (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

unTxHash :: AllegraTxId -> ByteString
unTxHash (Shelley.TxId txid) = Crypto.hashToBytes txid

unTxSeq :: AllegraTxSeq -> StrictSeq AllegraTx
unTxSeq (Shelley.TxSeq txSeq) = txSeq
