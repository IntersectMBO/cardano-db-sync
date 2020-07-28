{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Era.Shelley.Util
  ( blockBody
  , blockHash
  , blockNumber
  , blockPrevHash
  , blockProtoVersion
  , blockSize
  , blockTxCount
  , blockTxs
  , blockOpCert
  , blockVrfKey
  , blockVrfKeyToPoolHash
  , epochNumber
  , fakeGenesisHash
  , maybePaymentCred
  , mkSlotLeader
  , nonceToBytes
  , pointToSlotHash
  , renderAddress
  , renderHash
  , slotLeaderHash
  , slotNumber
  , stakingCredHash
  , txFee
  , txHash
  , txDelegationCerts
  , txInputList
  , txOutputList
  , txOutputSum
  , txMirCertificates
  , txParamUpdate
  , txPoolCertificates
  , txWithdrawals
  , unHeaderHash
  , unitIntervalToDouble
  , unKeyHashBS
  , unTxHash
  ) where

import           Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Crypto.VRF.Class as VRF

import qualified Cardano.Db as Db
import           Cardano.DbSync.Config
import           Cardano.DbSync.Types

import qualified Cardano.Api.Typed as Api

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Crypto as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Hashing as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley

blockBody :: Shelley.ShelleyBlock TPraosStandardCrypto -> Shelley.BHBody TPraosStandardCrypto
blockBody = Shelley.bhbody . Shelley.bheader . Shelley.shelleyBlockRaw

blockHash :: Shelley.ShelleyBlock TPraosStandardCrypto -> ByteString
blockHash = unHeaderHash . Shelley.shelleyBlockHeaderHash

blockNumber :: Shelley.ShelleyBlock TPraosStandardCrypto -> Word64
blockNumber =
  unBlockNo . Shelley.bheaderBlockNo . blockBody

blockPrevHash :: Shelley.ShelleyBlock TPraosStandardCrypto -> ByteString
blockPrevHash blk =
  case Shelley.bheaderPrev (Shelley.bhbody . Shelley.bheader $ Shelley.shelleyBlockRaw blk) of
    Shelley.GenesisHash -> fakeGenesisHash
    Shelley.BlockHash h -> Crypto.hashToBytes $ Shelley.unHashHeader h

blockProtoVersion :: Shelley.BHBody TPraosStandardCrypto -> Text
blockProtoVersion = Text.pack . show . Shelley.bprotver

blockSize :: Shelley.ShelleyBlock TPraosStandardCrypto -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Shelley.bbody . Shelley.shelleyBlockRaw

blockTxCount :: Shelley.ShelleyBlock TPraosStandardCrypto -> Word64
blockTxCount = fromIntegral . length . unTxSeq . Shelley.bbody . Shelley.shelleyBlockRaw

blockTxs :: Shelley.ShelleyBlock TPraosStandardCrypto -> [ShelleyTx]
blockTxs =
    txList . Shelley.bbody . Shelley.shelleyBlockRaw
  where
    txList :: ShelleyTxSeq -> [ShelleyTx]
    txList (Shelley.TxSeq txSeq) = toList txSeq

-- blockLeaderVrf :: Shelley.BHBody TPraosStandardCrypto -> ByteString
-- blockLeaderVrf = _ . Shelley.bheaderL

blockOpCert :: Shelley.BHBody TPraosStandardCrypto -> ByteString
blockOpCert = KES.rawSerialiseVerKeyKES . Shelley.ocertVkHot . Shelley.bheaderOCert

blockVrfKey :: Shelley.BHBody TPraosStandardCrypto -> ByteString
blockVrfKey = VRF.rawSerialiseVerKeyVRF . Shelley.bheaderVrfVk

blockVrfKeyToPoolHash :: Shelley.ShelleyBlock TPraosStandardCrypto -> ByteString
blockVrfKeyToPoolHash =
 Crypto.digest (Proxy :: Proxy Crypto.Blake2b_224) . slotLeaderHash

epochNumber :: Shelley.ShelleyBlock TPraosStandardCrypto -> Word64 -> Word64
epochNumber blk slotsPerEpoch = slotNumber blk `div` slotsPerEpoch

-- | This is both the Genesis Hash and the hash of the previous block.
fakeGenesisHash :: ByteString
fakeGenesisHash = BS.take 32 ("GenesisHash " <> BS.replicate 32 '\0')

maybePaymentCred :: Shelley.Addr TPraosStandardCrypto -> Maybe ByteString
maybePaymentCred addr =
  case addr of
    Shelley.Addr _nw pcred _sref ->
      Just $ LBS.toStrict (Binary.runPut $ Shelley.putCredential pcred)
    Shelley.AddrBootstrap {} ->
      Nothing

mkSlotLeader :: Shelley.ShelleyBlock TPraosStandardCrypto -> Maybe Db.PoolHashId -> Db.SlotLeader
mkSlotLeader blk mPoolId =
  let slHash = slotLeaderHash blk
      short = Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
      slName = case mPoolId of
                Nothing -> "ShelleyGenesis-" <> short
                Just _ -> "Pool-" <> short
  in Db.SlotLeader slHash mPoolId slName


nonceToBytes :: Shelley.Nonce -> ByteString
nonceToBytes nonce =
  case nonce of
    Shelley.Nonce hash -> Crypto.hashToBytes hash
    Shelley.NeutralNonce -> BS.replicate 28 '\0'

-- | Convert from Ouroboros 'Point' to `Shelley' types.
pointToSlotHash :: Point (Shelley.ShelleyBlock TPraosStandardCrypto) -> Maybe (SlotNo, ShelleyHash)
pointToSlotHash (Point x) =
  case x of
    Origin -> Nothing
    At blk -> Just (Point.blockPointSlot blk, Point.blockPointHash blk)

renderAddress :: Shelley.Addr TPraosStandardCrypto -> Text
renderAddress addr =
  case addr of
    Shelley.Addr nw pcred sref ->
      Api.serialiseAddress (Api.ShelleyAddress nw pcred sref)
    Shelley.AddrBootstrap (Shelley.BootstrapAddress baddr) ->
      Api.serialiseAddress (Api.ByronAddress baddr :: Api.Address Api.Byron)

renderHash :: ShelleyHash -> Text
renderHash = Text.decodeUtf8 . Base16.encode . unHeaderHash

slotLeaderHash :: Shelley.ShelleyBlock TPraosStandardCrypto -> ByteString
slotLeaderHash =
  DSIGN.rawSerialiseVerKeyDSIGN . unVKey . Shelley.bheaderVk . blockBody

slotNumber :: Shelley.ShelleyBlock TPraosStandardCrypto -> Word64
slotNumber = unSlotNo . Shelley.bheaderSlotNo . blockBody

stakingCredHash :: DbSyncEnv -> ShelleyStakingCred -> ByteString
stakingCredHash env cred =
  let network =
        case envProtocol env of
          DbSyncProtocolByron -> Shelley.Mainnet -- Should not happen
          DbSyncProtocolShelley -> envNetwork env
          DbSyncProtocolCardano -> envNetwork env
  in Shelley.serialiseRewardAcnt $ Shelley.RewardAcnt network cred


txDelegationCerts :: ShelleyTx -> [ShelleyDelegCert]
txDelegationCerts tx =
    mapMaybe extractDelegationCerts $ toList (Shelley._certs $ Shelley._body tx)
  where
    extractDelegationCerts :: ShelleyDCert -> Maybe ShelleyDelegCert
    extractDelegationCerts dcert =
      case dcert of
        Shelley.DCertDeleg pcert -> Just pcert
        _otherwise -> Nothing

txFee :: ShelleyTx -> Word64
txFee = fromIntegral . unCoin . Shelley._txfee . Shelley._body

txHash :: ShelleyTx -> ByteString
txHash = Crypto.hashToBytes . Shelley.hashAnnotated . Shelley._body

txInputList :: ShelleyTx -> [ShelleyTxIn]
txInputList = toList . Shelley._inputs . Shelley._body

txMirCertificates :: ShelleyTx -> [ShelleyMIRCert]
txMirCertificates tx =
    mapMaybe extractMirCert $ toList (Shelley._certs $ Shelley._body tx)
  where
    extractMirCert :: ShelleyDCert -> Maybe ShelleyMIRCert
    extractMirCert dcert =
      case dcert of
        Shelley.DCertMir mcert -> Just mcert
        _otherwise -> Nothing

txParamUpdate :: ShelleyTx -> Maybe (Shelley.Update TPraosStandardCrypto)
txParamUpdate = Shelley.strictMaybeToMaybe . Shelley._txUpdate . Shelley._body

txPoolCertificates :: ShelleyTx -> [ShelleyPoolCert]
txPoolCertificates tx =
    mapMaybe extractPoolCertificate $ toList (Shelley._certs $ Shelley._body tx)
  where
    extractPoolCertificate :: ShelleyDCert -> Maybe ShelleyPoolCert
    extractPoolCertificate dcert =
      case dcert of
        Shelley.DCertPool pcert -> Just pcert
        _otherwise -> Nothing

-- Outputs are ordered, so provide them as such with indices.
txOutputList :: ShelleyTx -> [(Word16, ShelleyTxOut)]
txOutputList tx =
  zip [0 .. ] $ toList (Shelley._outputs $ Shelley._body tx)

txOutputSum :: ShelleyTx -> Word64
txOutputSum tx =
    foldl' (+) 0 $ map outValue (Shelley._outputs $ Shelley._body tx)
  where
    outValue :: ShelleyTxOut -> Word64
    outValue (Shelley.TxOut _ coin) = fromIntegral $ unCoin coin

txWithdrawals :: ShelleyTx -> [(Shelley.RewardAcnt TPraosStandardCrypto, Coin)]
txWithdrawals = Map.toList . Shelley.unWdrl . Shelley._wdrls . Shelley._body

unHeaderHash :: ShelleyHash -> ByteString
unHeaderHash = Crypto.hashToBytes . Shelley.unHashHeader . Shelley.unShelleyHash

unitIntervalToDouble :: Shelley.UnitInterval -> Double
unitIntervalToDouble = fromRational . Shelley.unitIntervalToRational

unKeyHash :: Shelley.KeyHash d crypto -> Crypto.Hash (Shelley.ADDRHASH crypto) (DSIGN.VerKeyDSIGN (Shelley.DSIGN crypto))
unKeyHash (Shelley.KeyHash x) = x

unKeyHashBS :: Shelley.KeyHash d crypto -> ByteString
unKeyHashBS = Crypto.hashToBytes . unKeyHash

unTxHash :: ShelleyTxId -> ByteString
unTxHash (Shelley.TxId txid) = Crypto.hashToBytes txid

unTxSeq :: Shelley.Crypto c => Shelley.TxSeq c -> StrictSeq (Shelley.Tx c)
unTxSeq (Shelley.TxSeq txSeq) = txSeq

unVKey :: Shelley.VKey kd crypto -> DSIGN.VerKeyDSIGN (Shelley.DSIGN crypto)
unVKey (Shelley.VKey a) = a
