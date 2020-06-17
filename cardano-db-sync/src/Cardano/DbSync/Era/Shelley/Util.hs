{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Util
  ( blockHash
  , blockNumber
  , blockPrevHash
  , blockSize
  , blockTxCount
  , blockTxs
  , epochNumber
  , fakeGenesisHash
  , mkSlotLeader
  , pointToSlotHash
  , renderHash
  , rewardAccountHash
  , slotNumber
  , txFee
  , txHash
  , txInputList
  , txOutputList
  , txOutputSum
  , txPoolCertificates
  , unCoin
  , unHeaderHash
  , unKeyHashBS
  , unTxHash
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as Binary
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Crypto.DSIGN as DSIGN

import qualified Cardano.Db as Db
import           Cardano.DbSync.Types

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Text.Encoding as Text

import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Crypto as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

blockHash :: ShelleyBlock -> ByteString
blockHash = unHeaderHash . Shelley.shelleyBlockHeaderHash

blockNumber :: ShelleyBlock -> Word64
blockNumber =
  unBlockNo . Shelley.bheaderBlockNo . Shelley.bhbody . Shelley.bheader . Shelley.shelleyBlockRaw

blockPrevHash :: ShelleyBlock -> ByteString
blockPrevHash blk =
  case Shelley.bheaderPrev (Shelley.bhbody . Shelley.bheader $ Shelley.shelleyBlockRaw blk) of
    Shelley.GenesisHash -> fakeGenesisHash
    Shelley.BlockHash h -> Crypto.getHash $ Shelley.unHashHeader h

blockSize :: ShelleyBlock -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Shelley.bbody . Shelley.shelleyBlockRaw

blockTxCount :: ShelleyBlock -> Word64
blockTxCount = fromIntegral . length . getTxInternalUnsafe . Shelley.bbody . Shelley.shelleyBlockRaw

blockTxs :: ShelleyBlock -> [ShelleyTx]
blockTxs =
    txList . Shelley.bbody . Shelley.shelleyBlockRaw
  where
    txList :: ShelleyTxSeq -> [ShelleyTx]
    txList (Shelley.TxSeq txSeq) = toList txSeq

epochNumber :: ShelleyBlock -> Word64 -> Word64
epochNumber blk slotsPerEpoch = slotNumber blk `div` slotsPerEpoch

fakeGenesisHash :: ByteString
fakeGenesisHash = BS.take 32 (" G e n e s i s - H a s h " <> BS.replicate 32 ' ')

mkSlotLeader :: ShelleyBlock -> Db.SlotLeader
mkSlotLeader blk =
  let slHash = Binary.serialize' . Shelley.bheaderVk . Shelley.bhbody $ Shelley.bheader (Shelley.shelleyBlockRaw blk)
      slName = "SlotLeader-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
  in Db.SlotLeader slHash slName


-- | Convert from Ouroboros 'Point' to `Shelley' types.
pointToSlotHash :: Point ShelleyBlock -> Maybe (SlotNo, ShelleyHash)
pointToSlotHash (Point x) =
  case x of
    Origin -> Nothing
    At blk -> Just (Point.blockPointSlot blk, Point.blockPointHash blk)

renderHash :: ShelleyHash -> Text
renderHash = Text.decodeUtf8 . Base16.encode . unHeaderHash

rewardAccountHash :: ShelleyRewardAccount -> ByteString
rewardAccountHash ra =
  case Shelley.getRwdCred ra of
    Shelley.ScriptHashObj sh -> Crypto.getHash $ unScriptHash sh
    Shelley.KeyHashObj kh -> unKeyHashBS kh
  where
    unScriptHash :: Shelley.ScriptHash crypto -> Shelley.Hash crypto (Shelley.Script crypto)
    unScriptHash (Shelley.ScriptHash x) = x

slotNumber :: ShelleyBlock -> Word64
slotNumber =
  unSlotNo . Shelley.bheaderSlotNo . Shelley.bhbody . Shelley.bheader . Shelley.shelleyBlockRaw


txFee :: ShelleyTx -> Word64
txFee = unCoin . Shelley._txfee . Shelley._body

txHash :: ShelleyTx -> ByteString
txHash = Crypto.getHash . Shelley.hashTxBody . Shelley._body

txInputList :: ShelleyTx -> [ShelleyTxIn]
txInputList = toList . Shelley._inputs . Shelley._body

txPoolCertificates :: ShelleyTxBody -> [ShelleyPoolCert]
txPoolCertificates txBody =
    mapMaybe extractPoolCertificate certsList
  where
    certsList :: [ShelleyDCert]
    certsList = toList (Shelley._certs txBody)

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
    outValue (Shelley.TxOut _ coin) = unCoin coin

unCoin :: Coin -> Word64
unCoin (Coin c) = fromIntegral c

unHeaderHash :: ShelleyHash -> ByteString
unHeaderHash = Crypto.getHash . Shelley.unHashHeader . Shelley.unShelleyHash

unKeyHash :: Shelley.KeyHash d crypto -> Shelley.Hash crypto (DSIGN.VerKeyDSIGN (Shelley.DSIGN crypto))
unKeyHash (Shelley.KeyHash x) = x

unKeyHashBS :: Shelley.KeyHash d crypto -> ByteString
unKeyHashBS kh = Crypto.getHash $ unKeyHash kh

unTxHash :: ShelleyTxId -> ByteString
unTxHash (Shelley.TxId txid) = Crypto.getHash txid

-- -------------------------------------------------------------------------------------------------
-- Internal

getTxInternalUnsafe :: Shelley.Crypto c => Shelley.TxSeq c -> StrictSeq (Shelley.Tx c)
getTxInternalUnsafe (Shelley.TxSeq txSeq) = txSeq
