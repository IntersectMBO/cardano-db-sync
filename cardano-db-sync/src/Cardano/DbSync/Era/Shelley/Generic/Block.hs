{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.DbSync.Era.Shelley.Generic.Block
  ( Block (..)
  , BlockEra (..)
  , fromShelleyBlock
  , fromAllegraBlock
  , fromMaryBlock
  , fromAlonzoBlock

  , slotLeaderHash
  ) where

import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES

import           Cardano.Crypto.VRF.Praos (PraosVRF)

import           Cardano.DbSync.Era.Shelley.Generic.Tx
import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.Ledger.Alonzo ()
import           Cardano.Ledger.Core (Witnesses)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (VRF)
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.SafeHash (SafeToHash)

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra, ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus

import           Ouroboros.Network.Block (BlockNo (..))

import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.OCert as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley


data BlockEra
  = Shelley
  | Allegra
  | Mary
  | Alonzo
  deriving (Eq, Show)

data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !ByteString
  , blkCreatorPoolHash :: !ByteString
  , blkSlotLeader :: !ByteString
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Shelley.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , blkTxs :: ![Tx]
  }


fromAllegraBlock :: ShelleyBlock StandardAllegra -> Block
fromAllegraBlock blk =
  Block
    { blkEra = Allegra
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromAllegraTx (blockTxs blk)
    }

fromShelleyBlock :: ShelleyBlock StandardShelley -> Block
fromShelleyBlock blk =
  Block
    { blkEra = Shelley
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromShelleyTx (blockTxs blk)
    }

fromMaryBlock :: ShelleyBlock StandardMary -> Block
fromMaryBlock blk =
  Block
    { blkEra = Mary
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map fromMaryTx (blockTxs blk)
    }

fromAlonzoBlock :: Ledger.PParams StandardAlonzo -> ShelleyBlock StandardAlonzo -> Block
fromAlonzoBlock pp blk =
  Block
    { blkEra = Alonzo
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkCreatorPoolHash = creatorPoolHash blk
    , blkSlotLeader = slotLeaderHash blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersion blk
    , blkVrfKey = blockVrfKeyView blk
    , blkOpCert = blockOpCert blk
    , blkOpCertCounter = blockOpCertCounter blk
    , blkTxs = map (fromAlonzoTx pp) (alonzoBlockTxs blk)
    }

-- -------------------------------------------------------------------------------------------------

alonzoBlockTxs :: ShelleyBlock StandardAlonzo -> [(Word64, Ledger.Tx StandardAlonzo)]
alonzoBlockTxs = zip [0 ..] . toList . fromTxSeq @StandardAlonzo . Shelley.bbody . Consensus.shelleyBlockRaw

blockBody :: ShelleyBasedEra era => ShelleyBlock era -> Shelley.BHBody (Crypto era)
blockBody = Shelley.bhbody . Shelley.bheader . Consensus.shelleyBlockRaw

blockHash :: ShelleyBlock era -> ByteString
blockHash =
  Crypto.hashToBytes . Shelley.unHashHeader
    . Consensus.unShelleyHash . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyBasedEra era => ShelleyBlock era -> BlockNo
blockNumber = Shelley.bheaderBlockNo . blockBody

blockPrevHash :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
blockPrevHash blk =
  case Shelley.bheaderPrev (Shelley.bhbody . Shelley.bheader $ Consensus.shelleyBlockRaw blk) of
    Shelley.GenesisHash -> "Cardano.DbSync.Era.Shelley.Generic.Block.blockPrevHash"
    Shelley.BlockHash h -> Crypto.hashToBytes (Shelley.unHashHeader h)

blockOpCert :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
blockOpCert = KES.rawSerialiseVerKeyKES . Shelley.ocertVkHot . Shelley.bheaderOCert . blockBody

blockOpCertCounter :: ShelleyBasedEra era => ShelleyBlock era -> Word64
blockOpCertCounter = Shelley.ocertN . Shelley.bheaderOCert . blockBody

blockProtoVersion :: ShelleyBasedEra era => ShelleyBlock era -> Shelley.ProtVer
blockProtoVersion = Shelley.bprotver . blockBody

blockSize :: ShelleyBasedEra era => ShelleyBlock era -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Shelley.bbody . Consensus.shelleyBlockRaw

blockTxs
    :: ( ShelleyBasedEra era
        , Ledger.TxSeq era ~ Shelley.TxSeq era
        , SafeToHash (Witnesses era)
        )
    => ShelleyBlock era -> [(Word64, Shelley.Tx era)]
blockTxs = zip [0 ..] . unTxSeq . Shelley.bbody . Consensus.shelleyBlockRaw

blockVrfKeyView :: (ShelleyBasedEra era, VRF (Crypto era) ~ PraosVRF) => ShelleyBlock era -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey . Shelley.bheaderVrfVk . blockBody

creatorPoolHash :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
creatorPoolHash = unKeyHashRaw . Shelley.issuerIDfromBHBody . blockBody

slotLeaderHash :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
slotLeaderHash = unKeyHashRaw . Shelley.issuerIDfromBHBody . blockBody

slotNumber :: ShelleyBasedEra era => ShelleyBlock era -> SlotNo
slotNumber = Shelley.bheaderSlotNo . blockBody

unTxSeq
    :: (ShelleyBasedEra era, SafeToHash (Witnesses era))
    => Shelley.TxSeq era -> [Shelley.Tx era]
unTxSeq (Shelley.TxSeq txSeq) = toList txSeq
