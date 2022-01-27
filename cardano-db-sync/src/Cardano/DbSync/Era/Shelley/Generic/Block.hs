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

  , blockHash
  , slotLeaderHash
  ) where

import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES

import           Cardano.Crypto.VRF.Praos (PraosVRF)

import           Cardano.DbSync.Era.Shelley.Generic.Tx
import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.Ledger.Alonzo ()
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import           Cardano.Ledger.Core (Witnesses)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (VRF)
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.SafeHash (SafeToHash)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley

import           Cardano.Prelude

import qualified Cardano.Protocol.TPraos.BHeader as Protocol
import qualified Cardano.Protocol.TPraos.OCert as Protocol

import           Cardano.DbSync.Types
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardCrypto,
                   StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra, ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus

import           Ouroboros.Network.Block (BlockNo (..))


data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
  , blkCreatorPoolHash :: !ByteString
  , blkSlotLeader :: !ByteString
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Ledger.ProtVer
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
alonzoBlockTxs = zip [0 ..] . toList . fromTxSeq @StandardAlonzo . Ledger.bbody . Consensus.shelleyBlockRaw

blockBody :: ShelleyBasedEra era => ShelleyBlock era -> Protocol.BHBody (Crypto era)
blockBody = Protocol.bhbody . Ledger.bheader . Consensus.shelleyBlockRaw

blockHash :: ShelleyBlock era -> ByteString
blockHash =
  Crypto.hashToBytes . Protocol.unHashHeader
    . Consensus.unShelleyHash . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyBasedEra era => ShelleyBlock era -> BlockNo
blockNumber = Protocol.bheaderBlockNo . blockBody

blockPrevHash :: ShelleyBasedEra era => ShelleyBlock era -> Maybe ByteString
blockPrevHash blk =
  case Protocol.bheaderPrev (Protocol.bhbody . Ledger.bheader $ Consensus.shelleyBlockRaw blk) of
    Protocol.GenesisHash -> Nothing
    Protocol.BlockHash (Protocol.HashHeader h) -> Just $ Crypto.hashToBytes h

blockOpCert :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
blockOpCert = KES.rawSerialiseVerKeyKES . Protocol.ocertVkHot . Protocol.bheaderOCert . blockBody

blockOpCertCounter :: ShelleyBasedEra era => ShelleyBlock era -> Word64
blockOpCertCounter = Protocol.ocertN . Protocol.bheaderOCert . blockBody

blockProtoVersion :: ShelleyBasedEra era => ShelleyBlock era -> Ledger.ProtVer
blockProtoVersion = Protocol.bprotver . blockBody

blockSize :: ShelleyBasedEra era => ShelleyBlock era -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Ledger.bbody . Consensus.shelleyBlockRaw

blockTxs
    :: ( ShelleyBasedEra era
        , Ledger.TxSeq era ~ Shelley.TxSeq era
        , SafeToHash (Witnesses era)
        )
    => ShelleyBlock era -> [(Word64, Shelley.Tx era)]
blockTxs = zip [0 ..] . unTxSeq . Ledger.bbody . Consensus.shelleyBlockRaw

blockVrfKeyView :: (ShelleyBasedEra era, VRF (Crypto era) ~ PraosVRF) => ShelleyBlock era -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey . Protocol.bheaderVrfVk . blockBody

creatorPoolHash :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
creatorPoolHash = unKeyHashRaw . Protocol.issuerIDfromBHBody . blockBody

slotLeaderHash :: ShelleyBasedEra era => ShelleyBlock era -> ByteString
slotLeaderHash = unKeyHashRaw . Protocol.issuerIDfromBHBody . blockBody

slotNumber :: (Crypto era ~ StandardCrypto, ShelleyBasedEra era) => ShelleyBlock era -> SlotNo
slotNumber = Protocol.bheaderSlotNo . blockBody

unTxSeq
    :: (ShelleyBasedEra era, SafeToHash (Witnesses era))
    => Shelley.TxSeq era -> [Shelley.Tx era]
unTxSeq (Shelley.TxSeq txSeq) = toList txSeq
