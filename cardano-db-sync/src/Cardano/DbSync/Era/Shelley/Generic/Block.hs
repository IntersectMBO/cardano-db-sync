{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Block (
  Block (..),
  BlockEra (..),
  fromShelleyBlock,
  fromAllegraBlock,
  fromMaryBlock,
  fromAlonzoBlock,
  fromBabbageBlock,
  fromConwayBlock,
  getTxs,
  blockHash,
  blockPrevHash,
) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.VRF.Class (VerKeyVRF)
import Cardano.Crypto.VRF.Praos (PraosVRF)
import Cardano.DbSync.Era.Shelley.Generic.Tx
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Bech32 (serialiseVerKeyVrfToBech32)
import Cardano.Ledger.Alonzo ()
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), hashKey)
import Cardano.Prelude
import Cardano.Protocol.Crypto (Crypto, StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (
  AllegraEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  MaryEra,
  ShelleyEra,
 )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import Ouroboros.Consensus.Shelley.Protocol.Abstract
import Ouroboros.Network.Block (BlockNo (..))

data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
  , blkSlotLeader :: !(KeyHash 'BlockIssuer)
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Ledger.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , blkTxs :: [Tx] -- intentionally left lazy to delay the tx transformation
  }

fromAllegraBlock :: ShelleyBlock (TPraosStandard StandardCrypto) AllegraEra -> Block
fromAllegraBlock blk =
  Block
    { blkEra = Allegra
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionTPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkTPraos blk
    , blkOpCert = blockOpCertKeyTPraos blk
    , blkOpCertCounter = blockOpCertCounterTPraos blk
    , blkTxs = map fromAllegraTx (getTxs blk)
    }

fromShelleyBlock :: ShelleyBlock (TPraosStandard StandardCrypto) ShelleyEra -> Block
fromShelleyBlock blk =
  Block
    { blkEra = Shelley
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionTPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkTPraos blk
    , blkOpCert = blockOpCertKeyTPraos blk
    , blkOpCertCounter = blockOpCertCounterTPraos blk
    , blkTxs = map fromShelleyTx (getTxs blk)
    }

fromMaryBlock :: ShelleyBlock (TPraosStandard StandardCrypto) MaryEra -> Block
fromMaryBlock blk =
  Block
    { blkEra = Mary
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionTPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkTPraos blk
    , blkOpCert = blockOpCertKeyTPraos blk
    , blkOpCertCounter = blockOpCertCounterTPraos blk
    , blkTxs = map fromMaryTx (getTxs blk)
    }

fromAlonzoBlock :: Bool -> Maybe Prices -> ShelleyBlock (TPraosStandard StandardCrypto) AlonzoEra -> Block
fromAlonzoBlock iope mprices blk =
  Block
    { blkEra = Alonzo
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionTPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkTPraos blk
    , blkOpCert = blockOpCertKeyTPraos blk
    , blkOpCertCounter = blockOpCertCounterTPraos blk
    , blkTxs = map (fromAlonzoTx iope mprices) (getTxs blk)
    }

fromBabbageBlock :: Bool -> Maybe Prices -> ShelleyBlock (PraosStandard StandardCrypto) BabbageEra -> Block
fromBabbageBlock iope mprices blk =
  Block
    { blkEra = Babbage
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkPraos blk
    , blkOpCert = blockOpCertKeyPraos blk
    , blkOpCertCounter = blockOpCertCounterPraos blk
    , blkTxs = map (fromBabbageTx iope mprices) (getTxs blk)
    }

fromConwayBlock :: Bool -> Maybe Prices -> ShelleyBlock (PraosStandard StandardCrypto) ConwayEra -> Block
fromConwayBlock iope mprices blk =
  Block
    { blkEra = Conway
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionPraos blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkPraos blk
    , blkOpCert = blockOpCertKeyPraos blk
    , blkOpCertCounter = blockOpCertCounterPraos blk
    , blkTxs = map (fromConwayTx iope mprices) (getTxs blk)
    }

-- -------------------------------------------------------------------------------------------------

getTxs :: forall p era. Ledger.EraSegWits era => ShelleyBlock p era -> [(Word64, Ledger.Tx era)]
getTxs = zip [0 ..] . toList . Ledger.fromTxSeq @era . Ledger.bbody . Consensus.shelleyBlockRaw

blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.bheader . Consensus.shelleyBlockRaw

blockHash :: ShelleyBlock p era -> ByteString
blockHash =
  Crypto.hashToBytes
    . Consensus.unShelleyHash
    . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

blockPrevHash :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Maybe ByteString
blockPrevHash blk =
  case pHeaderPrevHash $ Ledger.bheader (Consensus.shelleyBlockRaw blk) of
    TPraos.GenesisHash -> Nothing
    TPraos.BlockHash (TPraos.HashHeader h) -> Just $ Crypto.hashToBytes h

blockOpCertKeyTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> ByteString
blockOpCertKeyTPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertTPraos

blockOpCertKeyPraos :: ShelleyBlock (PraosStandard StandardCrypto) era -> ByteString
blockOpCertKeyPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertPraos

blockOpCertCounterTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> Word64
blockOpCertCounterTPraos = TPraos.ocertN . blockOpCertTPraos

blockOpCertCounterPraos :: ShelleyBlock (PraosStandard StandardCrypto) era -> Word64
blockOpCertCounterPraos = TPraos.ocertN . blockOpCertPraos

blockOpCertTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertTPraos = TPraos.bheaderOCert . TPraos.bhbody . blockHeader

blockOpCertPraos :: ShelleyBlock (PraosStandard StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertPraos = Praos.hbOCert . getHeaderBodyPraos . blockHeader

blockProtoVersionTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionTPraos = TPraos.bprotver . TPraos.bhbody . blockHeader

blockProtoVersionPraos :: ShelleyBlock (PraosStandard StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionPraos = Praos.hbProtVer . getHeaderBodyPraos . blockHeader

blockSize :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Word64
blockSize = fromIntegral . pHeaderBlockSize . blockHeader

blockVrfKeyView :: VerKeyVRF (VRF StandardCrypto) -> Text
blockVrfKeyView = serialiseVerKeyVrfToBech32

blockVrfVkTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> VerKeyVRF PraosVRF
blockVrfVkTPraos = TPraos.bheaderVrfVk . TPraos.bhbody . blockHeader

blockVrfVkPraos :: ShelleyBlock (Praos StandardCrypto) era -> VerKeyVRF (VRF StandardCrypto)
blockVrfVkPraos = Praos.hbVrfVk . getHeaderBodyPraos . blockHeader

getHeaderBodyPraos :: Crypto c => Praos.Header c -> Praos.HeaderBody c
getHeaderBodyPraos (Praos.Header headerBody _) = headerBody

blockIssuer ::
  ShelleyProtocol p =>
  ShelleyBlock p era ->
  KeyHash 'BlockIssuer
blockIssuer = hashKey . pHeaderIssuer . blockHeader

slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader
