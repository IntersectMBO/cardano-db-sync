{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Block (
  Block (..),
  LeiosCertificate (..),
  BlockEra (..),
  fromShelleyBlock,
  fromAllegraBlock,
  fromMaryBlock,
  fromAlonzoBlock,
  fromBabbageBlock,
  fromConwayBlock,
  fromDijkstraBlock,
  getTxs,
  blockHash,
  blockPrevHash,
) where

import Cardano.Binary.FixedSizeCodec (rawEncodeFixedSized)
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Crypto.Leios (BitField (..), LeiosCert (..), leiosSignatureToBytes)
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
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraEraBlockBody (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), hashKey)
import Cardano.Prelude
import Cardano.Protocol.Crypto (Crypto, StandardCrypto, VRF)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Array.Byte (ByteArray)
import qualified Data.ByteString as BS
import Data.Primitive.ByteArray (indexByteArray, sizeofByteArray)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (
  AllegraEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  DijkstraEra,
  MaryEra,
  ShelleyEra,
 )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import Ouroboros.Consensus.Shelley.Protocol.Abstract
import Ouroboros.Network.Block (BlockNo (..))

data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
  , blkSlotLeader :: !(KeyHash BlockIssuer)
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Ledger.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , -- The Leios certificate in the block body, if any; its presence is what makes
    -- this block a certifying block. Always 'Nothing' before the Dijkstra era.
    blkLeiosCert :: !(Maybe LeiosCertificate)
  , blkTxs :: [Tx] -- intentionally left lazy to delay the tx transformation
  }

-- | The contents of a Dijkstra Leios certificate, as stored.
data LeiosCertificate = LeiosCertificate
  { lcSigners :: !ByteString
  -- ^ Bitfield over committee seats, MSB-first; bit @i@ set iff seat @i@ signed.
  , lcSignature :: !ByteString
  -- ^ The aggregate BLS signature.
  , lcSignerCount :: !Word64
  -- ^ How many seats signed, ie the population count of 'lcSigners'.
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
    , blkLeiosCert = Nothing
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
    , blkLeiosCert = Nothing
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
    , blkLeiosCert = Nothing
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
    , blkLeiosCert = Nothing
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
    , blkLeiosCert = Nothing
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
    , blkLeiosCert = Nothing
    , blkTxs = map (fromConwayTx iope mprices) (getTxs blk)
    }

fromDijkstraBlock :: Bool -> Maybe Prices -> ShelleyBlock (PraosStandard StandardCrypto) DijkstraEra -> Block
fromDijkstraBlock iope mprices blk =
  Block
    { blkEra = Dijkstra
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
    , blkLeiosCert = getLeiosCert blk
    , blkTxs = map (fromDijkstraTx iope mprices) (getTxs blk)
    }

-- -------------------------------------------------------------------------------------------------

getTxs :: forall p era. Ledger.EraBlockBody era => ShelleyBlock p era -> [(Word64, Ledger.Tx Ledger.TopTx era)]
getTxs blk = zip [0 ..] $ toList (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. Ledger.txSeqBlockBodyL)

-- | The Leios certificate in the block body, if the body carries one. Only a Dijkstra
-- body has a place to put one; the certificate is what makes the block a certifying block.
getLeiosCert :: DijkstraEraBlockBody era => ShelleyBlock p era -> Maybe LeiosCertificate
getLeiosCert blk =
  fromLeiosCert
    <$> Ledger.strictMaybeToMaybe (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. leiosCertBlockBodyL)

-- | A Leios certificate holds the aggregate BLS signature and the bitfield saying which
-- committee seats contributed to it. The bitfield is @ceil(committeeSize / 8)@ bytes,
-- MSB-first, with bit @i@ set iff seat @i@ signed, so the number of signing seats is just
-- its population count.
fromLeiosCert :: LeiosCert -> LeiosCertificate
fromLeiosCert cert =
  LeiosCertificate
    { lcSigners = signers
    , lcSignature = leiosSignatureToBytes (leiosCertSignature cert)
    , lcSignerCount = fromIntegral $ sum (map popCount (BS.unpack signers))
    }
  where
    signers = byteArrayToByteString . bitFieldBytes $ leiosCertSigners cert

    byteArrayToByteString :: ByteArray -> ByteString
    byteArrayToByteString ba = BS.pack [indexByteArray ba i | i <- [0 .. sizeofByteArray ba - 1]]

blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.blockHeader . Consensus.shelleyBlockRaw

blockHash :: ShelleyBlock p era -> ByteString
blockHash =
  Crypto.hashToBytes
    . Consensus.unShelleyHash
    . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

blockPrevHash :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Maybe ByteString
blockPrevHash blk =
  case pHeaderPrevHash $ Ledger.blockHeader (Consensus.shelleyBlockRaw blk) of
    TPraos.GenesisHash -> Nothing
    TPraos.BlockHash (TPraos.HashHeader h) -> Just $ Crypto.hashToBytes h

blockOpCertKeyTPraos :: ShelleyBlock (TPraosStandard StandardCrypto) era -> ByteString
blockOpCertKeyTPraos = rawEncodeFixedSized . TPraos.ocertVkHot . blockOpCertTPraos

blockOpCertKeyPraos :: ShelleyBlock (PraosStandard StandardCrypto) era -> ByteString
blockOpCertKeyPraos = rawEncodeFixedSized . TPraos.ocertVkHot . blockOpCertPraos

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
  KeyHash BlockIssuer
blockIssuer = hashKey . pHeaderIssuer . blockHeader

slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader
