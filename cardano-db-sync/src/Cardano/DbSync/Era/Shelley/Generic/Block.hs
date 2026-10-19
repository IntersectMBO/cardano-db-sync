{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  fromDijkstraBlock,
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
import Lens.Micro ((^.))
import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import Data.Maybe.Strict (strictMaybeToMaybe)
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
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
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
  , blkTxs :: [Tx] -- intentionally left lazy to delay the tx transformation
  -- Leios (Dijkstra-only; False / Nothing for earlier eras)
  , blkHasLeiosCert :: !Bool
  , blkLeiosEbAnnouncement :: !(Maybe Ledger.EbReferencesAnnouncement)
  , blkLeiosCert :: !(Maybe LeiosCert)
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
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
    , blkHasLeiosCert = False
    , blkLeiosEbAnnouncement = Nothing
    , blkLeiosCert = Nothing
    }

fromDijkstraBlock :: Bool -> Maybe Prices -> ShelleyBlock (Praos2 StandardCrypto) DijkstraEra -> Block
fromDijkstraBlock iope mprices blk =
  Block
    { blkEra = Dijkstra
    , blkHash = blockHash blk
    , blkPreviousHash = blockPrevHash blk
    , blkSlotLeader = blockIssuer blk
    , blkSlotNo = slotNumber blk
    , blkBlockNo = blockNumber blk
    , blkSize = blockSize blk
    , blkProto = blockProtoVersionDijkstra blk
    , blkVrfKey = blockVrfKeyView $ blockVrfVkDijkstra blk
    , blkOpCert = blockOpCertKeyDijkstra blk
    , blkOpCertCounter = blockOpCertCounterDijkstra blk
    , blkTxs = map (fromDijkstraTx iope mprices) (getTxs blk)
    , blkHasLeiosCert = isJust leiosCert
    , blkLeiosEbAnnouncement = strictMaybeToMaybe (pHeaderLeiosEbAnnouncement (blockHeader blk))
    , blkLeiosCert = leiosCert
    }
  where
    leiosCert = strictMaybeToMaybe (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. leiosCertBlockBodyL)

-- -------------------------------------------------------------------------------------------------

getTxs :: forall p era. Ledger.EraBlockBody era => ShelleyBlock p era -> [(Word64, Ledger.Tx Ledger.TopTx era)]
getTxs blk = zip [0 ..] $ toList (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. Ledger.txSeqBlockBodyL)

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

-- -------------------------------------------------------------------------------------------------
-- Dijkstra runs the Praos2 protocol with the Leios block header (Cardano.Protocol.Leios.BlockHeader),
-- so its header accessors read that header rather than the classic Praos one used by Babbage/Conway.

blockOpCertKeyDijkstra :: ShelleyBlock (Praos2 StandardCrypto) era -> ByteString
blockOpCertKeyDijkstra = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertDijkstra

blockOpCertCounterDijkstra :: ShelleyBlock (Praos2 StandardCrypto) era -> Word64
blockOpCertCounterDijkstra = TPraos.ocertN . blockOpCertDijkstra

blockOpCertDijkstra :: ShelleyBlock (Praos2 StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertDijkstra = Leios.hbOCert . getHeaderBodyDijkstra . blockHeader

blockProtoVersionDijkstra :: ShelleyBlock (Praos2 StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionDijkstra = versionInfoToProtVer . Leios.hbVersionInfo . getHeaderBodyDijkstra . blockHeader

blockVrfVkDijkstra :: ShelleyBlock (Praos2 StandardCrypto) era -> VerKeyVRF (VRF StandardCrypto)
blockVrfVkDijkstra = Leios.hbVrfVk . getHeaderBodyDijkstra . blockHeader

getHeaderBodyDijkstra :: Leios.Header c -> Leios.HeaderBody c
getHeaderBodyDijkstra = Leios.headerBody

-- The Dijkstra/Leios header carries a 'BlockHeaderVersionInfo' rather than a 'ProtVer'.
-- It has the same wire shape, and consensus' own forging code builds it as
-- @BlockHeaderVersionInfo (getVersion32 major) minor@ from @ProtVer major minor@
-- (see Ouroboros.Consensus.Shelley.Protocol.Praos). We invert that here: the highest
-- supported major version is the ProtVer major, and the self-reported software tag is
-- the ProtVer minor.
versionInfoToProtVer :: Ledger.BlockHeaderVersionInfo -> Ledger.ProtVer
versionInfoToProtVer vi =
  Ledger.ProtVer
    (fromMaybe minBound (Ledger.mkVersion (Ledger.bhviHighestSupportedMajorVersion vi)))
    (fromIntegral (Ledger.bhviSelfReportedSoftwareTag vi))

blockIssuer ::
  ShelleyProtocol p =>
  ShelleyBlock p era ->
  KeyHash BlockIssuer
blockIssuer = hashKey . pHeaderIssuer . blockHeader

slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader
