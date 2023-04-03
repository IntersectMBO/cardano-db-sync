{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  blockHash,
  blockPrevHash,
  alonzoBlockTxs,
  babbageBlockTxs,
) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.DbSync.Era.Shelley.Generic.Tx
import Cardano.DbSync.Types
import Cardano.Ledger.Alonzo ()
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Era (EraSegWits (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), VerKeyVRF, hashKey)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import Cardano.Prelude
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (
  StandardAllegra,
  StandardAlonzo,
  StandardBabbage,
  StandardMary,
  StandardShelley,
 )
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra, ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import Ouroboros.Consensus.Shelley.Protocol.Abstract
import Ouroboros.Network.Block (BlockNo (..))

data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
  , blkSlotLeader :: !(KeyHash 'BlockIssuer StandardCrypto)
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Ledger.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , blkTxs :: [Tx] -- intentionally left lazy to delay the tx transformation
  }

fromAllegraBlock :: ShelleyBlock TPraosStandard StandardAllegra -> Block
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
    , blkTxs = map fromAllegraTx (blockTxs blk)
    }

fromShelleyBlock :: ShelleyBlock TPraosStandard StandardShelley -> Block
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
    , blkTxs = map fromShelleyTx (blockTxs blk)
    }

fromMaryBlock :: ShelleyBlock TPraosStandard StandardMary -> Block
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
    , blkTxs = map fromMaryTx (blockTxs blk)
    }

fromAlonzoBlock :: Bool -> Maybe Prices -> ShelleyBlock TPraosStandard StandardAlonzo -> Block
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
    , blkTxs = map (fromAlonzoTx iope mprices) (alonzoBlockTxs blk)
    }

fromBabbageBlock :: Bool -> Maybe Prices -> ShelleyBlock PraosStandard StandardBabbage -> Block
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
    , blkTxs = map (fromBabbageTx iope mprices) (babbageBlockTxs blk)
    }

-- -------------------------------------------------------------------------------------------------

babbageBlockTxs :: ShelleyBlock p StandardBabbage -> [(Word64, Ledger.Tx StandardBabbage)]
babbageBlockTxs = zip [0 ..] . toList . fromTxSeq @StandardBabbage . Ledger.bbody . Consensus.shelleyBlockRaw

alonzoBlockTxs :: ShelleyBlock p StandardAlonzo -> [(Word64, Ledger.Tx StandardAlonzo)]
alonzoBlockTxs = zip [0 ..] . toList . fromTxSeq @StandardAlonzo . Ledger.bbody . Consensus.shelleyBlockRaw

blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.bheader . Consensus.shelleyBlockRaw

blockHash :: (ProtoCrypto p ~ StandardCrypto) => ShelleyBlock p era -> ByteString
blockHash =
  Crypto.hashToBytes -- . Protocol.unHashHeader
    . Consensus.unShelleyHash
    . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

blockPrevHash :: (ProtoCrypto p ~ StandardCrypto, ProtocolHeaderSupportsEnvelope p) => ShelleyBlock p era -> Maybe ByteString
blockPrevHash blk =
  case pHeaderPrevHash $ Ledger.bheader (Consensus.shelleyBlockRaw blk) of
    TPraos.GenesisHash -> Nothing
    TPraos.BlockHash (TPraos.HashHeader h) -> Just $ Crypto.hashToBytes h

blockOpCertKeyTPraos :: ShelleyBlock TPraosStandard era -> ByteString
blockOpCertKeyTPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertTPraos

blockOpCertKeyPraos :: ShelleyBlock PraosStandard era -> ByteString
blockOpCertKeyPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertPraos

blockOpCertCounterTPraos :: ShelleyBlock TPraosStandard era -> Word64
blockOpCertCounterTPraos = TPraos.ocertN . blockOpCertTPraos

blockOpCertCounterPraos :: ShelleyBlock PraosStandard era -> Word64
blockOpCertCounterPraos = TPraos.ocertN . blockOpCertPraos

blockOpCertTPraos :: ShelleyBlock TPraosStandard era -> TPraos.OCert StandardCrypto
blockOpCertTPraos = TPraos.bheaderOCert . TPraos.bhbody . blockHeader

blockOpCertPraos :: ShelleyBlock PraosStandard era -> TPraos.OCert StandardCrypto
blockOpCertPraos = Praos.hbOCert . getHeaderBodyPraos . blockHeader

blockProtoVersionTPraos :: ShelleyBlock TPraosStandard era -> Ledger.ProtVer
blockProtoVersionTPraos = TPraos.bprotver . TPraos.bhbody . blockHeader

blockProtoVersionPraos :: ShelleyBlock PraosStandard era -> Ledger.ProtVer
blockProtoVersionPraos = Praos.hbProtVer . getHeaderBodyPraos . blockHeader

blockSize :: ShelleyBasedEra era => ShelleyBlock p era -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Ledger.bbody . Consensus.shelleyBlockRaw

blockTxs ::
  ( ShelleyBasedEra era
  , Ledger.TxSeq era ~ Shelley.ShelleyTxSeq era
  , Ledger.Tx era ~ Shelley.ShelleyTx era
  ) =>
  ShelleyBlock p era ->
  [(Word64, Shelley.ShelleyTx era)]
blockTxs = zip [0 ..] . unTxSeq . Ledger.bbody . Consensus.shelleyBlockRaw

blockVrfKeyView :: VerKeyVRF StandardCrypto -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey

blockVrfVkTPraos :: ShelleyBlock TPraosStandard era -> VerKeyVRF StandardCrypto
blockVrfVkTPraos = TPraos.bheaderVrfVk . TPraos.bhbody . blockHeader

blockVrfVkPraos :: ShelleyBlock PraosStandard era -> VerKeyVRF StandardCrypto
blockVrfVkPraos = Praos.hbVrfVk . getHeaderBodyPraos . blockHeader

getHeaderBodyPraos :: Crypto c => Praos.Header c -> Praos.HeaderBody c
getHeaderBodyPraos (Praos.Header headerBody _) = headerBody

blockIssuer ::
  (ShelleyProtocol p, Crypto (ProtoCrypto p), ProtoCrypto p ~ crypto) =>
  ShelleyBlock p era ->
  KeyHash 'BlockIssuer crypto
blockIssuer = hashKey . pHeaderIssuer . blockHeader

slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader

unTxSeq ::
  ( ShelleyBasedEra era
  , Ledger.TxSeq era ~ Shelley.ShelleyTxSeq era
  , Ledger.Tx era ~ Shelley.ShelleyTx era
  ) =>
  Shelley.ShelleyTxSeq era ->
  [Shelley.ShelleyTx era]
unTxSeq = toList . Ledger.fromTxSeq
