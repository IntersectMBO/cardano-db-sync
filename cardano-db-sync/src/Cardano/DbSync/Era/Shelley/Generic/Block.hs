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
  , fromBabbageBlock

  , blockHash
  , blockPrevHash
  ) where

import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES

import           Cardano.DbSync.Era.Shelley.Generic.Tx
import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.Ledger.Alonzo ()
import           Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import           Cardano.Ledger.Core (Witnesses)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (Crypto)
import           Cardano.Ledger.Era (SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Keys (VerKeyVRF, hashKey)
import           Cardano.Ledger.SafeHash (SafeToHash)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley

import           Cardano.Prelude

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos

import           Cardano.DbSync.Types
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardBabbage,
                   StandardMary, StandardShelley)
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra, ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Abstract

import           Ouroboros.Network.Block (BlockNo (..))


data Block = Block
  { blkEra :: !BlockEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
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


fromAllegraBlock :: ShelleyBlock (TPraos StandardCrypto) StandardAllegra -> Block
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

fromShelleyBlock :: ShelleyBlock (TPraos StandardCrypto) StandardShelley -> Block
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

fromMaryBlock :: ShelleyBlock (TPraos StandardCrypto) StandardMary -> Block
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

fromAlonzoBlock :: Prices -> ShelleyBlock (TPraos StandardCrypto) StandardAlonzo -> Block
fromAlonzoBlock prices blk =
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
    , blkTxs = map (fromAlonzoTx prices) (alonzoBlockTxs blk)
    }

fromBabbageBlock :: Prices -> ShelleyBlock (Praos StandardCrypto) StandardBabbage -> Block
fromBabbageBlock prices blk =
  Block
    { blkEra = Alonzo
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
    , blkTxs = map (fromBabbageTx prices) (babbageBlockTxs blk)
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
    . Consensus.unShelleyHash . Consensus.shelleyBlockHeaderHash

blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

blockPrevHash :: (ProtoCrypto p ~ StandardCrypto, ProtocolHeaderSupportsEnvelope p) => ShelleyBlock p era -> Maybe ByteString
blockPrevHash blk =
  case pHeaderPrevHash $ Ledger.bheader (Consensus.shelleyBlockRaw blk) of
    TPraos.GenesisHash -> Nothing
    TPraos.BlockHash (TPraos.HashHeader h) -> Just $ Crypto.hashToBytes h

blockOpCertKeyTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> ByteString
blockOpCertKeyTPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertTPraos

blockOpCertKeyPraos :: ShelleyBlock (Praos StandardCrypto) era -> ByteString
blockOpCertKeyPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertPraos

blockOpCertCounterTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Word64
blockOpCertCounterTPraos = TPraos.ocertN . blockOpCertTPraos

blockOpCertCounterPraos :: ShelleyBlock (Praos StandardCrypto) era -> Word64
blockOpCertCounterPraos = TPraos.ocertN . blockOpCertPraos

blockOpCertTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertTPraos = TPraos.bheaderOCert . TPraos.bhbody . blockHeader

blockOpCertPraos :: ShelleyBlock (Praos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertPraos = Praos.hbOCert . getHeaderBodyPraos . blockHeader

blockProtoVersionTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionTPraos = TPraos.bprotver . TPraos.bhbody . blockHeader

blockProtoVersionPraos :: ShelleyBlock (Praos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionPraos = Praos.hbProtVer . getHeaderBodyPraos . blockHeader

blockSize :: ShelleyBasedEra era => ShelleyBlock p era -> Word64
blockSize = fromIntegral . Shelley.bBodySize . Ledger.bbody . Consensus.shelleyBlockRaw

blockTxs
    :: ( ShelleyBasedEra era
        , Ledger.TxSeq era ~ Shelley.TxSeq era
        , SafeToHash (Witnesses era)
        )
    => ShelleyBlock p era -> [(Word64, Shelley.Tx era)]
blockTxs = zip [0 ..] . unTxSeq . Ledger.bbody . Consensus.shelleyBlockRaw

-- (ShelleyBasedEra era, VRF (Crypto era) ~ PraosVRF) =>
blockVrfKeyView :: VerKeyVRF StandardCrypto -> Text
blockVrfKeyView = Api.serialiseToBech32 . Api.VrfVerificationKey

blockVrfVkTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> VerKeyVRF StandardCrypto
blockVrfVkTPraos = TPraos.bheaderVrfVk . TPraos.bhbody . blockHeader

blockVrfVkPraos :: ShelleyBlock (Praos StandardCrypto) era -> VerKeyVRF StandardCrypto
blockVrfVkPraos = Praos.hbVrfVk . getHeaderBodyPraos . blockHeader

getHeaderBodyPraos :: Crypto c => Praos.Header c -> Praos.HeaderBody c
getHeaderBodyPraos (Praos.Header headerBody _) = headerBody

blockIssuer :: (ShelleyProtocol p, Crypto (ProtoCrypto p)) => ShelleyBlock p era -> ByteString
blockIssuer = unKeyHashRaw . hashKey . pHeaderIssuer . blockHeader

slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader

unTxSeq
    :: (ShelleyBasedEra era, SafeToHash (Witnesses era))
    => Shelley.TxSeq era -> [Shelley.Tx era]
unTxSeq (Shelley.TxSeq txSeq) = toList txSeq
