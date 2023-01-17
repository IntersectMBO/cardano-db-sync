{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Byron.Util (
  boundaryEpochNumber,
  configSlotDuration,
  mkSlotLeader,
  slotLeaderHash,
  unAbstractHash,
  unAddressHash,
  unCryptoHash,
  blockHash,
  blockNumber,
  blockPayload,
  blockPreviousHash,
  ebbPrevHash,
  prevHash,
  epochNumber,
  genesisToHeaderHash,
  protocolVersion,
  renderAbstractHash,
  slotNumber,
  unHeaderHash,
  unTxHash,
) where

import Cardano.Binary (Raw)

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Slotting as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Cardano.Db as DB
import Cardano.Prelude hiding (catch)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron

boundaryEpochNumber :: Byron.ABoundaryBlock ByteString -> Word64
boundaryEpochNumber = Byron.boundaryEpoch . Byron.boundaryHeader

configSlotDuration :: Byron.Config -> Word64
configSlotDuration =
  fromIntegral . Byron.ppSlotDuration . Byron.gdProtocolParameters . Byron.configGenesisData

mkSlotLeader :: Byron.ABlock ByteString -> DB.SlotLeader
mkSlotLeader blk =
  let slHash = slotLeaderHash blk
      slName = "ByronGenesisKey-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
   in -- On Byrom poolHashId will always be Nothing.
      DB.SlotLeader slHash Nothing slName

slotLeaderHash :: Byron.ABlock ByteString -> ByteString
slotLeaderHash =
  BS.take 28
    . Crypto.abstractHashToBytes
    . Crypto.hashRaw
    . LBS.fromStrict
    . Crypto.xpubPublicKey
    . Crypto.unVerificationKey
    . Byron.headerGenesisKey
    . Byron.blockHeader

unAbstractHash :: Crypto.Hash Raw -> ByteString
unAbstractHash = Crypto.abstractHashToBytes

unAddressHash :: Byron.AddressHash Byron.Address' -> ByteString
unAddressHash = Crypto.abstractHashToBytes

unCryptoHash :: Crypto.Hash Raw -> ByteString
unCryptoHash = Crypto.abstractHashToBytes

blockHash :: Byron.ABlock ByteString -> ByteString
blockHash = unHeaderHash . Byron.blockHashAnnotated

blockNumber :: Byron.ABlock ByteString -> Word64
blockNumber =
  Byron.unChainDifficulty . Byron.headerDifficulty . Byron.blockHeader

blockPayload :: Byron.ABlock a -> [Byron.TxAux]
blockPayload =
  Byron.unTxPayload . Byron.bodyTxPayload . Byron.blockBody

blockPreviousHash :: Byron.ABlock a -> ByteString
blockPreviousHash = unHeaderHash . Byron.headerPrevHash . Byron.blockHeader

ebbPrevHash :: Byron.ABoundaryBlock a -> ByteString
ebbPrevHash bblock =
  case Byron.boundaryPrevHash (Byron.boundaryHeader bblock) of
    Left gh -> genesisToHeaderHash gh
    Right hh -> unHeaderHash hh

prevHash :: Byron.ByronBlock -> ByteString
prevHash blk = case Byron.byronBlockRaw blk of
  Byron.ABOBBlock ablk -> blockPreviousHash ablk
  Byron.ABOBBoundary abblk -> ebbPrevHash abblk

epochNumber :: Byron.ABlock ByteString -> Word64 -> Word64
epochNumber blk slotsPerEpoch =
  slotNumber blk `div` slotsPerEpoch

genesisToHeaderHash :: Byron.GenesisHash -> ByteString
genesisToHeaderHash = unAbstractHash . Byron.unGenesisHash

protocolVersion :: Byron.ABlock ByteString -> Byron.ProtocolVersion
protocolVersion = Byron.headerProtocolVersion . Byron.blockHeader

renderAbstractHash :: Crypto.AbstractHash algo a -> Text
renderAbstractHash =
  Text.decodeUtf8 . Base16.encode . Crypto.abstractHashToBytes

slotNumber :: Byron.ABlock ByteString -> Word64
slotNumber =
  Byron.unSlotNumber . Byron.headerSlot . Byron.blockHeader

unHeaderHash :: Byron.HeaderHash -> ByteString
unHeaderHash = Crypto.abstractHashToBytes

unTxHash :: Crypto.Hash Byron.Tx -> ByteString
unTxHash = Crypto.abstractHashToBytes
