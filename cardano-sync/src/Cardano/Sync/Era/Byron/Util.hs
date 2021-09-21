{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync.Era.Byron.Util
  ( blockHash
  , blockNumber
  , blockPayload
  , blockPreviousHash
  , boundaryEpochNumber
  , configSlotDuration
  , epochNumber
  , genesisToHeaderHash
  , protocolVersion
  , renderAbstractHash
  , slotLeaderHash
  , slotNumber
  , unAbstractHash
  , unAddressHash
  , unCryptoHash
  , unHeaderHash
  , unTxHash
  ) where

import           Cardano.Prelude hiding (catch)

import           Cardano.Binary (Raw)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Wallet as Crypto

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Slotting as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text


blockHash :: Byron.ABlock ByteString -> ByteString
blockHash = unHeaderHash . Byron.blockHashAnnotated

boundaryEpochNumber :: Byron.ABoundaryBlock ByteString -> Word64
boundaryEpochNumber = Byron.boundaryEpoch . Byron.boundaryHeader

blockNumber :: Byron.ABlock ByteString -> Word64
blockNumber =
  Byron.unChainDifficulty . Byron.headerDifficulty . Byron.blockHeader

blockPayload :: Byron.ABlock a -> [Byron.TxAux]
blockPayload =
  Byron.unTxPayload . Byron.bodyTxPayload . Byron.blockBody

blockPreviousHash :: Byron.ABlock a -> Byron.HeaderHash
blockPreviousHash = Byron.headerPrevHash . Byron.blockHeader

configSlotDuration :: Byron.Config -> Word64
configSlotDuration =
  fromIntegral . Byron.ppSlotDuration . Byron.gdProtocolParameters . Byron.configGenesisData

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

slotLeaderHash :: Byron.ABlock ByteString -> ByteString
slotLeaderHash =
  BS.take 28
    . Crypto.abstractHashToBytes . Crypto.hashRaw .LBS.fromStrict . Crypto.xpubPublicKey
    . Crypto.unVerificationKey . Byron.headerGenesisKey . Byron.blockHeader

slotNumber :: Byron.ABlock ByteString -> Word64
slotNumber =
  Byron.unSlotNumber . Byron.headerSlot . Byron.blockHeader

unAbstractHash :: Crypto.Hash Raw -> ByteString
unAbstractHash = Crypto.abstractHashToBytes

unAddressHash :: Byron.AddressHash Byron.Address' -> ByteString
unAddressHash = Crypto.abstractHashToBytes

unHeaderHash :: Byron.HeaderHash -> ByteString
unHeaderHash = Crypto.abstractHashToBytes

unTxHash :: Crypto.Hash Byron.Tx -> ByteString
unTxHash = Crypto.abstractHashToBytes

unCryptoHash :: Crypto.Hash Raw -> ByteString
unCryptoHash = Crypto.abstractHashToBytes
