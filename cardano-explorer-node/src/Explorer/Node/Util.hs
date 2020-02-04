{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node.Util
  ( blockHash
  , blockMerkelRoot
  , blockNumber
  , blockPayload
  , blockPreviousHash
  , boundaryEpochNumber
  , configSlotDuration
  , genesisToHeaderHash
  , mkSlotLeader
  , pointToSlotHash
  , renderAbstractHash
  , slotLeaderHash
  , slotNumber
  , textShow
  , unAbstractHash
  , unAddressHash
  , unCryptoHash
  , unHeaderHash
  , unTxHash
  ) where

import           Cardano.Prelude

import           Cardano.Binary (Raw)
import qualified Cardano.Crypto as Crypto

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Explorer DB functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Common as Ledger
import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.Slotting as Ledger
import qualified Cardano.Chain.Update as Ledger
import qualified Cardano.Chain.UTxO as Ledger

import           Crypto.Hash (Blake2b_256)

import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Explorer.DB as DB

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock, ByronHash (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Network.Block (Point (..), SlotNo (..))


blockHash :: Ledger.ABlock ByteString -> ByteString
blockHash = unHeaderHash . Ledger.blockHashAnnotated

blockMerkelRoot :: Ledger.ABlock ByteString -> Crypto.AbstractHash Blake2b_256 Raw
blockMerkelRoot =
  Ledger.getMerkleRoot . Ledger.txpRoot . Ledger.recoverTxProof
    . Ledger.bodyTxPayload . Ledger.blockBody

boundaryEpochNumber :: Ledger.ABoundaryBlock ByteString -> Word64
boundaryEpochNumber = Ledger.boundaryEpoch . Ledger.boundaryHeader

blockNumber :: Ledger.ABlock ByteString -> Word64
blockNumber =
  Ledger.unChainDifficulty . Ledger.headerDifficulty . Ledger.blockHeader

blockPayload :: Ledger.ABlock a -> [Ledger.TxAux]
blockPayload =
  Ledger.unTxPayload . Ledger.bodyTxPayload . Ledger.blockBody

blockPreviousHash :: Ledger.ABlock a -> Ledger.HeaderHash
blockPreviousHash = Ledger.headerPrevHash . Ledger.blockHeader

configSlotDuration :: Ledger.Config -> Word64
configSlotDuration =
  fromIntegral . Ledger.ppSlotDuration . Ledger.gdProtocolParameters . Ledger.configGenesisData

genesisToHeaderHash :: Ledger.GenesisHash -> Ledger.HeaderHash
genesisToHeaderHash = coerce

mkSlotLeader :: Ledger.ABlock ByteString -> DB.SlotLeader
mkSlotLeader blk =
  let slHash = slotLeaderHash blk
      slName = "SlotLeader-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
  in DB.SlotLeader slHash slName


-- | Convert from Ouroboros 'Point' to `Ledger' types.
pointToSlotHash :: Point ByronBlock -> Maybe (Ledger.SlotNumber, Ledger.HeaderHash)
pointToSlotHash (Point x) =
  case x of
    Origin -> Nothing
    At blk -> Just (Ledger.SlotNumber . unSlotNo $ Point.blockPointSlot blk, unByronHash $ Point.blockPointHash blk)

renderAbstractHash :: ByteArrayAccess bin => bin -> Text
renderAbstractHash =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

slotLeaderHash :: Ledger.ABlock ByteString -> ByteString
slotLeaderHash =
  Data.ByteArray.convert . Ledger.addressHash . Ledger.headerGenesisKey . Ledger.blockHeader

slotNumber :: Ledger.ABlock ByteString -> Word64
slotNumber =
  Ledger.unSlotNumber . Ledger.headerSlot . Ledger.blockHeader

textShow :: Show a => a -> Text
textShow = Text.pack . show

unAbstractHash :: Crypto.Hash Raw -> ByteString
unAbstractHash = Data.ByteArray.convert

unAddressHash :: Ledger.AddressHash Ledger.Address' -> ByteString
unAddressHash = Data.ByteArray.convert

unHeaderHash :: Ledger.HeaderHash -> ByteString
unHeaderHash = Data.ByteArray.convert

unTxHash :: Crypto.Hash Ledger.Tx -> ByteString
unTxHash = Data.ByteArray.convert

unCryptoHash :: Crypto.Hash Raw -> ByteString
unCryptoHash = Data.ByteArray.convert
