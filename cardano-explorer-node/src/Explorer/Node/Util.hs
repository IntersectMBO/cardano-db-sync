{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Explorer.Node.Util
  ( blockHash
  , blockMerkelRoot
  , blockNumber
  , blockPayload
  , blockPreviousHash
  , boundaryEpochNumber
  , genesisToHeaderHash
  , leftPanic
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
import qualified Cardano.Chain.UTxO as Ledger

import           Crypto.Hash (Blake2b_256)

import           Data.Coerce (coerce)
import qualified Data.ByteArray
import qualified Data.Text as Text

import qualified Explorer.DB as DB


blockHash :: Ledger.ABlock ByteString -> Ledger.HeaderHash
blockHash = Ledger.blockHashAnnotated

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

genesisToHeaderHash :: Ledger.GenesisHash -> Ledger.HeaderHash
genesisToHeaderHash = coerce

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

leftPanic :: Text -> Either DB.LookupFail a -> a
leftPanic msg =
  \case
    Left err -> panic $ msg <> DB.renderLookupFail err
    Right val -> val
