{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Byron.Util
  ( blockHash
  , blockMerkelRoot
  , blockNumber
  , blockPayload
  , blockPreviousHash
  , boundaryEpochNumber
  , configSlotDuration
  , epochNumber
  , genesisToHeaderHash
  , liftedLogException
  , logException
  , mkSlotLeader
  , pointToSlotHash
  , renderByteArray
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

import           Cardano.Prelude hiding (catch)

import           Cardano.Binary (Raw)
import           Cardano.BM.Trace (Trace, logError)
import qualified Cardano.Crypto as Crypto

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Slotting as Byron
import qualified Cardano.Chain.Update as Byron
import qualified Cardano.Chain.UTxO as Byron

import           Control.Exception.Lifted (SomeException, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Crypto.Hash (Blake2b_256)

import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Db as DB

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, ByronHash (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Network.Block (Point (..), SlotNo (..))


blockHash :: Byron.ABlock ByteString -> ByteString
blockHash = unHeaderHash . Byron.blockHashAnnotated

blockMerkelRoot :: Byron.ABlock ByteString -> Crypto.AbstractHash Blake2b_256 Raw
blockMerkelRoot =
  Byron.getMerkleRoot . Byron.txpRoot . Byron.recoverTxProof
    . Byron.bodyTxPayload . Byron.blockBody

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

genesisToHeaderHash :: Byron.GenesisHash -> Byron.HeaderHash
genesisToHeaderHash = coerce

-- | Needed when debugging disappearing exceptions.
liftedLogException :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
liftedLogException tracer txt action =
    action `catch` logger
  where
    logger :: MonadIO m => SomeException -> m a
    logger e =
      liftIO $ do
        putStrLn $ "Caught exception: txt " ++ show e
        logError tracer $ txt <> textShow e
        throwIO e

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action =
    action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e

mkSlotLeader :: Byron.ABlock ByteString -> DB.SlotLeader
mkSlotLeader blk =
  let slHash = slotLeaderHash blk
      slName = "SlotLeader-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
  in DB.SlotLeader slHash slName


-- | Convert from Ouroboros 'Point' to `Byron' types.
pointToSlotHash :: Point ByronBlock -> Maybe (Byron.SlotNumber, Byron.HeaderHash)
pointToSlotHash (Point x) =
  case x of
    Origin -> Nothing
    At blk -> Just (Byron.SlotNumber . unSlotNo $ Point.blockPointSlot blk, unByronHash $ Point.blockPointHash blk)

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

renderAbstractHash :: Crypto.AbstractHash algo a -> Text
renderAbstractHash =
    Text.decodeUtf8 . Base16.encode . Crypto.abstractHashToBytes

slotLeaderHash :: Byron.ABlock ByteString -> ByteString
slotLeaderHash =
  Crypto.abstractHashToBytes . Byron.addressHash . Byron.headerGenesisKey . Byron.blockHeader

slotNumber :: Byron.ABlock ByteString -> Word64
slotNumber =
  Byron.unSlotNumber . Byron.headerSlot . Byron.blockHeader

textShow :: Show a => a -> Text
textShow = Text.pack . show

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
