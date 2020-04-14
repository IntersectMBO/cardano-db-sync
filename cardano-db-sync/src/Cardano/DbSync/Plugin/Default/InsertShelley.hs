{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.InsertShelley
  ( insertShelleyBlock
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import qualified Cardano.Crypto as Crypto

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util (textShow, renderByteArray)

import qualified Shelley.Spec.Ledger.BlockChain as SL
import           Shelley.Spec.Ledger.Tx as SL

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..), Crypto)
import           Ouroboros.Network.Block (BlockNo (..), Tip, SlotNo (..), getTipBlockNo)
import           Ouroboros.Network.Point (withOrigin)


mkSlotLeader :: Crypto c => SL.Block c -> DB.SlotLeader
mkSlotLeader blk =
  let slHash = Crypto.abstractHashToBytes . Crypto.hash . SL.bheaderVk . SL.bhbody . SL.bheader $ blk
      slName = "SlotLeader-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
  in DB.SlotLeader slHash slName

insertShelleyBlock
    :: Crypto c
    => Trace IO Text
    -> ShelleyBlock c
    -> Tip (ShelleyBlock c)
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertShelleyBlock tracer blk tip = do
  runExceptT $ do
    let block = shelleyBlockRaw blk
    insertAShelleyBlock tracer block tip

insertAShelleyBlock
    :: (Crypto c, MonadIO m)
    => Trace IO Text -> SL.Block c -> Tip (ShelleyBlock c)
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertAShelleyBlock tracer blk tip = do
    meta <- liftLookupFail "insertABlock" DB.queryMeta

    let blockId = show . SL.unHashHeader . SL.bhHash . SL.bheader $ blk
    pbid <- liftLookupFail "insertABlock" $ DB.queryBlockId blockId

    let slotsPerEpoch = 10 * DB.metaProtocolConst meta

    let blockHash :: ByteString
        blockHash = Crypto.abstractHashToBytes . Crypto.hash . SL.bhash . SL.bhbody . SL.bheader $ blk

    let blockHeaderSize :: Int
        blockHeaderSize = SL.bHeaderSize . SL.bheader $ blk

    let getTxInternalUnsafe :: SL.TxSeq c -> Seq (SL.Tx c)
        getTxInternalUnsafe (SL.TxSeq txSeq) = txSeq

    let txsCount :: Int
        txsCount = length . getTxInternalUnsafe . SL.bbody $ blk

    slid <- lift . DB.insertSlotLeader $ mkSlotLeader blk
    _blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash       = blockHash
                    , DB.blockEpochNo    = Just $ slotNumber `div` slotsPerEpoch
                    , DB.blockSlotNo     = Just $ slotNumber
                    , DB.blockBlockNo    = Just $ blockNumber
                    , DB.blockPrevious   = Just pbid
                    , DB.blockMerkelRoot = Nothing -- Not sure how to translate this.
                    -- Just $ unCryptoHash (blockMerkelRoot blk)
                    , DB.blockSlotLeader = slid
                    , DB.blockSize       = fromIntegral blockHeaderSize
                    , DB.blockTime       = DB.slotUtcTime meta slotNumber
                    , DB.blockTxCount    = fromIntegral txsCount
                    }

    -- TODO(KS): Insert the transaction is MISSING!
    --mapMVExceptT (insertTx tracer blkId) $ blockPayload blk

    liftIO $ do
      let followingClosely = withOrigin 0 unBlockNo (getTipBlockNo tip) - blockNumber < 20
          (epoch, slotWithin) = slotNumber `divMod` slotsPerEpoch
      when (followingClosely && slotWithin /= 0 && slotNumber > 0 && slotNumber `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertABlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithin, ")"
            ]
      logger tracer $ mconcat
        [ "insertABlock: slot ", textShow slotNumber
        , ", block ", textShow blockNumber
        , ", hash ", renderByteArray blockHash
        ]
  where

    slotNumber :: Word64
    slotNumber = unSlotNo . SL.bheaderSlotNo . SL.bhbody . SL.bheader $ blk

    blockNumber :: Word64
    blockNumber = unBlockNo . SL.bheaderBlockNo . SL.bhbody . SL.bheader $ blk

    logger :: Trace IO a -> a -> IO ()
    logger
      | withOrigin 0 unBlockNo (getTipBlockNo tip) - blockNumber < 20 = logInfo
      | slotNumber `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

