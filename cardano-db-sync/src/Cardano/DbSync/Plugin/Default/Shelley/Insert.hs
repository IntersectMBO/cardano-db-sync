{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Shelley.Insert
  ( insertShelleyBlock
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logInfo)
import qualified Cardano.Crypto as Crypto

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text

import           Ouroboros.Network.Block (BlockNo (..), Tip)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley


insertShelleyBlock
    :: Trace IO Text -> ShelleyBlock -> Tip ShelleyBlock
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertShelleyBlock tracer blk tip = do
  runExceptT $ do
    meta <- liftLookupFail "insertABlock" DB.queryMeta

    pbid <- liftLookupFail "insertABlock" $ DB.queryBlockId (Shelley.blockPrevHash blk)

    let slotsPerEpoch = DB.metaSlotsPerEpoch meta

    slid <- lift . DB.insertSlotLeader $ Shelley.mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Shelley.blockHash blk
                    , DB.blockEpochNo = Just $ Shelley.slotNumber blk `div` slotsPerEpoch
                    , DB.blockSlotNo = Just $ Shelley.slotNumber blk
                    , DB.blockBlockNo = Just $ Shelley.blockNumber blk
                    , DB.blockPrevious  = Just pbid
                    , DB.blockMerkelRoot = Nothing -- Shelley blocks do not have one.
                    , DB.blockSlotLeader = slid
                    , DB.blockSize = Shelley.blockSize blk
                    , DB.blockTime = DB.slotUtcTime meta (Shelley.slotNumber blk)
                    , DB.blockTxCount = Shelley.blockTxCount blk
                    }

    zipWithM_ (insertTx tracer blkId) [0 .. ] (Shelley.blockTxs blk)

    liftIO $ do
      let followingClosely = unBlockNo (tipBlockNo tip) - Shelley.blockNumber blk < 20
          (epoch, slotWithin) = Shelley.slotNumber blk `divMod` slotsPerEpoch
      when (followingClosely && slotWithin /= 0 && Shelley.slotNumber blk > 0 && Shelley.slotNumber blk  `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertShelleyBlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithin, ")"
            ]
      logger tracer $ mconcat
        [ "insertShelleyBlock: slot ", textShow (Shelley.slotNumber blk)
        , ", block ", textShow (Shelley.blockNumber blk)
        , ", hash ", renderByteArray (Shelley.blockHash blk)
        ]
  where
    logger :: Trace IO a -> a -> IO ()
    logger
      | unBlockNo (tipBlockNo tip) - Shelley.blockNumber blk < 20 = logInfo
      | Shelley.slotNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

-- -----------------------------------------------------------------------------

insertTx
    :: forall m. (MonadIO m)
    => Trace IO Text -> DB.BlockId -> Word64 -> ShelleyTx
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId blockIndex tx = do
    let txFee = calculateTxFee tx

    -- Insert transaction and get txId from the DB.
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Crypto.abstractHashToBytes $ Crypto.serializeCborHash tx
                , DB.txBlock = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = vfValue txFee
                , DB.txFee = vfFee txFee
                , DB.txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Shelley.txOutputList tx)

    -- Insert the transaction inputs.
    mapM_ (insertTxIn tracer txId) (Shelley.txInputList tx)


insertTxOut
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> (Word16, ShelleyTxOut)
    -> ExceptT e (ReaderT SqlBackend m) ()
insertTxOut _tracer txId (index, Shelley.TxOut addr value) =
  void . lift . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = index
              , DB.txOutAddress = Text.decodeUtf8 $ Base16.encode (Shelley.serialiseAddr addr)
              , DB.txOutValue = fromIntegral value
              }


insertTxIn
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> ShelleyTxIn
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Shelley.TxIn txId index) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId (Shelley.unTxHash txId)
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral index
              }

-------------------------------------------------------------------------------

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !Word64
  , vfFee :: !Word64
  }

calculateTxFee :: ShelleyTx -> ValueFee
calculateTxFee tx =
  ValueFee (Shelley.txOutputSum tx) (Shelley.txFee tx)
