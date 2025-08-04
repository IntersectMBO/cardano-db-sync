{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Util (
  assertBool,
  deleteAllBlocks,
  dummyUTCTime,
  extractDbResult,
  mkAddressHash,
  mkBlock,
  mkBlockHash,
  mkTxHash,
  mkTxs,
  mkTxOutCore,
  testSlotLeader,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Data.Word (Word64)
import Text.Printf (printf)

import Cardano.Db
import Cardano.Db.Schema.Variants.TxOutCore (TxOutCore (..))

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg bool =
  liftIO $ unless bool (error msg)

extractDbResult :: MonadIO m => Either DbError a -> m a
extractDbResult (Left err) = liftIO $ throwIO err
extractDbResult (Right val) = pure val

deleteAllBlocks :: DbM ()
deleteAllBlocks = do
  result <- queryMinBlock
  case result of
    Nothing -> pure ()
    Just (blockId, word64) -> do
      deleteResult <- deleteBlocksForTests TxOutVariantCore blockId word64
      extractDbResult deleteResult

dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime (ModifiedJulianDay 0) 0

mkAddressHash :: BlockId -> TxId -> String
mkAddressHash blkId txId =
  take 28 $ printf "tx out #%d, tx #%d" (getBlockId blkId) (getTxId txId) ++ replicate 28 ' '

mkBlock :: Word64 -> SlotLeaderId -> Block
mkBlock blk slid =
  Block
    { blockHash = mkBlockHash blk
    , blockEpochNo = Just 0
    , blockSlotNo = Just 0
    , blockEpochSlotNo = Nothing
    , blockBlockNo = Nothing
    , blockPreviousId = Nothing
    , blockSlotLeaderId = slid
    , blockSize = 42
    , blockTime = dummyUTCTime
    , blockTxCount = 0
    , blockProtoMajor = 1
    , blockProtoMinor = 0
    , blockVrfKey = Nothing
    , blockOpCert = Nothing
    , blockOpCertCounter = Nothing
    }

mkBlockHash :: Word64 -> ByteString
mkBlockHash blkId =
  BS.pack (take 32 $ printf "block #%d" blkId ++ replicate 32 ' ')

mkTxHash :: BlockId -> Word64 -> ByteString
mkTxHash blk tx =
  BS.pack (take 32 $ printf "block #%d, tx #%d" (getBlockId blk) tx ++ replicate 32 ' ')

mkTxs :: BlockId -> Word -> [Tx]
mkTxs blkId count =
  take (fromIntegral count) $ map create [0 ..]
  where
    create w =
      Tx
        { txHash = mkTxHash blkId w
        , txBlockId = blkId
        , txBlockIndex = 0
        , txOutSum = DbLovelace 2
        , txFee = DbLovelace 1
        , txDeposit = Just 0
        , txSize = 12
        , txInvalidHereafter = Nothing
        , txInvalidBefore = Nothing
        , txValidContract = True
        , txScriptSize = 0
        , txTreasuryDonation = DbLovelace 0
        }

testSlotLeader :: SlotLeader
testSlotLeader =
  SlotLeader (BS.pack . take 28 $ "test slot leader" ++ replicate 28 ' ') Nothing "Dummy test slot leader"

mkTxOutCore :: BlockId -> TxId -> TxOutW
mkTxOutCore blkId txId =
  let addr = mkAddressHash blkId txId
   in VCTxOutW $
        TxOutCore
          { txOutCoreAddress = Text.pack addr
          , txOutCoreAddressHasScript = False
          , txOutCoreConsumedByTxId = Nothing
          , txOutCoreDataHash = Nothing
          , txOutCoreIndex = 0
          , txOutCoreInlineDatumId = Nothing
          , txOutCorePaymentCred = Nothing
          , txOutCoreReferenceScriptId = Nothing
          , txOutCoreStakeAddressId = Nothing
          , txOutCoreTxId = txId
          , txOutCoreValue = DbLovelace 1000000000
          }
