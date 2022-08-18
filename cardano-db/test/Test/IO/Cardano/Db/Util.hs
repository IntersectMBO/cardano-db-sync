{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Util
  ( assertBool
  , dummyUTCTime
  , mkAddressHash
  , mkBlock
  , mkBlockHash
  , mkTxHash
  , mkTxs
  , mkTxOut
  , testSlotLeader
  , unBlockId
  , unTxId
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Int (Int64)
import qualified Data.Text as Text
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock (UTCTime (..))
import           Data.Word (Word64)

import           Cardano.Db

import           Text.Printf (printf)


assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg bool =
  liftIO $ unless bool (error msg)

dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime (ModifiedJulianDay 0) 0

mkAddressHash :: Int64 -> TxId -> String
mkAddressHash blkNo txId =
  take 28 $ printf "tx out #%d, tx #%d" blkNo (unTxId txId) ++ replicate 28 ' '

mkBlock :: Int64 -> SlotLeaderId -> Block
mkBlock blkNo slid =
  Block
    { blockHash = mkBlockHash blkNo
    , blockEpochNo = Just 0
    , blockSlotNo = Just 0
    , blockEpochSlotNo = Nothing
    , blockBlockNo = fromIntegral blkNo
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


mkBlockHash :: Int64 -> ByteString
mkBlockHash blkNo =
  BS.pack (take 32 $ printf "block #%d" blkNo ++ replicate 32 ' ')

mkTxHash :: Int64 -> Word64 -> ByteString
mkTxHash blkNo tx =
  BS.pack (take 32 $ printf "block #%d, tx #%d" blkNo tx ++ replicate 32 ' ')

mkTxs :: Int64 -> Word -> [Tx]
mkTxs blkNo count =
    take (fromIntegral count) $ map create [ 0 .. ]
  where
    create w =
      Tx
        { txHash = mkTxHash blkNo w
        , txBlockNo = blkNo
        , txBlockIndex = 0
        , txOutSum = DbLovelace 2
        , txFee = DbLovelace 1
        , txDeposit = 0
        , txSize = 12
        , txInvalidHereafter = Nothing
        , txInvalidBefore = Nothing
        , txValidContract = True
        , txScriptSize = 0
        }

testSlotLeader :: SlotLeader
testSlotLeader =
  SlotLeader (BS.pack . take 28 $ "test slot leader" ++ replicate 28 ' ') Nothing "Dummy test slot leader"

mkTxOut :: Int64 -> TxId -> TxOut
mkTxOut blkNo txId =
  let addr = mkAddressHash blkNo txId in
  TxOut txId 0 (Text.pack addr) (BS.pack addr) False Nothing Nothing (DbLovelace 1000000000) Nothing Nothing Nothing blkNo
