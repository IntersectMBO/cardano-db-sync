{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Util (
  assertBool,
  deleteAllBlocks,
  dummyUTCTime,
  mkAddressHash,
  mkBlock,
  mkBlockHash,
  mkTxHash,
  mkTxs,
  mkTxOutCore,
  testSlotLeader,
) where

import Cardano.Db
import qualified Cardano.Db.Schema.Core.TxOut as C
import Control.Monad (unless)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Data.Word (Word64)
import Database.Persist.Sql (SqlBackend)
import Text.Printf (printf)

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg bool =
  liftIO $ unless bool (error msg)

deleteAllBlocks :: MonadIO m => ReaderT SqlBackend m ()
deleteAllBlocks = do
  mblkId <- queryMinBlock
  whenJust mblkId $ uncurry (deleteBlocksForTests TxOutCore)

dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime (ModifiedJulianDay 0) 0

mkAddressHash :: BlockId -> TxId -> String
mkAddressHash blkId txId =
  take 28 $ printf "tx out #%d, tx #%d" (unBlockId blkId) (unTxId txId) ++ replicate 28 ' '

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
  BS.pack (take 32 $ printf "block #%d, tx #%d" (unBlockId blk) tx ++ replicate 32 ' ')

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
   in CTxOutW $
        C.TxOut
          { C.txOutAddress = Text.pack addr
          , C.txOutAddressHasScript = False
          , C.txOutConsumedByTxId = Nothing
          , C.txOutDataHash = Nothing
          , C.txOutIndex = 0
          , C.txOutInlineDatumId = Nothing
          , C.txOutPaymentCred = Nothing
          , C.txOutReferenceScriptId = Nothing
          , C.txOutStakeAddressId = Nothing
          , C.txOutTxId = txId
          , C.txOutValue = DbLovelace 1000000000
          }
