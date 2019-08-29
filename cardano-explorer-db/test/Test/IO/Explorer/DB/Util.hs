{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Explorer.DB.Util
  ( assertBool
  , deleteAllBlocksCascade
  , mkAddressHash
  , mkBlock
  , mkBlockHash
  , mkMerkelRoot
  , mkTxHash
  , mkTxs
  , mkTxOut
  , unBlockId
  , unTxId
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, deleteCascade, selectKeysList, unSqlBackendKey)

import           Explorer.DB

import           Text.Printf (printf)


assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg bool =
  liftIO $ unless bool (error msg)

deleteAllBlocksCascade :: MonadIO m => ReaderT SqlBackend m ()
deleteAllBlocksCascade = do
  (keys :: [BlockId]) <- selectKeysList [] []
  mapM_ deleteCascade keys

mkAddressHash :: BlockId -> TxId -> Text
mkAddressHash blkId txId =
  Text.pack (take 28 $ printf "tx out #%d, tx #%d" (unBlockId blkId) (unTxId txId) ++ replicate 28 ' ')

mkBlock :: Word64 -> Block
mkBlock blk =
  Block (mkBlockHash blk) Nothing 0 Nothing Nothing 42

mkBlockHash :: Word64 -> ByteString
mkBlockHash blkId =
  BS.pack (take 32 $ printf "block #%d" blkId ++ replicate 32 ' ')

mkMerkelRoot :: Word64 -> ByteString
mkMerkelRoot blkId =
  BS.pack (take 32 $ printf "merkel root #%d" blkId ++ replicate 32 ' ')

mkTxHash :: BlockId -> Word64 -> ByteString
mkTxHash blk tx =
  BS.pack (take 32 $ printf "block #%d, tx #%d" (unBlockId blk) tx ++ replicate 32 ' ')

mkTxs :: BlockId -> Word -> [Tx]
mkTxs blkId count =
    take (fromIntegral count) $ map create [ 0 .. ]
  where
    create w = Tx (mkTxHash blkId w) blkId 1

mkTxOut :: BlockId -> TxId -> TxOut
mkTxOut blkId txId =
  TxOut txId 0 (mkAddressHash blkId txId) 1000000000

unBlockId :: BlockId -> Word64
unBlockId = fromIntegral . unSqlBackendKey . unBlockKey

unTxId :: TxId -> Word64
unTxId = fromIntegral . unSqlBackendKey . unTxKey
