{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Explorer.DB.TotalSupply
  ( tests
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, deleteCascade, selectKeysList, unSqlBackendKey)

import           Explorer.DB

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)
-- import           Test.HUnit.Base (assertBool)

import           Text.Printf (printf)

tests :: TestTree
tests =
  testGroup "TotalSupply"
    [ testCase "Initial supply correct" initialSupplyTest
    ]


initialSupplyTest :: IO ()
initialSupplyTest =
  runDbNoLogging $ do
    -- Delete the blocks if they exist.
    deleteAllBlocksCascade

    -- Set up initial supply.
    bid0 <- both <$> insertBlock (mkBlock 0)
    (tx0Ids :: [TxId]) <- mapM (fmap2 both insertTx) $ mkTxs bid0 4
    _ <- mapM (fmap2 both insertTxOut) $ map (mkTxOut bid0) tx0Ids
    count <- queryBlockCount
    assertBool "Block count should be 1" (count == 1)
    supply0 <- queryTotalSupply
    assertBool "Total supply should not be > 0" (supply0 > 0)

    -- Spend from the Utxo set.
    bid1 <- both <$> insertBlock (mkBlock 1)
    tx1Id <- both <$> insertTx (Tx (mkTxHash bid1 1) bid1 500000000)
    _ <- both <$> insertTxIn (TxIn tx1Id (head tx0Ids) 0)
    _ <- insertTxOut $ TxOut tx1Id 0 (mkAddressHash bid1 tx1Id) 500000000
    supply1 <- queryTotalSupply
    assertBool ("Total supply should be < " ++ show supply0) (supply1 < supply0)


-- Could use 'assertBool' from HUNit, but need 'MonadIO'.
assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg bool =
  liftIO $ unless bool (error msg)

mkBlock :: Word64 -> Block
mkBlock blk =
  Block (mkBlockHash blk) Nothing 0 Nothing Nothing 42

mkTxs :: BlockId -> Word -> [Tx]
mkTxs blkId count =
    take (fromIntegral count) $ map create [ 0 .. ]
  where
    create w = Tx (mkTxHash blkId w) blkId 1

mkTxOut :: BlockId -> TxId -> TxOut
mkTxOut blkId txId =
  TxOut txId 0 (mkAddressHash blkId txId) 1000000000

mkBlockHash :: Word64 -> ByteString
mkBlockHash blk =
  BS.pack (take 32 $ printf "block #%d" blk ++ replicate 32 ' ')

mkTxHash :: BlockId -> Word64 -> ByteString
mkTxHash blk tx =
  BS.pack (take 32 $ printf "block #%d, tx #%d" (unBlockId blk) tx ++ replicate 32 ' ')

mkAddressHash :: BlockId -> TxId -> ByteString
mkAddressHash blkId txId =
  BS.pack (take 28 $ printf "tx out #%d, tx #%d" (unBlockId blkId) (unTxId txId) ++ replicate 28 ' ')

both :: Either a a -> a
both (Left a) = a
both (Right a) = a

-- Surely there is a better way!
unBlockId :: BlockId -> Word64
unBlockId = fromIntegral . unSqlBackendKey . unBlockKey

unTxId :: TxId -> Word64
unTxId = fromIntegral . unSqlBackendKey . unTxKey

deleteAllBlocksCascade :: MonadIO m => ReaderT SqlBackend m ()
deleteAllBlocksCascade = do
  (keys :: [BlockId]) <- selectKeysList [] []
  mapM_ deleteCascade keys

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
