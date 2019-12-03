{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-error=orphans #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.IO.Explorer.Web.Query where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Logger (LoggingT, runLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Set (fromList, empty)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word (Word16, Word64)
import           Database.Persist.Sql (PersistValue, IsolationLevel (..), SqlBackend,
                    runSqlConnWithIsolation)
import           Database.Persist.Postgresql (rawExecute, withPostgresqlConn)

import           Explorer.DB
import           Explorer.Web.Query

import           Test.IO.Explorer.DB.Util (assertBool, dummyUTCTime, mkBlockHash, testSlotLeader)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)


tests :: TestTree
tests =
  testGroup "Web.Query"
    [ testCase "case 1" testCase1
    , testCase "empty utxo with empty db" testEmptyUtxo
    ]

loggingMode :: MonadIO m => LoggingT m a -> m a
loggingMode =
    if False
      then runStdoutLoggingT
      else flip runLoggingT $ \ _ _ _ _ -> pure ()

dropAndRemakeDb :: IO PGConfig
dropAndRemakeDb = do
  pgconfig' <- readPGPassFileEnv
  let
    pgconfig = pgconfig' { pgcDbname = (pgcDbname pgconfig') <> "-tests" }
    pgconfigExternal = pgconfig' { pgcDbname = "postgres" }
    --debug :: MonadLogger m => String -> m ()
    --debug = monadLoggerLog defaultLoc "" LevelDebug
    -- ran outside a transaction
    runExternalSql :: Text -> [PersistValue] -> IO ()
    runExternalSql sql params =
      loggingMode . withPostgresqlConn (toConnectionString pgconfigExternal) $ \backend ->
        flip runReaderT backend $
          rawExecute sql params
  putStrLn "dropping old test db"
  runExternalSql ("DROP DATABASE IF EXISTS \"" <> (decodeUtf8 $ pgcDbname pgconfig) <> "\"") []
  putStrLn "remaking db"
  runExternalSql ("CREATE DATABASE \"" <> (decodeUtf8 $ pgcDbname pgconfig) <> "\"") []
  putStrLn "doing test"
  runMigrations (\oldcfg -> oldcfg { pgcDbname = pgcDbname pgconfig }) True (MigrationDir "../schema") (LogFileDir "/tmp")
  pure pgconfig

dropAndRemakeDbThenTest :: (SqlBackend -> LoggingT IO ()) -> IO ()
dropAndRemakeDbThenTest action = do
  pgconfig <- dropAndRemakeDb
  print pgconfig
  loggingMode . withPostgresqlConn (toConnectionString pgconfig) $ action


testEmptyUtxo :: IO ()
testEmptyUtxo = do
  dropAndRemakeDbThenTest $ \backend -> runSqlAction backend $ do
    slid <- insertSlotLeader testSlotLeader
    bid0 <- insertBlock (blockZero slid)
    snapshot <- queryUtxoSnapshot bid0
    liftIO $ print snapshot
    assertBool "snapshot should be empty" (snapshot == [])

testCase1 :: IO ()
testCase1 = do
  dropAndRemakeDbThenTest $ \backend -> do
    (slid, bid0) <- runSqlAction backend $ do
      slid <- insertSlotLeader testSlotLeader
      bid0 <- insertBlock $ blockZero slid
      pure (slid, bid0)

    snapshot00 <- runSqlAction backend $ do
      snapshot00 <- fromList <$> queryUtxoSnapshot bid0
      assertBool "utxo must be empty when no outputs exist" (snapshot00 == empty)
      pure snapshot00

    (bid1, expected1, out1, tx0, tx0id) <- runSqlAction backend $ do
      bid1 <- insertBlock $ mkBlock 1 slid bid0
      let tx0 = mkTx 0 bid1
      tx0id <- insertTx tx0
      let
        out0 = mkOut tx0id 0 "tx0 out0" 123
        out1 = mkOut tx0id 1 "tx0 out1" 123
        expected1 = fromList
          [ (out0, txHash tx0)
          , (out1, txHash tx0)
          ]
      mapM_ insertTxOut [ out0, out1 ]
      pure (bid1, expected1, out1, tx0, tx0id)

    snapshot10 <- runSqlAction backend $ do
      snapshot01 <- fromList <$> queryUtxoSnapshot bid0
      assertBool "snapshot at point 0 must not change when inserting new blocks" (snapshot00 == snapshot01)
      snapshot10 <- fromList <$> queryUtxoSnapshot bid1
      liftIO $ do
        print snapshot10
        print expected1
      assertBool "snapshot at point 1 should be expected value" (snapshot10 == expected1)
      pure snapshot10

    (bid2, tx1, out2, expected2) <- runSqlAction backend $ do
      bid2 <- insertBlock $ mkBlock 2 slid bid1
      let tx1 = mkTx 1 bid2
      tx1id <- insertTx tx1
      let
        out2 = mkOut tx1id 0 "tx1 out0" 123
        expected2 = fromList
          [ (out1, txHash tx0)
          , (out2, txHash tx1)
          ]
      _ <- insertTxIn $ mkIn tx1id (tx0id, 0)
      _ <- insertTxOut out2
      pure (bid2, tx1, out2, expected2)

    (snapshot20) <- runSqlAction backend $ do
      snapshot02 <- fromList <$> queryUtxoSnapshot bid0
      snapshot11 <- fromList <$> queryUtxoSnapshot bid1
      snapshot20 <- fromList <$> queryUtxoSnapshot bid2
      assertBool "snapshot at point 0 must not change when inserting new blocks" (snapshot00 == snapshot02)
      assertBool "snapshot at point 1 must not change when inserting new blocks" (snapshot10 == snapshot11)
      assertBool "snapshot at point 2 should be expected value" (snapshot20 == expected2)
      pure (snapshot20)

    (bid3, expected3) <- runSqlAction backend $ do
      bid3 <- insertBlock $ mkBlock 3 slid bid2
      let tx2 = mkTx 2 bid3
      tx2id <- insertTx tx2
      let
        out3 = mkOut tx2id 0 "tx2 out0" 123
        expected3 = fromList
          [ (out1, txHash tx0)
          , (out2, txHash tx1)
          , (out3, txHash tx2)
          ]
      _ <- insertTxOut out3
      pure (bid3, expected3)

    runSqlAction backend $ do
      snapshot03 <- fromList <$> queryUtxoSnapshot bid0
      snapshot12 <- fromList <$> queryUtxoSnapshot bid1
      snapshot21 <- fromList <$> queryUtxoSnapshot bid2
      snapshot30 <- fromList <$> queryUtxoSnapshot bid3
      assertBool "snapshot at point 0 must not change when inserting new blocks" (snapshot00 == snapshot03)
      assertBool "snapshot at point 1 must not change when inserting new blocks" (snapshot10 == snapshot12)
      assertBool "snapshot at point 2 must not change when inserting new blocks" (snapshot20 == snapshot21)
      assertBool "snapshot at point 3 should be expected value" (snapshot30 == expected3)

deriving instance Show TxOut
deriving instance Eq TxOut
deriving instance Ord TxOut

runSqlAction :: MonadUnliftIO m => SqlBackend -> ReaderT SqlBackend m a -> m a
runSqlAction backend action =
  runSqlConnWithIsolation action backend Serializable

blockZero :: SlotLeaderId -> Block
blockZero slid =
  Block (mkHash '\0') Nothing Nothing Nothing Nothing Nothing slid 0 dummyUTCTime 0

mkHash :: Char -> ByteString
mkHash = BS.pack . replicate 32

mkBlock :: Word64 -> SlotLeaderId -> BlockId -> Block
mkBlock blk slid previous =
  Block (mkBlockHash blk) Nothing Nothing (Just blk) (Just previous) Nothing slid 0 dummyUTCTime 0

-- TODO, make a `mkTxHash`, so the tx hashes dont claim `block #0`
mkTx :: Word64 -> BlockId -> Tx
mkTx txnum block = Tx (mkBlockHash txnum) block 0 0 12

mkOut :: TxId -> Word16 -> Text -> Word64 -> TxOut
mkOut txid index addr value = TxOut txid index addr value

mkIn :: TxId -- the tx spending this input
  -> (TxId, Word16) -- the index of, an output, and the tx to find it in
  -> TxIn
mkIn parent (outtx, outidx) = TxIn parent outtx outidx
