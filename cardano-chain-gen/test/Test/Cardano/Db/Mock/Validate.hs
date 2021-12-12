{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Validate where

import           Cardano.Db
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Word (Word64)

import           Database.PostgreSQL.Simple (SqlError (..))
import           Database.Persist.Sql (SqlBackend)

import           Test.Cardano.Db.Mock.Config

import           Test.Tasty.HUnit

assertBlocksCount :: DBSyncEnv -> Word -> IO ()
assertBlocksCount env n = do
    assertEqBackoff env queryBlockCount n defaultDelays "Unexpected block count"

assertBlocksCountDetailed :: DBSyncEnv -> Word -> [Int] -> IO ()
assertBlocksCountDetailed env n delays = do
    assertEqBackoff env queryBlockCount n delays "Unexpected block count"

assertTxCount :: DBSyncEnv -> Word -> IO ()
assertTxCount env n = do
    assertEqBackoff env queryTxCount n defaultDelays "Unexpected tx count"

assertRewardCount :: DBSyncEnv -> Word64 -> IO ()
assertRewardCount env n =
    assertEqBackoff env queryRewardCount n defaultDelays "Unexpected rewards count"

assertBlockNoBackoff :: DBSyncEnv -> Word64 -> IO ()
assertBlockNoBackoff env blockNo =
    assertEqBackoff env queryBlockHeight (Just blockNo) defaultDelays "Unexpected BlockNo"

defaultDelays :: [Int]
defaultDelays = [1,2,4,8,16,32,64]

assertEqQuery :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> String -> IO ()
assertEqQuery env query a msg = do
    assertEqBackoff env query a [] msg

assertEqBackoff :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> [Int] -> String -> IO ()
assertEqBackoff env query a delays msg = do
    assertBackoff env query delays (== a) (\a' -> msg <> ": " <> show a' <> " /= " <> show a)

assertBackoff :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> [Int] -> (a -> Bool) -> (a -> String) -> IO ()
assertBackoff env query delays check errMsg = go delays
  where
    go ds = do
      q <- assertQuery env query check errMsg
      case (q, ds) of
        (Nothing, _) -> pure ()
        (Just err, []) -> assertFailure err
        (Just _err, dl : rest) -> do
          threadDelay $ dl * 100_000
          go rest

assertQuery :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> (a -> Bool) -> (a -> String) -> IO (Maybe String)
assertQuery env query check errMsg = do
  ma <- try $ queryDBSync env query
  case ma of
    Left sqlErr | migrationNotDoneYet (decodeUtf8 $ sqlErrorMsg sqlErr) -> pure $ Just $ show sqlErr
    Left err -> throwIO err
    Right a | not (check a) -> pure $ Just $ errMsg a
    _ -> pure Nothing

checkStillRuns :: DBSyncEnv -> IO ()
checkStillRuns env = do
    ret <- pollDBSync env
    case ret of
      Nothing -> pure ()
      Just (Right ()) -> throwIO $ userError "dbsync has stopped"
      Just (Left err) -> throwIO err

migrationNotDoneYet :: Text -> Bool
migrationNotDoneYet txt =
    Text.isPrefixOf "relation" txt && Text.isSuffixOf "does not exist" txt
