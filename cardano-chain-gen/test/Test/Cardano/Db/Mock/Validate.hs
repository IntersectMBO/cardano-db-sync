{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Validate where

import           Cardano.Db
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Word (Word64)

import           Database.PostgreSQL.Simple (SqlError (..))
import           Database.Persist.Sql (SqlBackend)

import           Test.Tasty.HUnit

assertBlocksCount :: Word -> IO ()
assertBlocksCount n = do
    assertEqBackoff queryBlockCount n defaultDelays "Unexpected block count"

assertBlocksCountDetailed :: Word -> [Int] -> IO ()
assertBlocksCountDetailed n delays = do
    assertEqBackoff queryBlockCount n delays "Unexpected block count"

assertTxCount :: Word -> IO ()
assertTxCount n = do
    assertEqBackoff queryTxCount n defaultDelays "Unexpected tx count"

assertRewardCount :: Word64 -> IO ()
assertRewardCount n =
    assertEqBackoff queryRewardCount n defaultDelays "Unexpected rewards count"

assertBlockNoBackoff :: Word64 -> IO ()
assertBlockNoBackoff blockNo =
    assertEqBackoff queryBlockHeight (Just blockNo) defaultDelays "Unexpected BlockNo"

defaultDelays :: [Int]
defaultDelays = [1,2,4,8,16,32,64]

assertEqBackoff :: (Eq a, Show a) => ReaderT SqlBackend (NoLoggingT IO) a -> a -> [Int] -> String -> IO ()
assertEqBackoff query a delays msg = do
    assertBackoff query delays (== a) (\a' -> msg <> ": " <> show a' <> " /= " <> show a)

assertBackoff :: ReaderT SqlBackend (NoLoggingT IO) a -> [Int] -> (a -> Bool) -> (a -> String) -> IO ()
assertBackoff query delays check errMsg = go delays
  where
    go ds = do
      q <- assertQuery query check errMsg
      case (q, ds) of
        (Nothing, _) -> pure ()
        (Just err, []) -> assertFailure err
        (Just _err, dl : rest) -> do
          threadDelay $ dl * 100_000
          go rest

assertQuery :: ReaderT SqlBackend (NoLoggingT IO) a -> (a -> Bool) -> (a -> String) -> IO (Maybe String)
assertQuery query check errMsg = do
  ma <- try $ runDbNoLogging query
  case ma of
    Left sqlErr | migrationNotDoneYet (decodeUtf8 $ sqlErrorMsg sqlErr) -> pure $ Just $ show sqlErr
    Left err -> throwIO err
    Right a | not (check a) -> pure $ Just $ errMsg a
    _ -> pure Nothing

checkStillRuns :: Async () -> IO ()
checkStillRuns thread = do
    ret <- poll thread
    case ret of
      Nothing -> pure ()
      Just (Right ()) -> throwIO $ userError "dbsync has stopped"
      Just (Left err) -> throwIO err

migrationNotDoneYet :: Text -> Bool
migrationNotDoneYet txt =
    Text.isPrefixOf "relation" txt && Text.isSuffixOf "does not exist" txt
