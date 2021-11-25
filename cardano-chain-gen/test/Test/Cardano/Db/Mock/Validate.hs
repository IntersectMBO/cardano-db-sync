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

assertBlockNoBackoff :: Word64 -> IO ()
assertBlockNoBackoff blockNo =
    assertEqBackoff queryBlockHeight blockNo "Unexpected BlockNo"

assertEqBackoff :: (Eq a, Show a) => ReaderT SqlBackend (NoLoggingT IO) a -> a -> String -> IO ()
assertEqBackoff query a msg = do
    assertBackoff query (== a) (\a' -> msg <> ": " <> show a' <> " /= " <> show a)

assertBackoff :: ReaderT SqlBackend (NoLoggingT IO) a -> (a -> Bool) -> (a -> String) -> IO ()
assertBackoff query check errMsg = go delays
  where
    go ds = do
      q <- assertQuery query check errMsg
      case (q, ds) of
        (Nothing, _) -> pure ()
        (Just err, []) -> assertFailure err
        (Just _err, dl : rest) -> do
          threadDelay $ dl * 100_000
          go rest

    delays = [1,2,4,8,16,32,64]

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
