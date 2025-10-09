{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Error (
  DbCallStack (..),
  DbLookupError (..),
  DbSessionError (..),
  runOrThrowIODb,
  runOrThrowIO,
  logAndThrowIO,
  mkDbCallStack,
  mkDbLookupError,
  mkDbSessionError,
  formatSessionError,
  formatDbCallStack,
) where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Prelude (HasCallStack, MonadIO, SrcLoc (..), callStack, getCallStack, textShow, throwIO)
import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hasql.Session as HsqlSes

-- | Validation errors for expected business logic failures (e.g., "record not found")
data DbLookupError = DbLookupError
  { dbLookupErrCallStack :: !DbCallStack
  , dbLookupErrMsg :: !Text
  }
  deriving (Show, Eq)

instance Exception DbLookupError

-- | System errors for unexpected infrastructure failures (e.g., connection and query errors)
data DbSessionError = DbSessionError
  { dbSessionErrCallStack :: !DbCallStack
  , dbSessionErrMsg :: !Text
  }
  deriving (Show, Eq)

instance Exception DbSessionError

data DbCallStack = DbCallStack
  { dbCsFncName :: !String
  , dbCsModule :: !String
  , dbCsFile :: !String
  , dbCsLine :: !Int
  , dbCsCallChain :: ![(String, SrcLoc)]
  }
  deriving (Show, Eq)

runOrThrowIODb :: forall e a. Exception e => IO (Either e a) -> IO a
runOrThrowIODb ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a

runOrThrowIO :: forall e a m. MonadIO m => Exception e => m (Either e a) -> m a
runOrThrowIO ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a

logAndThrowIO :: Trace IO Text -> Text -> IO a
logAndThrowIO tracer msg = do
  logError tracer msg
  throwIO $ userError $ show msg

-- | Create a DbCallStack from the current call stack
mkDbCallStack :: HasCallStack => DbCallStack
mkDbCallStack =
  case getCallStack callStack of
    [] -> DbCallStack "unknown" "" "" 0 []
    -- Skip the first frame (which is always mkDbCallStack) and use the second frame
    (_ : rest) -> case rest of
      [] -> DbCallStack "unknown" "" "" 0 []
      ((callerName, callerLoc) : chainRest) ->
        DbCallStack
          { dbCsFncName = callerName -- Real calling function
          , dbCsModule = srcLocModule callerLoc
          , dbCsFile = srcLocFile callerLoc
          , dbCsLine = srcLocStartLine callerLoc
          , dbCsCallChain = take 4 chainRest -- Remaining call chain
          }

-- | Format a single frame with function name, module, and location
formatFrame :: String -> String -> String -> Int -> Text
formatFrame fnName moduleName fileName lineNum =
  "fn:" <> Text.pack fnName <> " md:" <> Text.pack moduleName <> " loc:" <> Text.pack fileName <> ":" <> textShow lineNum

-- | Format a DbCallStack for display in error messages
formatDbCallStack :: DbCallStack -> Text
formatDbCallStack cs =
  let mainFrame = formatFrame (dbCsFncName cs) (dbCsModule cs) (dbCsFile cs) (dbCsLine cs)
      chainFrames =
        map
          ( \(fnName, srcLoc) ->
              formatFrame fnName (srcLocModule srcLoc) (srcLocFile srcLoc) (srcLocStartLine srcLoc)
          )
          (dbCsCallChain cs)
   in if null chainFrames
        then "\n DB call chain: " <> mainFrame
        else "\n DB call chain: " <> mainFrame <> " <- " <> Text.intercalate " <- " chainFrames

-- | Convenience function to create DbLookupError with call stack
mkDbLookupError :: HasCallStack => Text -> DbLookupError
mkDbLookupError = DbLookupError mkDbCallStack

-- | Convenience function to create DbSessionError with call stack
mkDbSessionError :: HasCallStack => Text -> DbSessionError
mkDbSessionError = DbSessionError mkDbCallStack

-- | Format SessionError with ResultError first, then query details
formatSessionError :: HsqlSes.SessionError -> Text
formatSessionError sessionErr =
  case sessionErr of
    HsqlSes.QueryError sql params commandErr ->
      Text.pack (show commandErr) <> "\n QueryError " <> Text.pack (show sql) <> " " <> Text.pack (show params)
    HsqlSes.PipelineError commandErr ->
      Text.pack (show commandErr) <> "\n PipelineError"
