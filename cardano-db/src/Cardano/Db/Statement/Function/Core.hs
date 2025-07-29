{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Statement.Function.Core (
  runDbSession,
  runDbSessionMain,
  runDbSessionPool,
  mkDbCallStack,
  bulkEncoder,
  ResultType (..),
  ResultTypeBulk (..),
)
where

import Cardano.BM.Trace (logInfo)
import Cardano.Db.Error (DbCallStack (..), DbError (..))
import Cardano.Db.Types (ConnectionType (..), DbAction (..), DbEnv (..))
import Cardano.Prelude (MonadIO (..), Text, ask, throwIO)
import Data.Pool (withResource)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS

-- | Runs a database session (regular or pipelined) with automatic error handling and optional logging.
--
-- This function executes a `Session` within the `DbAction` monad, providing automatic error
-- propagation via MonadError constraints. It captures timing information and call site details
-- for debugging purposes when logging is active in the `DbEnv`.
--
-- Database errors are automatically propagated using the MonadError constraint, allowing for
-- clean error composition using do-notation without manual `Either` handling.
--
-- This is the core function for executing both regular and pipelined database operations
-- with automatic error propagation.
--
-- ==== Parameters
-- * @DbCallStack@: Call site information for debugging and logging.
-- * @Session a@: The `Hasql` session to execute (can be a regular session or pipeline).
--
-- ==== Returns
-- * @DbAction m a@: The result with automatic error propagation via MonadError.
--
-- ==== Examples
-- ```
-- -- Regular session with automatic error handling:
-- result <- runDbSessionMain (mkDbCallStack "operation") $
--   HsqlS.statement record statement
--
-- -- Pipeline session with automatic error handling:
-- results <- runDbSessionMain (mkDbCallStack "batchOperation") $
--   HsqlS.pipeline $ do
--     r1 <- HsqlP.statement input1 statement1
--     r2 <- HsqlP.statement input2 statement2
--     pure (r1, r2)
--
-- -- Usage in a function that chains multiple database operations:
-- myFunction :: MonadIO m => DbAction m Result
-- myFunction = do
--   result1 <- runDbSessionMain (mkDbCallStack "query1") session1
--   result2 <- runDbSessionMain (mkDbCallStack "query2") session2
--   pure $ combineResults result1 result2
-- ```
--
-- ==== Error Handling
-- Database errors are automatically caught and propagated via the MonadError constraint.
-- If any session fails, the error is thrown using `throwError` and propagates up the stack.
runDbSession :: MonadIO m => ConnectionType -> DbCallStack -> HsqlS.Session a -> DbAction m a
runDbSession connType dbCallStack@DbCallStack {..} session = DbAction $ do
  dbEnv <- ask
  let locationInfo =
        " Function: "
          <> dbCsFncName
          <> " at "
          <> dbCsModule
          <> ":"
          <> dbCsFile
          <> ":"
          <> Text.pack (show dbCsLine)
          <> if null dbCsCallChain
            then ""
            else "\n  Call chain: " <> Text.intercalate "\n    <- " dbCsCallChain

  case dbTracer dbEnv of
    Nothing -> run dbEnv
    Just tracer -> do
      start <- liftIO getCurrentTime
      result <- run dbEnv
      end <- liftIO getCurrentTime
      let duration = diffUTCTime end start
      let connTypeText = case connType of
            UseMainConnection -> "Main"
            UsePoolConnection -> "Pool"
      liftIO $ logInfo tracer $ connTypeText <> " Query: " <> dbCsFncName <> locationInfo <> " in " <> Text.pack (show duration)
      pure result
  where
    run dbEnv = do
      result <- case connType of
        UseMainConnection ->
          liftIO $ HsqlS.run session (dbConnection dbEnv)
        UsePoolConnection ->
          liftIO $ withResource (dbPoolConnection dbEnv) $ \conn ->
            HsqlS.run session conn
      case result of
        Left sessionErr -> do
          let errorMsg = case connType of
                UseMainConnection -> "Main database query failed"
                UsePoolConnection -> "Pool database query failed"
          liftIO $ throwIO $ DbError dbCallStack errorMsg (Just sessionErr)
        Right val -> pure val

-- | Convenience function for main connection operations (backward compatibility)
runDbSessionMain :: MonadIO m => DbCallStack -> HsqlS.Session a -> DbAction m a
runDbSessionMain = runDbSession UseMainConnection

-- | Convenience function for pool connection operations
runDbSessionPool :: MonadIO m => DbCallStack -> HsqlS.Session a -> DbAction m a
runDbSessionPool = runDbSession UsePoolConnection

-- | Extracts call site information from the current call stack.
--
-- This helper function parses the Haskell call stack to provide source location
-- details for the last 5 function calls, giving better debugging context.
--
-- ==== Returns
-- * @DbCallStack@: A record containing module name, file path, line number and call chain
mkDbCallStack :: HasCallStack => Text -> DbCallStack
mkDbCallStack name =
  case getCallStack callStack of
    [] -> DbCallStack name "Unknown" "Unknown" 0 []
    ((_, loc) : rest) ->
      DbCallStack
        { dbCsFncName = name
        , dbCsModule = Text.pack (srcLocModule loc)
        , dbCsFile = Text.pack (srcLocFile loc)
        , dbCsLine = srcLocStartLine loc
        , dbCsCallChain = take 5 $ map formatFrame rest -- Take next 5 frames
        }
  where
    formatFrame (fnName, srcLoc) =
      Text.pack fnName
        <> " at "
        <> Text.pack (srcLocModule srcLoc)
        <> ":"
        <> Text.pack (srcLocFile srcLoc)
        <> ":"
        <> Text.pack (show (srcLocStartLine srcLoc))

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultType c r where
  NoResult :: ResultType c () -- No ID, result type is ()
  WithResult :: HsqlD.Result c -> ResultType c c -- Return ID, result type is c

-- | The result type of an insert operation (usualy it's newly generated id).
-- data ResultTypeBulk c r where
--   NoResultBulk :: ResultTypeBulk c () -- No IDs, result type is ()
--   WithResultBulk :: HsqlD.Result [c] -> ResultTypeBulk c [c] -- Return IDs, result type is [c]

-- | The bulk insert result type
data ResultTypeBulk a where
  NoResultBulk :: ResultTypeBulk () -- No results returned
  WithResultBulk :: HsqlD.Result [a] -> ResultTypeBulk [a] -- Return generated IDs

-- | Creates a parameter encoder for an array of values from a single-value encoder
bulkEncoder :: HsqlE.NullableOrNot HsqlE.Value a -> HsqlE.Params [a]
bulkEncoder v = HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray v
