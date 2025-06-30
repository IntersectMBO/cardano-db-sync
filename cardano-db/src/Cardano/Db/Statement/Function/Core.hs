{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Statement.Function.Core (
  runDbSession,
  mkDbCallStack,
  bulkEncoder,
  ResultType (..),
  ResultTypeBulk (..),
)
where

import Cardano.BM.Trace (logInfo)
import Cardano.Db.Error (DbCallStack (..), DbError (..))
import Cardano.Db.Types (DbAction (..), DbEnv (..))
import Cardano.Prelude (MonadError (..), MonadIO (..), Text, ask, for_, when)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS

-- | Runs a database session (regular or pipelined) with optional logging.
--
-- This function executes a `Session` within the `DbAction` monad, handling
-- the execution and logging details if enabled in the `DbEnv`. It captures
-- timing information and call site details for debugging purposes when logging
-- is active.
--
-- This is the core function for executing both regular and pipelined database
-- operations.
--
-- ==== Parameters
-- * @DbCallStack@: Call site information for debugging and logging.
-- * @Session a@: The `Hasql` session to execute (can be a regular session or pipeline).
--
-- ==== Returns
-- * @DbAction m a@: The result of the session wrapped in the `DbAction` monad.
--
-- ==== Examples
-- ```
-- -- Regular session:
-- result <- runDbSession (mkDbCallStack "operation") $
--   HsqlS.statement record statement
--
-- -- Pipeline session:
-- results <- runDbSession (mkDbCallStack "batchOperation") $
--   HsqlS.pipeline $ do
--     r1 <- HsqlP.statement input1 statement1
--     r2 <- HsqlP.statement input2 statement2
--     pure (r1, r2)
-- ```
runDbSession :: MonadIO m => DbCallStack -> HsqlS.Session a -> DbAction m a
runDbSession dbCallStack@DbCallStack {..} session = DbAction $ do
  dbEnv <- ask
  let logMsg msg =
        when (dbEnableLogging dbEnv) $
          for_ (dbTracer dbEnv) $
            \tracer -> liftIO $ logInfo tracer msg
      locationInfo =
        " Function: "
          <> dbCsFncName
          <> " at "
          <> dbCsModule
          <> ":"
          <> dbCsFile
          <> ":"
          <> Text.pack (show dbCsLine)

  if dbEnableLogging dbEnv
    then do
      start <- liftIO getCurrentTime
      result <- run dbEnv
      end <- liftIO getCurrentTime
      let duration = diffUTCTime end start
      logMsg $ "Query: " <> dbCsFncName <> locationInfo <> " in " <> Text.pack (show duration)
      pure result
    else run dbEnv
  where
    run dbEnv = do
      result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
      case result of
        Left sessionErr ->
          throwError $ DbError dbCallStack "Database query failed" (Just sessionErr)
        Right val -> pure val

-- | Extracts call site information from the current call stack.
--
-- This helper function parses the Haskell call stack to provide source location
-- details.
--
-- ==== Returns
-- * @DbCallStack@: A record containing module name, file path, and line number
mkDbCallStack :: HasCallStack => Text -> DbCallStack
mkDbCallStack name =
  case reverse (getCallStack callStack) of
    (_, srcLoc) : _ ->
      DbCallStack
        { dbCsFncName = name
        , dbCsModule = Text.pack $ srcLocModule srcLoc
        , dbCsFile = Text.pack $ srcLocFile srcLoc
        , dbCsLine = srcLocStartLine srcLoc
        }
    [] -> error "No call stack info"

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
