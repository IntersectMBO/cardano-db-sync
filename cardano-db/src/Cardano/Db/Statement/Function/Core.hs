{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Statement.Function.Core
  ( runDbT,
    mkDbTransaction,
    mkCallSite,
    manyEncoder,
    ResultType(..),
    ResultTypeBulk(..),
  )
where

import Cardano.BM.Trace (logDebug)
import Cardano.Db.Error (CallSite (..), DbError (..))
import Cardano.Db.Types (DbAction (..), DbTransMode (..), DbTransaction (..), DbEnv (..))
import Cardano.Prelude (MonadIO (..), ask, when, MonadError (..))
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Stack (HasCallStack, getCallStack, callStack, SrcLoc (..))
import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Transaction.Sessions as HsqlT

-- | Runs a database transaction with optional logging.
--
-- This function executes a `DbTransaction` within the `DbAction` monad, handling
-- the transaction mode (read-only or write) and logging execution details if
-- enabled in the `DbEnv`. It captures timing information and call site details
-- for debugging purposes when logging is active.
--
-- ==== Parameters
-- * @DbTransMode@: The transaction mode (`Write` or `ReadOnly`).
-- * @DbTransaction{..}@: The transaction to execute, containing the function name,
--   call site, and the `Hasql` transaction.
--
-- ==== Returns
-- * @DbAction m a@: The result of the transaction wrapped in the `DbAction` monad.
runDbT
  :: MonadIO m
  => DbTransMode
  -> DbTransaction a
  -> DbAction m a
runDbT mode DbTransaction{..} = DbAction $ do
  dbEnv <- ask
  let logMsg msg = when (dbEnableLogging dbEnv) $ liftIO $ logDebug (dbTracer dbEnv) msg

  -- Run the session and handle the result
  let runSession = do
        result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
        case result of
          Left err -> throwError $ QueryError "Transaction failed" dtCallSite err
          Right val -> pure val

  if dbEnableLogging dbEnv
    then do
      start <- liftIO getCurrentTime
      result <- runSession
      end <- liftIO getCurrentTime
      let duration = diffUTCTime end start
      logMsg $ "Transaction: " <> dtFunctionName <> locationInfo <> " in " <> Text.pack (show duration)
      pure result
    else runSession
  where
    session = HsqlT.transaction HsqlT.Serializable transMode dtTx
    transMode = case mode of
      TransWrite -> HsqlT.Write
      TransReadOnly -> HsqlT.Read
    locationInfo = " at " <> csModule dtCallSite <> ":" <>
                    csFile dtCallSite <> ":" <> Text.pack (show $ csLine dtCallSite)

-- | Creates a `DbTransaction` with a function name and call site.
--
-- Constructs a `DbTransaction` record for use with `runDbT`, capturing the
-- function name and call site from the current stack trace. This is useful
-- for logging and debugging database operations.
--
-- ==== Parameters
-- * @funcName@: The name of the function or operation being performed.
-- * @transx@: The `Hasql` transaction to encapsulate.
--
-- ==== Returns
-- * @DbTransaction a@: A transaction record with metadata.
mkDbTransaction :: Text.Text -> HsqlT.Transaction a -> DbTransaction a
mkDbTransaction funcName transx =
  DbTransaction
    { dtFunctionName = funcName
    , dtCallSite = mkCallSite
    , dtTx = transx
    }

mkCallSite :: HasCallStack => CallSite
mkCallSite =
  case reverse (getCallStack callStack) of
    (_, srcLoc) : _ -> CallSite
      { csModule = Text.pack $ srcLocModule srcLoc
      , csFile = Text.pack $ srcLocFile srcLoc
      , csLine = srcLocStartLine srcLoc
      }
    [] -> error "No call stack info"

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultType c r where
  NoResult   :: ResultType c ()  -- No ID, result type is ()
  WithResult :: HsqlD.Result c -> ResultType c c  -- Return ID, result type is c

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultTypeBulk c r where
  NoResultBulk   :: ResultTypeBulk c ()  -- No IDs, result type is ()
  WithResultBulk :: HsqlD.Result [c] -> ResultTypeBulk c [c]  -- Return IDs, result type is [c]

-- | Creates a parameter encoder for an array of values from a single-value encoder
manyEncoder :: HsqlE.NullableOrNot HsqlE.Value a -> HsqlE.Params [a]
manyEncoder v = HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray v
