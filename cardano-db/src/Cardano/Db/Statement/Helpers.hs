{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Helpers where

import Cardano.BM.Trace (logDebug)
import Cardano.Db.Error (CallSite (..), DbError (..))
import Cardano.Db.Types (DbAction (..), DbTxMode (..), DbTransaction (..), DbEnv (..))
import Cardano.Prelude (MonadIO (..), ask, when, MonadError (..))
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Stack (HasCallStack, getCallStack, callStack, SrcLoc (..))
import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Transaction.Sessions as HsqlT
import qualified Data.Text.Encoding as TextEnc

-- | Runs a database transaction with optional logging.
--
-- This function executes a `DbTransaction` within the `DbAction` monad, handling
-- the transaction mode (read-only or write) and logging execution details if
-- enabled in the `DbEnv`. It captures timing information and call site details
-- for debugging purposes when logging is active.
--
-- ==== Parameters
-- * @mode@: The transaction mode (`Write` or `ReadOnly`).
-- * @DbTransaction{..}@: The transaction to execute, containing the function name,
--   call site, and the `Hasql` transaction.
--
-- ==== Returns
-- * @DbAction m a@: The result of the transaction wrapped in the `DbAction` monad.
runDbT
  :: MonadIO m
  => DbTxMode
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
      logMsg $ "Starting transaction: " <> dtFunctionName <> locationInfo
      start <- liftIO getCurrentTime
      result <- runSession
      end <- liftIO getCurrentTime
      let duration = diffUTCTime end start
      logMsg $ "Transaction completed: " <> dtFunctionName <> locationInfo <> " in " <> Text.pack (show duration)
      pure result
    else runSession
  where
    session = HsqlT.transaction HsqlT.Serializable txMode dtTx
    txMode = case mode of
      Write -> HsqlT.Write
      ReadOnly -> HsqlT.Read
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
  where
    mkCallSite :: HasCallStack => CallSite
    mkCallSite =
      case reverse (getCallStack callStack) of
        (_, srcLoc) : _ -> CallSite
          { csModule = Text.pack $ srcLocModule srcLoc
          , csFile = Text.pack $ srcLocFile srcLoc
          , csLine = srcLocStartLine srcLoc
          }
        [] -> error "No call stack info"

-- | Inserts multiple records into a table in a single transaction using UNNEST.
--
-- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- executing all inserts in one SQL statement, and returns the generated IDs.
--
-- ==== Parameters
-- * @table@: Text - The name of the table to insert into.
-- * @cols@: [Text] - List of column names (excluding the ID column).
-- * @types@: [Text] - List of PostgreSQL type casts for each column (e.g., "bigint[]").
-- * @extract@: ([a] -> [b]) - Function to extract fields from a list of records into a tuple of lists.
-- * @enc@: HsqlE.Params [b] - Encoder for the extracted fields as a tuple of lists.
-- * @dec@: HsqlD.Result [c] - Decoder for the returned IDs.
-- * @xs@: [a] - List of records to insert.
--
-- ==== Returns
-- * @DbAction m [c]@: The list of generated IDs wrapped in the `DbAction` monad.
bulkInsert
  :: Text.Text -- Table name
  -> [Text.Text] -- Column names
  -> [Text.Text] -- Type casts for UNNEST
  -> ([a] -> b) -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b -- Bulk encoder
  -> HsqlD.Result [c] -- ID decoder
  -> [a] -- Records
  -> HsqlT.Transaction [c]  -- Resulting IDs
bulkInsert table cols types extract enc dec xs =
  HsqlT.statement params $ HsqlS.Statement sql enc dec True
  where
    params = extract xs
    sql = TextEnc.encodeUtf8 $
      "INSERT INTO " <> table <> " (" <> Text.intercalate ", " cols <> ") \
          \SELECT * FROM UNNEST (" <> Text.intercalate ", " (zipWith (\i t -> "$" <> Text.pack (show i) <> "::" <> t) [1..] types) <> ") \
          \RETURNING id"
