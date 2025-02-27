{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Helpers
  ( runDbT,
    mkDbTransaction,
    insert,
    insertCheckUnique,
    bulkInsertNoReturn,
    bulkInsertReturnIds,
    insertManyUnique,
    manyEncoder,
  )
where

import Cardano.BM.Trace (logDebug)
import Cardano.Db.Error (CallSite (..), DbError (..))
import Cardano.Db.Types (DbAction (..), DbTxMode (..), DbTransaction (..), DbEnv (..), HasDbInfo (..))
import Cardano.Prelude (MonadIO (..), ask, when, MonadError (..), Proxy(..))
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
import qualified Data.List.NonEmpty as NE

-- | Runs a database transaction with optional logging.
--
-- This function executes a `DbTransaction` within the `DbAction` monad, handling
-- the transaction mode (read-only or write) and logging execution details if
-- enabled in the `DbEnv`. It captures timing information and call site details
-- for debugging purposes when logging is active.
--
-- ==== Parameters
-- * @DbTxMode@: The transaction mode (`Write` or `ReadOnly`).
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

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultType c r where
  NoResult   :: ResultType c ()  -- No IDs, result type is ()
  WithResult :: HsqlD.Result [c] -> ResultType c [c]  -- Return IDs, result type is [c]

-- | Whether to add a unique constraint to an insert operation.
data WithConstraint a =
  NoConstraint | WithConstraint a

-- | Inserts a record into a table, with option of returning the generated ID.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insert
  :: forall a c r. (HasDbInfo a)
  => HsqlE.Params a             -- Encoder
  -> ResultType c r             -- Whether to return a result and decoder
  -> a                          -- Record
  -> HsqlT.Transaction r
insert encoder resultType record =
  HsqlT.statement record $ HsqlS.Statement sql encoder decoder True
  where
    (decoder, shouldReturntype) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec  -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    -- columns drop the ID column
    colsNoId = NE.fromList $ NE.drop 1 (columnNames (Proxy @a))

    values = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "INSERT INTO " <> table
      , " (" <> Text.intercalate ", " (NE.toList colsNoId) <> ")"
      , " VALUES (" <> values <> ")"
      , shouldReturntype
      ]

-- | Inserts a record into a table, checking for a unique constraint violation.
--
-- ==== Parameters
-- * @constraintName@: The name of the unique constraint to check.
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insertCheckUnique
  :: forall a c r. (HasDbInfo a)
  => Text.Text                  -- Unique constraint name
  -> HsqlE.Params a             -- Encoder
  -> ResultType c r             -- Whether to return a result and decoder
  -> a                          -- Record
  -> HsqlT.Transaction r
insertCheckUnique constraintName encoder resultType record =
  HsqlT.statement record $ HsqlS.Statement sql encoder decoder True
  where
    (decoder, shouldReturntype) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec  -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    cols = columnNames (Proxy @a)
    -- Drop the ID column
    colsNoId = NE.fromList $ NE.drop 1 cols
    dummyUpdateField = NE.head cols
    placeholders = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "INSERT INTO " <> table
      , " (" <> Text.intercalate ", " (NE.toList cols) <> ")"
      , " VALUES (" <> placeholders <> ")"
      , " ON CONFLICT ON CONSTRAINT " <> constraintName
      , " DO UPDATE SET " <> dummyUpdateField <> " = EXCLUDED." <> dummyUpdateField
      , shouldReturntype
      ]

-- | Inserts multiple records into a table in a single transaction using UNNEST and discards the generated IDs.
bulkInsertNoReturn
  :: forall a b. (HasDbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Bulk encoder
  -> [a]                        -- Records
  -> HsqlT.Transaction ()
bulkInsertNoReturn extract enc = bulkInsert extract enc NoConstraint NoResult

-- | Inserts multiple records into a table in a single transaction using UNNEST and returns the generated IDs.
bulkInsertReturnIds
  :: forall a b c. (HasDbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Bulk Encoder
  -> HsqlD.Result [c]           -- Bulk decoder
  -> [a]                        -- Records
  -> HsqlT.Transaction [c]
bulkInsertReturnIds extract enc dec = bulkInsert extract enc NoConstraint (WithResult dec)

insertManyUnique
  :: forall a b. (HasDbInfo a)
  => ([a] -> b)                -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b            -- Bulk Encoder
  -> WithConstraint Text.Text  -- Whether to add a constraint
  -> [a]                       -- Records
  -> HsqlT.Transaction ()
insertManyUnique extract enc withConstraint = bulkInsert extract enc withConstraint NoResult

-- | Inserts multiple records into a table in a single transaction using UNNEST.
--
-- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- executing all inserts in one SQL statement, and can return the generated IDs.
bulkInsert
  :: forall a b c r. (HasDbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Encoder
  -> WithConstraint Text.Text   -- Whether to add a constraint
  -> ResultType c r             -- Whether to return a result and decoder
  -> [a]                        -- Records
  -> HsqlT.Transaction r
bulkInsert extract enc withConstraint returnIds xs =
  HsqlT.statement params $ HsqlS.Statement sql enc decoder True
  where
    params = extract xs
    table = tableName (Proxy @a)
    cols = NE.toList $ columnNames (Proxy @a)
    colsNoId = drop 1 cols

    unnestVals = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

    conflictClause :: Text.Text
    conflictClause = case withConstraint of
      WithConstraint constraint -> " ON CONFLICT ON CONSTRAINT " <> constraint <> " DO NOTHING"
      NoConstraint -> ""

    (decoder, shouldReturnId) = case returnIds of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec  -> (dec, "RETURNING id")

    sql = TextEnc.encodeUtf8 $ Text.concat
      ["INSERT INTO " <> table
      , " (" <> Text.intercalate ", " colsNoId <> ") "
      , " SELECT * FROM UNNEST ("
      , unnestVals <> " ) "
      , conflictClause
      , shouldReturnId
      ]

-- | Creates a parameter encoder for an array of values from a single-value encoder
manyEncoder :: HsqlE.NullableOrNot HsqlE.Value a -> HsqlE.Params [a]
manyEncoder v = HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray v
