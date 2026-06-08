{-# LANGUAGE GADTs #-}

module Cardano.Db.Statement.Function.Core (
  runSession,
  runSessionEntity,
  bulkEncoder,
  ResultType (..),
  ResultTypeBulk (..),
)
where

import Cardano.Db.Error (DbCallStack, DbSessionError (..), formatSessionError)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbEnv (..), DbM (..))
import Cardano.Prelude (MonadIO (..), ask, throwIO)
import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS

runSession :: DbCallStack -> HsqlS.Session a -> DbM a
runSession callStack session = do
  dbEnv <- ask
  result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
  case result of
    Left sessionErr -> liftIO $ throwIO $ DbSessionError callStack (formatSessionError sessionErr)
    Right a -> pure a

-- | Runs a database session and returns the result as an Entity.
runSessionEntity :: DbCallStack -> HsqlS.Session (Maybe (Entity record)) -> DbM (Maybe record)
runSessionEntity callStack session = do
  dbEnv <- ask
  result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
  case result of
    Left sessionErr -> liftIO $ throwIO $ DbSessionError callStack (formatSessionError sessionErr)
    Right a -> pure $ entityVal <$> a

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultType c r where
  NoResult :: ResultType c () -- No ID, result type is ()
  WithResult :: HsqlD.Result c -> ResultType c c -- Return ID, result type is c

-- | The bulk insert result type
data ResultTypeBulk a where
  NoResultBulk :: ResultTypeBulk () -- No results returned
  WithResultBulk :: HsqlD.Result [a] -> ResultTypeBulk [a] -- Return generated IDs (RETURNING id)

  -- | Return the given columns via @RETURNING@, decoded with the supplied result decoder.
  -- Useful when the caller needs more than the @id@ (e.g. to match returned rows back to their
  -- inputs by natural key when @ON CONFLICT DO NOTHING@ only returns the newly-inserted rows).
  WithResultBulkColumns :: [Text.Text] -> HsqlD.Result [a] -> ResultTypeBulk [a]

-- | Creates a parameter encoder for an array of values from a single-value encoder
bulkEncoder :: HsqlE.NullableOrNot HsqlE.Value a -> HsqlE.Params [a]
bulkEncoder v = HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray v
