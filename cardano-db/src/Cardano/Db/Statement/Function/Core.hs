{-# LANGUAGE GADTs #-}

module Cardano.Db.Statement.Function.Core (
  runSession,
  runSessionEntity,
  bulkEncoder,
  ResultType (..),
  ResultTypeBulk (..),
)
where

import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbEnv (..), DbM (..))
import Cardano.Prelude (MonadIO (..), ask, throwIO)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS

runSession :: HsqlS.Session a -> DbM a
runSession session = do
  dbEnv <- ask
  result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
  case result of
    Left sessionErr -> liftIO $ throwIO sessionErr
    Right a -> pure a

-- | Runs a database session and returns the result as an Entity.
runSessionEntity :: HsqlS.Session (Maybe (Entity record)) -> DbM (Maybe record)
runSessionEntity session = do
  dbEnv <- ask
  result <- liftIO $ HsqlS.run session (dbConnection dbEnv)
  case result of
    Left sessionErr -> liftIO $ throwIO sessionErr
    Right a -> pure $ entityVal <$> a

-- | The result type of an insert operation (usualy it's newly generated id).
data ResultType c r where
  NoResult :: ResultType c () -- No ID, result type is ()
  WithResult :: HsqlD.Result c -> ResultType c c -- Return ID, result type is c

-- | The bulk insert result type
data ResultTypeBulk a where
  NoResultBulk :: ResultTypeBulk () -- No results returned
  WithResultBulk :: HsqlD.Result [a] -> ResultTypeBulk [a] -- Return generated IDs

-- | Creates a parameter encoder for an array of values from a single-value encoder
bulkEncoder :: HsqlE.NullableOrNot HsqlE.Value a -> HsqlE.Params [a]
bulkEncoder v = HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray v
