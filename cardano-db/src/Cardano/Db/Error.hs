{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Error (
  DbCallStack (..),
  DbError (..),
  runOrThrowIODb,
  runOrThrowIO,
  logAndThrowIO,
) where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Prelude (MonadIO, throwIO)
import Control.Exception (Exception)
import Data.Text (Text)

import qualified Hasql.Session as HsqlSes

data DbError = DbError
  { dbErrorDbCallStack :: !DbCallStack
  , dbErrorMessage :: !Text
  , dbErrorCause :: !(Maybe HsqlSes.SessionError)
  }
  deriving (Show, Eq)

instance Exception DbError

data DbCallStack = DbCallStack
  { dbCsFncName :: !Text
  , dbCsModule :: !Text
  , dbCsFile :: !Text
  , dbCsLine :: !Int
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
