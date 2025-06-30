{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.DbEvent (
  DbEvent (..),
  ThreadChannels (..),
  liftDbIO,
  liftDbError,
  acquireDbConnection,
  blockingFlushDbEventQueue,
  lengthDbEventQueue,
  mkDbApply,
  newThreadChannels,
  writeDbEventQueue,
  waitRollback,
  waitRestartState,
  waitDoneInit,
  runAndSetDone,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, newEmptyTMVarIO, newTVarIO, readTVar, readTVarIO, takeTMVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Connection.Setting as HsqlSet
import Ouroboros.Network.Block (BlockNo, Tip (..))
import qualified Ouroboros.Network.Point as Point

data DbEvent
  = DbApplyBlock !CardanoBlock
  | DbRollBackToPoint !CardanoPoint !(Tip CardanoBlock) !(StrictTMVar IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo))
  | DbRestartState !(StrictTMVar IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo))
  | DbFinish

data ThreadChannels = ThreadChannels
  { tcQueue :: !(TBQueue DbEvent)
  , tcDoneInit :: !(StrictTVar IO Bool)
  }

liftDbIO :: IO a -> ExceptT SyncNodeError IO a
liftDbIO action = do
  result <- liftIO $ try action
  case result of
    Left dbErr -> throwError $ SNErrDatabase dbErr
    Right val -> pure val

liftDbError :: ExceptT DB.DbError IO a -> ExceptT SyncNodeError IO a
liftDbError dbAction = do
  result <- liftIO $ runExceptT dbAction
  case result of
    Left dbErr -> throwError $ SNErrDatabase dbErr
    Right val -> pure val

acquireDbConnection :: [HsqlSet.Setting] -> IO HsqlC.Connection
acquireDbConnection settings = do
  result <- HsqlC.acquire settings
  case result of
    Left connErr -> throwIO $ SNErrDatabase $ DB.DbError (DB.mkDbCallStack "acquireDbConnection") (show connErr) Nothing
    Right conn -> pure conn

mkDbApply :: CardanoBlock -> DbEvent
mkDbApply = DbApplyBlock

-- | This simulates a synhronous operations, since the thread waits for the db
-- worker thread to finish the rollback.
waitRollback :: ThreadChannels -> CardanoPoint -> Tip CardanoBlock -> IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo)
waitRollback tc point serverTip = do
  resultVar <- newEmptyTMVarIO
  atomically $ writeDbEventQueue tc $ DbRollBackToPoint point serverTip resultVar
  atomically $ takeTMVar resultVar

waitRestartState :: ThreadChannels -> IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo)
waitRestartState tc = do
  resultVar <- newEmptyTMVarIO
  atomically $ do
    _ <- TBQ.flushTBQueue (tcQueue tc)
    writeDbEventQueue tc $ DbRestartState resultVar
  atomically $ takeTMVar resultVar

waitDoneInit :: ThreadChannels -> IO ()
waitDoneInit tc = atomically $ do
  isDone <- readTVar (tcDoneInit tc)
  unless isDone retry

runAndSetDone :: ThreadChannels -> IO Bool -> IO Bool
runAndSetDone tc action = do
  isDone <- readTVarIO (tcDoneInit tc)
  if isDone
    then pure True
    else do
      fl <- action
      atomically $ writeTVar (tcDoneInit tc) fl
      pure fl

lengthDbEventQueue :: ThreadChannels -> STM Natural
lengthDbEventQueue = STM.lengthTBQueue . tcQueue

newThreadChannels :: IO ThreadChannels
newThreadChannels =
  -- Use an odd number here so that the db_tip_height metric increments by this odd number
  -- when syncing, instead of incrementing by say 100.
  -- The pipeline queue in the LocalChainSync machinery is 50 elements long
  -- so we should not exceed that.
  ThreadChannels
    <$> TBQ.newTBQueueIO 47
    <*> newTVarIO False

writeDbEventQueue :: ThreadChannels -> DbEvent -> STM ()
writeDbEventQueue = TBQ.writeTBQueue . tcQueue

-- | Block if the queue is empty and if its not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbEventQueue :: ThreadChannels -> IO [DbEvent]
blockingFlushDbEventQueue tc = do
  STM.atomically $ do
    x <- TBQ.readTBQueue $ tcQueue tc
    xs <- TBQ.flushTBQueue $ tcQueue tc
    pure $ x : xs
