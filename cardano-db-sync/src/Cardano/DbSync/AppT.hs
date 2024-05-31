{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.AppT (
  AppT (..),
  App,
  MonadAppDB (..),
  runApp,
  runAppInIO,
  convertAppToReaderT,
  askTrace,
  getTraceFromSyncEnv,
  askNetwork,
  runAppWithNoLogging,
  runAppWithLogging,
  askInsertOptions,
  liftAtomicallyT,
  NoLedgerEnv (..),
  SyncEnv (..),
  SyncOptions (..),
  InsertOptions (..),
  LedgerEnv (..),
  HasLedgerEnv (..),
  StateQueryTMVar (..),
  FixesRan (..),
  ConsistentLevel (..),
  EpochState (..),
  RunMigration,
)
where

import Cardano.BM.Trace (Trace, logError)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (CacheStatus)
import Cardano.DbSync.Config.Types (LedgerStateDir, SyncNodeConfig)
import Cardano.DbSync.Error.Types (SyncNodeError)
import Cardano.DbSync.Ledger.Types (CardanoLedgerState, LedgerDB)
import Cardano.DbSync.Types (CardanoBlock, CardanoInterpreter, OffChainPoolResult, OffChainPoolWorkQueue, OffChainVoteResult, OffChainVoteWorkQueue)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Prelude (
  Applicative,
  Bool,
  Either (..),
  Eq,
  FilePath,
  Functor (..),
  IO,
  Monad ((>>=)),
  MonadError,
  MonadReader,
  STM,
  Show,
  Word64,
  atomically,
  either,
  pure,
  throwIO,
  ($),
 )
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, StrictTVar)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT, NoLoggingT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (MonadUnliftIO)
import qualified Data.Strict.Maybe as Strict
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano ()
import Ouroboros.Consensus.Ledger.Query (Query)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Prelude (Show (..), error, (++), (.))

-- | The application monad parameterized over `IO`.
type App = AppT IO

-- | The application monad transformer.
newtype AppT m a = AppT
  { runAppT :: ReaderT SyncEnv (ExceptT SyncNodeError m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader SyncEnv, MonadError SyncNodeError)

-- | The database monad class. This is used to abstract over the database layer.
class Monad m => MonadAppDB m where
  dbQueryToApp :: ReaderT SqlBackend IO a -> m a

instance MonadAppDB App where
  dbQueryToApp query = do
    sqlBackend <- AppT $ asks envBackend
    liftIO $ runReaderT query sqlBackend

-- | Lift an `App` action into the `AppT` monad transformer.
instance MonadTrans AppT where
  lift = AppT . lift . lift

data SyncEnv = SyncEnv
  { envBackend :: !SqlBackend
  , envCache :: !CacheStatus
  , envConnectionString :: !ConnectionString
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envDbConstraints :: !(StrictTVar IO DB.ManualDbConstraints)
  , envEpochState :: !(StrictTVar IO EpochState)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envIndexes :: !(StrictTVar IO Bool)
  , envIsFixed :: !(StrictTVar IO FixesRan)
  , envBootstrap :: !(StrictTVar IO Bool)
  , envLedgerEnv :: !LedgerEnv
  , envNetworkMagic :: !NetworkMagic
  , envOffChainPoolResultQueue :: !(StrictTBQueue IO OffChainPoolResult)
  , envOffChainPoolWorkQueue :: !(StrictTBQueue IO OffChainPoolWorkQueue)
  , envOffChainVoteResultQueue :: !(StrictTBQueue IO OffChainVoteResult)
  , envOffChainVoteWorkQueue :: !(StrictTBQueue IO OffChainVoteWorkQueue)
  , envOptions :: !SyncOptions
  , envSyncNodeConfig :: !SyncNodeConfig
  , envRunDelayedMigration :: RunMigration
  , envSystemStart :: !SystemStart
  }

data SyncOptions = SyncOptions
  { soptEpochAndCacheEnabled :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
  , soptSkipFix :: !Bool
  , soptOnlyFix :: !Bool
  , soptPruneConsumeMigration :: !DB.PruneConsumeMigration
  , soptInsertOptions :: !InsertOptions
  , snapshotEveryFollowing :: !Word64
  , snapshotEveryLagging :: !Word64
  }
  deriving (Show)

data InsertOptions = InsertOptions
  { ioInOut :: !Bool
  , ioUseLedger :: !Bool
  , ioShelley :: !Bool
  , ioRewards :: !Bool
  , ioMultiAssets :: !Bool
  , ioMetadata :: !Bool
  , ioKeepMetadataNames :: Strict.Maybe [Word64]
  , ioPlutusExtra :: !Bool
  , ioOffChainPoolData :: !Bool
  , ioGov :: !Bool
  }
  deriving (Show)

-- A representation of if we are using a ledger or not given CLI options
data LedgerEnv where
  HasLedger :: HasLedgerEnv -> LedgerEnv
  NoLedger :: NoLedgerEnv -> LedgerEnv

data HasLedgerEnv = HasLedgerEnv
  { leTrace :: Trace IO Text
  , leUseLedger :: !Bool
  , leHasRewards :: !Bool
  , leProtocolInfo :: !(Consensus.ProtocolInfo CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leSystemStart :: !SystemStart
  , leAbortOnPanic :: !Bool
  , leSnapshotEveryFollowing :: !Word64
  , leSnapshotEveryLagging :: !Word64
  , leInterpreter :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar :: !(StrictTVar IO (Strict.Maybe LedgerDB))
  , leStateWriteQueue :: !(TBQueue (FilePath, CardanoLedgerState))
  }

data NoLedgerEnv = NoLedgerEnv
  { nleTracer :: Trace IO Text
  , nleSystemStart :: !SystemStart
  , nleQueryVar :: StateQueryTMVar CardanoBlock CardanoInterpreter
  , nleHistoryInterpreterVar :: StrictTVar IO (Strict.Maybe CardanoInterpreter)
  , nleNetwork :: !Ledger.Network
  , nleProtocolInfo :: !(Consensus.ProtocolInfo CardanoBlock)
  }

newtype StateQueryTMVar blk result = StateQueryTMVar
  { unStateQueryTMVar ::
      StrictTMVar IO (Query blk result, StrictTMVar IO (Either AcquireFailure result))
  }

type RunMigration = DB.MigrationToRun -> App ()

data FixesRan = NoneFixRan | DataFixRan | AllFixRan

data ConsistentLevel = Consistent | DBAheadOfLedger | Unchecked
  deriving (Show, Eq)

data EpochState = EpochState
  { esInitialized :: !Bool
  , esEpochNo :: !(Strict.Maybe EpochNo)
  }

--------------------------------------------------------------------------------
-- Application Functions
--------------------------------------------------------------------------------

-- | Run an `App` action.
runApp :: SyncEnv -> App a -> IO (Either SyncNodeError a)
runApp env app = runExceptT (runReaderT (runAppT app) env)

-- | Run an `App` action in `IO`.
runAppInIO :: SyncEnv -> App a -> IO a
runAppInIO syncEnv appM = do
  result <- runApp syncEnv appM
  case result of
    Left err -> error $ "Error occurred: " ++ show err
    Right val -> pure val

-- | Lift an `App` action to fit the required type for `runDbIohkNoLogging`
runAppWithNoLogging ::
  MonadUnliftIO m =>
  SyncEnv ->
  App a ->
  ReaderT SqlBackend (NoLoggingT m) a
runAppWithNoLogging syncEnv action = do
  let ioAction = runApp syncEnv action
  liftIO ioAction >>= either (error . show) pure

-- | Lift an `App` action to fit the required type for `runDbIohkLogging`
runAppWithLogging ::
  MonadUnliftIO m =>
  SyncEnv ->
  App a ->
  ReaderT SqlBackend (LoggingT m) a
runAppWithLogging syncEnv action = do
  let ioAction = runApp syncEnv action
  liftIO ioAction >>= either (error . show) pure

-- | Convert App to ReaderT SqlBackend m
convertAppToReaderT :: MonadIO m => SyncEnv -> App a -> ReaderT SqlBackend m a
convertAppToReaderT syncEnv app = ReaderT $ \_ ->
  liftIO $
    runExceptT (runReaderT (runAppT app) syncEnv) >>= \case
      Left err -> do
        let tracer = case envLedgerEnv syncEnv of
              HasLedger hasLedgerEnv -> leTrace hasLedgerEnv
              NoLedger noLedgerEnv -> nleTracer noLedgerEnv
        liftIO $ logError tracer $ pack $ show err
        liftIO $ throwIO err
      Right val -> pure val

--------------------------------------------------------------------------------
-- Application Environment Accessors
--------------------------------------------------------------------------------

-- | Get `Trace` from the `SyncEnv`.
askTrace :: App (Trace IO Text)
askTrace = do
  ledgerEnv <- AppT $ asks envLedgerEnv
  pure $ case ledgerEnv of
    HasLedger hasLedgerEnv -> leTrace hasLedgerEnv
    NoLedger noLedgerEnv -> nleTracer noLedgerEnv

-- | Get `Ledger.Network` from the `SyncEnv`.
askNetwork :: App Ledger.Network
askNetwork = do
  syncEnv <- AppT ask
  pure $ case envLedgerEnv syncEnv of
    HasLedger hasLedgerEnv -> leNetwork hasLedgerEnv
    NoLedger noLedgerEnv -> nleNetwork noLedgerEnv

-- | Get `InsertOptions` from the `SyncEnv`.
askInsertOptions :: App InsertOptions
askInsertOptions = do
  envoptions <- AppT $ asks envOptions
  pure $ soptInsertOptions envoptions

-- | Get `Trace` from the `SyncEnv`.
getTraceFromSyncEnv :: SyncEnv -> Trace IO Text
getTraceFromSyncEnv sEnv =
  case envLedgerEnv sEnv of
    HasLedger hasLedgerEnv -> leTrace hasLedgerEnv
    NoLedger noLedgerEnv -> nleTracer noLedgerEnv

liftAtomicallyT :: STM a -> App a
liftAtomicallyT = liftIO . atomically
