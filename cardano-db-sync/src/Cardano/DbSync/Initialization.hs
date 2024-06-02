{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Initialization (
  mkSyncEnv,
  mkNoLedgerEnv,
  mkSyncEnvFromConfig,
  newStateQueryTMVar,
)
where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (
  ConsistentLevel (..),
  CurrentEpochNo (..),
  FixesRan (..),
  LedgerEnv (..),
  NoLedgerEnv (..),
  RunMigration,
  StateQueryTMVar (..),
  SyncEnv (..),
  SyncOptions (..),
 )
import Cardano.DbSync.Cache.Types (newEmptyCache, useNoCache)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.DbSync.Ledger.State (mkHasLedgerEnv)
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Constraint (dbConstraintNamesExists)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Prelude hiding (atomically, (.))
import Control.Concurrent.Class.MonadSTM.Strict (
  newEmptyTMVarIO,
  newTBQueueIO,
  newTVarIO,
 )
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Node ()
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Network.Magic (NetworkMagic (..))

mkSyncEnv ::
  Trace IO Text ->
  SqlBackend ->
  ConnectionString ->
  SyncOptions ->
  Consensus.ProtocolInfo CardanoBlock ->
  Ledger.Network ->
  NetworkMagic ->
  SystemStart ->
  SyncNodeConfig ->
  SyncNodeParams ->
  Bool ->
  RunMigration ->
  IO SyncEnv
mkSyncEnv trce backend connectionString syncOptions protoInfo nw nwMagic systemStart syncNodeConfigFromFile syncNP ranMigrations runMigrationFnc = do
  dbCNamesVar <- newTVarIO =<< dbConstraintNamesExists backend
  cache <- if soptCache syncOptions then newEmptyCache 250000 50000 else pure useNoCache
  consistentLevelVar <- newTVarIO Unchecked
  fixDataVar <- newTVarIO $ if ranMigrations then DataFixRan else NoneFixRan
  indexesVar <- newTVarIO $ enpForceIndexes syncNP
  bts <- getBootstrapInProgress trce (isTxOutBootstrap' syncNodeConfigFromFile) backend
  bootstrapVar <- newTVarIO bts
  -- Offline Pool + Anchor queues
  opwq <- newTBQueueIO 1000
  oprq <- newTBQueueIO 1000
  oawq <- newTBQueueIO 1000
  oarq <- newTBQueueIO 1000
  epochVar <- newTVarIO initCurrentEpochNo
  epochSyncTime <- newTVarIO =<< getCurrentTime
  ledgerEnvType <-
    case (enpMaybeLedgerStateDir syncNP, hasLedger' syncNodeConfigFromFile) of
      (Just dir, True) ->
        HasLedger
          <$> mkHasLedgerEnv
            trce
            protoInfo
            dir
            nw
            systemStart
            syncOptions
      (Nothing, False) -> NoLedger <$> mkNoLedgerEnv trce protoInfo nw systemStart
      (Just _, False) -> do
        logWarning trce $
          "Disabling the ledger doesn't require having a --state-dir."
            <> " For more details view https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/configuration.md#ledger"
        NoLedger <$> mkNoLedgerEnv trce protoInfo nw systemStart
      -- This won't ever call because we error out this combination at parse time
      (Nothing, True) -> NoLedger <$> mkNoLedgerEnv trce protoInfo nw systemStart

  pure $
    SyncEnv
      { envBackend = backend
      , envBootstrap = bootstrapVar
      , envCache = cache
      , envConnectionString = connectionString
      , envConsistentLevel = consistentLevelVar
      , envDbConstraints = dbCNamesVar
      , envCurrentEpochNo = epochVar
      , envEpochSyncTime = epochSyncTime
      , envIndexes = indexesVar
      , envIsFixed = fixDataVar
      , envLedgerEnv = ledgerEnvType
      , envNetworkMagic = nwMagic
      , envOffChainPoolResultQueue = oprq
      , envOffChainPoolWorkQueue = opwq
      , envOffChainVoteResultQueue = oarq
      , envOffChainVoteWorkQueue = oawq
      , envOptions = syncOptions
      , envRunDelayedMigration = runMigrationFnc
      , envSyncNodeConfig = syncNodeConfigFromFile
      , envSystemStart = systemStart
      }
  where
    hasLedger' = hasLedger . sioLedger . dncInsertOptions
    isTxOutBootstrap' = isTxOutBootstrap . sioTxOut . dncInsertOptions

mkSyncEnvFromConfig ::
  Trace IO Text ->
  SqlBackend ->
  ConnectionString ->
  SyncOptions ->
  GenesisConfig ->
  SyncNodeConfig ->
  SyncNodeParams ->
  -- | migrations were ran on startup
  Bool ->
  -- | run migration function
  RunMigration ->
  IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce backend connectionString syncOptions genCfg syncNodeConfigFromFile syncNodeParams ranMigration runMigrationFnc =
  case genCfg of
    GenesisCardano _ bCfg sCfg _ _
      | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
          pure
            . Left
            . SNErrCardanoConfig
            $ mconcat
              [ "ProtocolMagicId "
              , DB.textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
              , " /= "
              , DB.textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
              ]
      | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
          pure
            . Left
            . SNErrCardanoConfig
            $ mconcat
              [ "SystemStart "
              , DB.textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
              , " /= "
              , DB.textShow (Shelley.sgSystemStart $ scConfig sCfg)
              ]
      | otherwise ->
          Right
            <$> mkSyncEnv
              trce
              backend
              connectionString
              syncOptions
              (fst $ mkProtocolInfoCardano genCfg [])
              (Shelley.sgNetworkId $ scConfig sCfg)
              (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
              (SystemStart . Byron.gdStartTime $ Byron.configGenesisData bCfg)
              syncNodeConfigFromFile
              syncNodeParams
              ranMigration
              runMigrationFnc

mkNoLedgerEnv :: Trace IO Text -> Consensus.ProtocolInfo CardanoBlock -> Ledger.Network -> SystemStart -> IO NoLedgerEnv
mkNoLedgerEnv trce protoInfo network systemStart = do
  qVar <- newStateQueryTMVar
  interVar <- newTVarIO Strict.Nothing
  pure $ NoLedgerEnv trce systemStart qVar interVar network protoInfo

newStateQueryTMVar :: IO (StateQueryTMVar blk result)
newStateQueryTMVar = StateQueryTMVar <$> newEmptyTMVarIO

getBootstrapInProgress ::
  Trace IO Text ->
  Bool ->
  SqlBackend ->
  IO Bool
getBootstrapInProgress trce bootstrapFlag sqlBackend = do
  DB.runDbIohkNoLogging sqlBackend $ do
    ems <- DB.queryAllExtraMigrations
    let btsState = DB.bootstrapState ems
    case (bootstrapFlag, btsState) of
      (False, DB.BootstrapNotStarted) ->
        pure False
      (False, DB.BootstrapDone) ->
        -- Bootsrap was previously finalised so it's not needed.
        pure False
      (False, DB.BootstrapInProgress) -> do
        liftIO $ DB.logAndThrowIO trce "Bootstrap flag not set, but still in progress"
      (True, DB.BootstrapNotStarted) -> do
        liftIO $
          logInfo trce $
            mconcat
              [ "Syncing with bootstrap. "
              , "This won't populate tx_out until the tip of the chain."
              ]
        DB.insertExtraMigration DB.BootstrapStarted
        pure True
      (True, DB.BootstrapInProgress) -> do
        liftIO $
          logInfo trce $
            mconcat
              [ "Syncing with bootstrap is in progress. "
              , "This won't populate tx_out until the tip of the chain."
              ]
        pure True
      (True, DB.BootstrapDone) -> do
        liftIO $
          logWarning trce $
            mconcat
              [ "Bootstrap flag is set, but it will be ignored, "
              , "since bootstrap is already done."
              ]
        pure False

initCurrentEpochNo :: CurrentEpochNo
initCurrentEpochNo =
  CurrentEpochNo
    { cenEpochNo = Strict.Nothing
    }
