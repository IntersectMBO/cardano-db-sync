{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Api (
  extractInsertOptions,
  setConsistentLevel,
  getConsistentLevel,
  isConsistent,
  getDisableInOutState,
  getRanIndexes,
  runIndexMigrations,
  initPruneConsumeMigration,
  runExtraMigrationsMaybe,
  runAddJsonbToSchema,
  runRemoveJsonbFromSchema,
  getSafeBlockNoDiff,
  getPruneInterval,
  whenConsumeOrPruneTxOut,
  whenPruneTxOut,
  getTxOutTableType,
  getPruneConsume,
  getHasConsumedOrPruneTxOut,
  getSkipTxIn,
  getPrunes,
  mkSyncEnvFromConfig,
  getInsertOptions,
  getTrace,
  getTopLevelConfig,
  getNetwork,
  hasLedgerState,
  writePrefetch,
  addNewEventsAndSort,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Types (CacheCapacity (..), newEmptyCache, newStakeChannels, useNoCache)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Event (LedgerEvent (..))
import Cardano.DbSync.Ledger.State (mkHasLedgerEnv)
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (dbConstraintNamesExists)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  newTBQueueIO,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import qualified Control.Concurrent.Class.MonadSTM.Strict.TBQueue as TBQ
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig, configSecurityParam)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (pInfoConfig))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)
import Ouroboros.Network.Magic (NetworkMagic (..))

setConsistentLevel :: SyncEnv -> ConsistentLevel -> IO ()
setConsistentLevel env cst = do
  logInfo (getTrace env) $ "Setting ConsistencyLevel to " <> textShow cst
  atomically $ writeTVar (envConsistentLevel env) cst

getConsistentLevel :: SyncEnv -> IO ConsistentLevel
getConsistentLevel env =
  readTVarIO (envConsistentLevel env)

isConsistent :: SyncEnv -> IO Bool
isConsistent env = do
  cst <- getConsistentLevel env
  case cst of
    Consistent -> pure True
    _ -> pure False

getDisableInOutState :: SyncEnv -> IO Bool
getDisableInOutState syncEnv = do
  bst <- readTVarIO $ envBootstrap syncEnv
  pure $ bst || not (ioInOut iopts)
  where
    iopts = getInsertOptions syncEnv

getRanIndexes :: SyncEnv -> IO Bool
getRanIndexes env = do
  readTVarIO $ envIndexes env

runIndexMigrations :: SyncEnv -> IO ()
runIndexMigrations env = do
  haveRan <- readTVarIO $ envIndexes env
  unless haveRan $ do
    envRunDelayedMigration env DB.Indexes
    logInfo (getTrace env) "Indexes were created"
    atomically $ writeTVar (envIndexes env) True

initPruneConsumeMigration :: Bool -> Bool -> Bool -> Bool -> DB.PruneConsumeMigration
initPruneConsumeMigration consumed pruneTxOut bootstrap forceTxIn' =
  DB.PruneConsumeMigration
    { DB.pcmPruneTxOut = pruneTxOut || bootstrap
    , DB.pcmConsumedTxOut = consumed || pruneTxOut || bootstrap
    , DB.pcmSkipTxIn = not forceTxIn' && (consumed || pruneTxOut || bootstrap)
    }

getPruneConsume :: SyncEnv -> DB.PruneConsumeMigration
getPruneConsume = soptPruneConsumeMigration . envOptions

runExtraMigrationsMaybe :: SyncEnv -> IO ()
runExtraMigrationsMaybe syncEnv = do
  let pcm = getPruneConsume syncEnv
      txOutTableType = getTxOutTableType syncEnv
  logInfo (getTrace syncEnv) $ "runExtraMigrationsMaybe: " <> textShow pcm
  DB.runDbIohkNoLogging (envBackend syncEnv) $
    DB.runExtraMigrations
      (getTrace syncEnv)
      txOutTableType
      (getSafeBlockNoDiff syncEnv)
      pcm

runAddJsonbToSchema :: SyncEnv -> IO ()
runAddJsonbToSchema syncEnv =
  void $ DB.runDbIohkNoLogging (envBackend syncEnv) DB.enableJsonbInSchema

runRemoveJsonbFromSchema :: SyncEnv -> IO ()
runRemoveJsonbFromSchema syncEnv =
  void $ DB.runDbIohkNoLogging (envBackend syncEnv) DB.disableJsonbInSchema

getSafeBlockNoDiff :: SyncEnv -> Word64
getSafeBlockNoDiff syncEnv = 2 * getSecurityParam syncEnv

getPruneInterval :: SyncEnv -> Word64
getPruneInterval syncEnv = 10 * getSecurityParam syncEnv

whenConsumeOrPruneTxOut :: MonadIO m => SyncEnv -> m () -> m ()
whenConsumeOrPruneTxOut syncEnv action = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  when (not disInOut && DB.pcmConsumedTxOut (getPruneConsume syncEnv)) action

whenPruneTxOut :: MonadIO m => SyncEnv -> m () -> m ()
whenPruneTxOut env =
  when (DB.pcmPruneTxOut $ getPruneConsume env)

getTxOutTableType :: SyncEnv -> DB.TxOutTableType
getTxOutTableType syncEnv = ioTxOutTableType . soptInsertOptions $ envOptions syncEnv

getHasConsumedOrPruneTxOut :: SyncEnv -> Bool
getHasConsumedOrPruneTxOut =
  DB.pcmConsumedTxOut . getPruneConsume

getSkipTxIn :: SyncEnv -> Bool
getSkipTxIn =
  DB.pcmSkipTxIn . getPruneConsume

getPrunes :: SyncEnv -> Bool
getPrunes = do
  DB.pcmPruneTxOut . getPruneConsume

extractInsertOptions :: SyncPreConfig -> SyncInsertOptions
extractInsertOptions = sicOptions . pcInsertConfig

initCurrentEpochNo :: CurrentEpochNo
initCurrentEpochNo =
  CurrentEpochNo
    { cenEpochNo = Strict.Nothing
    }

addNewEventsAndSort :: SyncEnv -> ApplyResult -> IO ApplyResult
addNewEventsAndSort env applyResult = do
  epochEvents <- liftIO $ atomically $ generateNewEpochEvents env details
  pure applyResult {apEvents = sort $ epochEvents <> apEvents applyResult}
  where
    details = apSlotDetails applyResult

generateNewEpochEvents :: SyncEnv -> SlotDetails -> STM [LedgerEvent]
generateNewEpochEvents env details = do
  !lastEpochNo <- readTVar (envCurrentEpochNo env)
  writeTVar (envCurrentEpochNo env) newCurrentEpochNo
  pure $ maybeToList (newEpochEvent lastEpochNo)
  where
    currentEpochNo :: EpochNo
    currentEpochNo = sdEpochNo details

    newEpochEvent :: CurrentEpochNo -> Maybe LedgerEvent
    newEpochEvent lastEpochNo =
      case cenEpochNo lastEpochNo of
        Strict.Nothing -> Just $ LedgerStartAtEpoch currentEpochNo
        Strict.Just oldEpoch
          | currentEpochNo == EpochNo (1 + unEpochNo oldEpoch) ->
              Just $ LedgerNewEpoch currentEpochNo (getSyncStatus details)
        _ -> Nothing

    newCurrentEpochNo :: CurrentEpochNo
    newCurrentEpochNo =
      CurrentEpochNo
        { cenEpochNo = Strict.Just currentEpochNo
        }

getTopLevelConfig :: SyncEnv -> TopLevelConfig CardanoBlock
getTopLevelConfig syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger hasLedgerEnv -> Consensus.pInfoConfig $ leProtocolInfo hasLedgerEnv
    NoLedger noLedgerEnv -> Consensus.pInfoConfig $ nleProtocolInfo noLedgerEnv

getTrace :: SyncEnv -> Trace IO Text
getTrace sEnv =
  case envLedgerEnv sEnv of
    HasLedger hasLedgerEnv -> leTrace hasLedgerEnv
    NoLedger noLedgerEnv -> nleTracer noLedgerEnv

getNetwork :: SyncEnv -> Ledger.Network
getNetwork sEnv =
  case envLedgerEnv sEnv of
    HasLedger hasLedgerEnv -> leNetwork hasLedgerEnv
    NoLedger noLedgerEnv -> nleNetwork noLedgerEnv

getInsertOptions :: SyncEnv -> InsertOptions
getInsertOptions = soptInsertOptions . envOptions

hasLedgerState :: SyncEnv -> Bool
hasLedgerState syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger _ -> True
    NoLedger _ -> False

writePrefetch :: SyncEnv -> CardanoBlock -> IO ()
writePrefetch syncEnv cblock = do
  atomically $
    TBQ.writeTBQueue (pTxInQueue $ envPrefetch syncEnv) $
      PrefetchTxIdBlock cblock

mkSyncEnv ::
  Trace IO Text ->
  SqlBackend ->
  ConnectionString ->
  SyncOptions ->
  ProtocolInfo CardanoBlock ->
  Ledger.Network ->
  NetworkMagic ->
  SystemStart ->
  SyncNodeConfig ->
  SyncNodeParams ->
  RunMigration ->
  IO SyncEnv
mkSyncEnv trce backend connectionString syncOptions protoInfo nw nwMagic systemStart syncNodeConfigFromFile syncNP runMigrationFnc = do
  dbCNamesVar <- newTVarIO =<< dbConstraintNamesExists backend
  cache <-
    if soptCache syncOptions
      then
        newEmptyCache
          CacheCapacity
            { cacheCapacityAddress = 100000
            , cacheCapacityStake = 100000
            , cacheCapacityDatum = 250000
            , cacheCapacityMultiAsset = 250000
            , cacheCapacityTx = 100000
            }
      else pure useNoCache
  prefetch <- newPrefetch
  consistentLevelVar <- newTVarIO Unchecked
  indexesVar <- newTVarIO $ enpForceIndexes syncNP
  bts <- getBootstrapInProgress trce (isTxOutConsumedBootstrap' syncNodeConfigFromFile) backend
  bootstrapVar <- newTVarIO bts
  -- Offline Pool + Anchor queues
  cChans <- newStakeChannels
  opwq <- newTBQueueIO 1000
  oprq <- newTBQueueIO 1000
  oawq <- newTBQueueIO 1000
  oarq <- newTBQueueIO 1000
  epochVar <- newTVarIO initCurrentEpochNo
  epochSyncTime <- newTVarIO =<< getCurrentTime
  pool <- DB.runIohkLogging trce $ createPostgresqlPool connectionString 5 -- TODO make configurable
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
      , envPool = pool
      , envBootstrap = bootstrapVar
      , envCache = cache
      , envPrefetch = prefetch
      , envConnectionString = connectionString
      , envConsistentLevel = consistentLevelVar
      , envDbConstraints = dbCNamesVar
      , envCurrentEpochNo = epochVar
      , envEpochSyncTime = epochSyncTime
      , envIndexes = indexesVar
      , envLedgerEnv = ledgerEnvType
      , envNetworkMagic = nwMagic
      , envStakeChans = cChans
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
    isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions

mkSyncEnvFromConfig ::
  Trace IO Text ->
  SqlBackend ->
  ConnectionString ->
  SyncOptions ->
  GenesisConfig ->
  SyncNodeConfig ->
  SyncNodeParams ->
  -- | run migration function
  RunMigration ->
  IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce backend connectionString syncOptions genCfg syncNodeConfigFromFile syncNodeParams runMigrationFnc =
  case genCfg of
    GenesisCardano _ bCfg sCfg _ _
      | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
          pure
            . Left
            . SNErrCardanoConfig
            $ mconcat
              [ "ProtocolMagicId "
              , textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
              , " /= "
              , textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
              ]
      | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
          pure
            . Left
            . SNErrCardanoConfig
            $ mconcat
              [ "SystemStart "
              , textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
              , " /= "
              , textShow (Shelley.sgSystemStart $ scConfig sCfg)
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
              runMigrationFnc

getSecurityParam :: SyncEnv -> Word64
getSecurityParam syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger hle -> getMaxRollbacks $ leProtocolInfo hle
    NoLedger nle -> getMaxRollbacks $ nleProtocolInfo nle

getMaxRollbacks ::
  (ConsensusProtocol (BlockProtocol blk)) =>
  ProtocolInfo blk ->
  Word64
getMaxRollbacks = maxRollbacks . configSecurityParam . pInfoConfig

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
