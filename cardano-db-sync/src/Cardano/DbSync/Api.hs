{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Api (
  extractInsertOptions,
  setConsistentLevel,
  getConsistentLevel,
  isConsistent,
  getDisableInOutState,
  getRanIndexes,
  runNearTipMigrations,
  initPruneConsumeMigration,
  runConsumedTxOutMigrationsMaybe,
  runAddJsonbToSchema,
  runRemoveJsonbFromSchema,
  getSafeBlockNoDiff,
  getPruneInterval,
  whenConsumeOrPruneTxOut,
  whenPruneTxOut,
  getTxOutVariantType,
  getPruneConsume,
  getHasConsumedOrPruneTxOut,
  getSkipTxIn,
  getPrunes,
  mkSyncEnvFromConfig,
  verifySnapshotPoint,
  getInsertOptions,
  getTrace,
  getTopLevelConfig,
  getNetwork,
  hasLedgerState,
  getLatestPoints,
  getSlotHash,
  getDbLatestBlockInfo,
  getDbTipBlockNo,
  getCurrentTipBlockNo,
  generateNewEpochEvents,
  logDbState,
  convertToPoint,
)
where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  newTBQueueIO,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, HeaderHash, Point (..), fromRawHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig, configSecurityParam)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (pInfoConfig))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)
import Ouroboros.Network.Block (BlockNo (..), Point (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point

import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Types (CacheCapacity (..), newEmptyCache, useNoCache)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Era.Cardano.Util (initEpochStatistics)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Event (LedgerEvent (..))
import Cardano.DbSync.Ledger.State (
  getHeaderHash,
  hashToAnnotation,
  listKnownSnapshots,
  mkHasLedgerEnv,
 )
import Cardano.DbSync.Ledger.Types (HasLedgerEnv (..), LedgerStateFile (..), SnapshotPoint (..))
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util

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

runNearTipMigrations :: SyncEnv -> IO ()
runNearTipMigrations env = do
  haveRan <- readTVarIO $ envIndexes env
  unless haveRan $ do
    envRunNearTipMigration env DB.NearTip
    logInfo (getTrace env) "NearTip migrations were ran successfully."
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

runConsumedTxOutMigrationsMaybe :: SyncEnv -> IO ()
runConsumedTxOutMigrationsMaybe syncEnv = do
  let pcm = getPruneConsume syncEnv
      txOutVariantType = getTxOutVariantType syncEnv
      bulkSize = DB.getTxOutBulkSize txOutVariantType

  logInfo (getTrace syncEnv) $ "runConsumedTxOutMigrationsMaybe: " <> textShow pcm
  DB.runDbDirectSilent (envDbEnv syncEnv) $
    DB.runConsumedTxOutMigrations
      (getTrace syncEnv)
      bulkSize
      txOutVariantType
      (getSafeBlockNoDiff syncEnv)
      pcm

runAddJsonbToSchema :: SyncEnv -> IO ()
runAddJsonbToSchema syncEnv =
  void $ DB.runDbDirectSilent (envDbEnv syncEnv) DB.enableJsonbInSchema

runRemoveJsonbFromSchema :: SyncEnv -> IO ()
runRemoveJsonbFromSchema syncEnv =
  void $ DB.runDbDirectSilent (envDbEnv syncEnv) DB.disableJsonbInSchema

getSafeBlockNoDiff :: SyncEnv -> Word64
getSafeBlockNoDiff syncEnv = 2 * getSecurityParam syncEnv

getPruneInterval :: SyncEnv -> Word64
getPruneInterval syncEnv = 10 * getSecurityParam syncEnv

whenConsumeOrPruneTxOut :: MonadIO m => SyncEnv -> m () -> m ()
whenConsumeOrPruneTxOut env =
  when (DB.pcmConsumedTxOut $ getPruneConsume env)

whenPruneTxOut :: MonadIO m => SyncEnv -> m () -> m ()
whenPruneTxOut env =
  when (DB.pcmPruneTxOut $ getPruneConsume env)

getTxOutVariantType :: SyncEnv -> DB.TxOutVariantType
getTxOutVariantType syncEnv = ioTxOutVariantType . soptInsertOptions $ envOptions syncEnv

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

getSlotHash :: DB.DbEnv -> SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash backend = DB.runDbDirectSilent backend . DB.querySlotHash

hasLedgerState :: SyncEnv -> Bool
hasLedgerState syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger _ -> True
    NoLedger _ -> False

getDbLatestBlockInfo :: DB.DbEnv -> IO (Maybe TipInfo)
getDbLatestBlockInfo dbEnv = do
  runMaybeT $ do
    block <- MaybeT $ DB.runDbDirectSilent dbEnv DB.queryLatestBlock
    -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
    -- era, but we need to make the types match, hence `fromMaybe`.
    pure $
      TipInfo
        { bHash = DB.blockHash block
        , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
        , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
        , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
        }

getDbTipBlockNo :: SyncEnv -> IO (Point.WithOrigin BlockNo)
getDbTipBlockNo env = do
  mblk <- getDbLatestBlockInfo (envDbEnv env)
  pure $ maybe Point.Origin (Point.At . bBlockNo) mblk

logDbState :: SyncEnv -> IO ()
logDbState env = do
  mblk <- getDbLatestBlockInfo (envDbEnv env)
  case mblk of
    Nothing -> logInfo tracer "Database is empty"
    Just tip -> logInfo tracer $ mconcat ["Database tip is at ", showTip tip]
  where
    showTip :: TipInfo -> Text
    showTip tipInfo =
      mconcat
        [ "slot "
        , textShow (unSlotNo $ bSlotNo tipInfo)
        , ", block "
        , textShow (unBlockNo $ bBlockNo tipInfo)
        ]

    tracer :: Trace IO Text
    tracer = getTrace env

getCurrentTipBlockNo :: SyncEnv -> IO (WithOrigin BlockNo)
getCurrentTipBlockNo env = do
  maybeTip <- getDbLatestBlockInfo (envDbEnv env)
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin

mkSyncEnv ::
  MetricSetters ->
  Trace IO Text ->
  DB.DbEnv ->
  SyncOptions ->
  ProtocolInfo CardanoBlock ->
  Ledger.Network ->
  Word64 ->
  NetworkMagic ->
  SystemStart ->
  SyncNodeConfig ->
  SyncNodeParams ->
  RunMigration ->
  Bool ->
  IO SyncEnv
mkSyncEnv metricSetters trce dbEnv syncOptions protoInfo nw maxLovelaceSupply nwMagic systemStart syncNodeConfigFromFile syncNP runNearTipMigrationFnc isJsonbInSchema = do
  dbCNamesVar <- newTVarIO =<< DB.runDbDirectSilent dbEnv DB.queryRewardAndEpochStakeConstraints
  cache <-
    if soptCache syncOptions
      then
        newEmptyCache
          CacheCapacity
            { cacheCapacityAddress = 50000
            , cacheCapacityStake = 50000
            , cacheCapacityDatum = 125000
            , cacheCapacityMultiAsset = 125000
            , cacheCapacityTx = 50000
            , cacheOptimisePools = 50000
            , cacheOptimiseStake = 50000
            }
      else pure useNoCache
  consistentLevelVar <- newTVarIO Unchecked
  indexesVar <- newTVarIO $ enpForceIndexes syncNP
  bts <- getBootstrapInProgress trce (isTxOutConsumedBootstrap' syncNodeConfigFromFile) dbEnv
  bootstrapVar <- newTVarIO bts
  -- Offline Pool + Anchor queues
  opwq <- newTBQueueIO 1000
  oprq <- newTBQueueIO 1000
  oawq <- newTBQueueIO 1000
  oarq <- newTBQueueIO 1000
  epochVar <- newTVarIO initCurrentEpochNo
  epochStatistics <- initEpochStatistics
  dbIsolationStateVar <- newTVarIO DB.SyncLagging -- For database transaction isolation optimisation
  ledgerEnvType <-
    case (enpMaybeLedgerStateDir syncNP, hasLedger' syncNodeConfigFromFile) of
      (Just dir, True) ->
        HasLedger
          <$> mkHasLedgerEnv
            trce
            protoInfo
            dir
            nw
            maxLovelaceSupply
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
      { envDbEnv = dbEnv
      , envMetricSetters = metricSetters
      , envBootstrap = bootstrapVar
      , envCache = cache
      , envEpochStatistics = epochStatistics
      , envConsistentLevel = consistentLevelVar
      , envDbConstraints = dbCNamesVar
      , envCurrentEpochNo = epochVar
      , envIndexes = indexesVar
      , envDbIsolationState = dbIsolationStateVar
      , envLedgerEnv = ledgerEnvType
      , envNetworkMagic = nwMagic
      , envOffChainPoolResultQueue = oprq
      , envOffChainPoolWorkQueue = opwq
      , envOffChainVoteResultQueue = oarq
      , envOffChainVoteWorkQueue = oawq
      , envOptions = syncOptions
      , envRunNearTipMigration = runNearTipMigrationFnc
      , envSyncNodeConfig = syncNodeConfigFromFile
      , envIsJsonbInSchema = isJsonbInSchema
      , envSystemStart = systemStart
      }
  where
    hasLedger' = hasLedger . sioLedger . dncInsertOptions
    isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions

mkSyncEnvFromConfig ::
  MetricSetters ->
  Trace IO Text ->
  DB.DbEnv ->
  SyncOptions ->
  GenesisConfig ->
  SyncNodeConfig ->
  SyncNodeParams ->
  -- | run migration function
  RunMigration ->
  Bool ->
  IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig metricsSetters trce dbEnv syncOptions genCfg syncNodeConfigFromFile syncNodeParams runNearTipMigrationFnc isJsonbInSchema =
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
              metricsSetters
              trce
              dbEnv
              syncOptions
              (fst $ mkProtocolInfoCardano genCfg [])
              (Shelley.sgNetworkId $ scConfig sCfg)
              (Shelley.sgMaxLovelaceSupply $ scConfig sCfg)
              (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
              (SystemStart . Byron.gdStartTime $ Byron.configGenesisData bCfg)
              syncNodeConfigFromFile
              syncNodeParams
              runNearTipMigrationFnc
              isJsonbInSchema

-- | 'True' is for in memory points and 'False' for on disk
getLatestPoints :: SyncEnv -> IO [(CardanoPoint, Bool)]
getLatestPoints env = do
  case envLedgerEnv env of
    HasLedger hasLedgerEnv -> do
      snapshotPoints <- listKnownSnapshots hasLedgerEnv
      if null snapshotPoints
        then do
          -- Fallback: When no snapshots available (e.g., after restart),
          -- query database for recent blocks to use as intersection points
          lastPoints <- DB.runDbDirectSilent (envDbEnv env) DB.queryLatestPoints
          pure $ mapMaybe convert lastPoints
        else
          verifySnapshotPoint env snapshotPoints
    NoLedger _ -> do
      -- Brings the 5 latest.
      lastPoints <- DB.runDbDirectSilent (envDbEnv env) DB.queryLatestPoints
      pure $ mapMaybe convert lastPoints
  where
    convert (Nothing, _) = Nothing
    convert (Just slot, bs) = convertToDiskPoint (SlotNo slot) bs

verifySnapshotPoint :: SyncEnv -> [SnapshotPoint] -> IO [(CardanoPoint, Bool)]
verifySnapshotPoint env snapPoints =
  catMaybes <$> mapM validLedgerFileToPoint snapPoints
  where
    validLedgerFileToPoint :: SnapshotPoint -> IO (Maybe (CardanoPoint, Bool))
    validLedgerFileToPoint (OnDisk lsf) = do
      hashes <- getSlotHash (envDbEnv env) (lsfSlotNo lsf)
      let valid = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
      case valid of
        Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convertToDiskPoint slot hash
        _ -> pure Nothing
    validLedgerFileToPoint (InMemory pnt) = do
      case pnt of
        GenesisPoint -> pure Nothing
        BlockPoint slotNo hsh -> do
          hashes <- getSlotHash (envDbEnv env) slotNo
          let valid = find (\(_, dbHash) -> getHeaderHash hsh == dbHash) hashes
          case valid of
            Just (dbSlotNo, _) | slotNo == dbSlotNo -> pure $ Just (pnt, True)
            _ -> pure Nothing

convertToDiskPoint :: SlotNo -> ByteString -> Maybe (CardanoPoint, Bool)
convertToDiskPoint slot hashBlob = (,False) <$> convertToPoint slot hashBlob

convertToPoint :: SlotNo -> ByteString -> Maybe CardanoPoint
convertToPoint slot hashBlob =
  Point . Point.block slot <$> convertHashBlob hashBlob
  where
    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

getSecurityParam :: SyncEnv -> Word64
getSecurityParam syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger hle -> getMaxRollbacks $ leProtocolInfo hle
    NoLedger nle -> getMaxRollbacks $ nleProtocolInfo nle

getMaxRollbacks ::
  ConsensusProtocol (BlockProtocol blk) =>
  ProtocolInfo blk ->
  Word64
getMaxRollbacks = Ledger.unNonZero . maxRollbacks . configSecurityParam . pInfoConfig

getBootstrapInProgress ::
  Trace IO Text ->
  Bool ->
  DB.DbEnv ->
  IO Bool
getBootstrapInProgress trce bootstrapFlag dbEnv = do
  DB.runDbDirectSilent dbEnv $ do
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
