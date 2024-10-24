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
  getIsConsumedFixed,
  noneFixed,
  isDataFixed,
  getIsSyncFixed,
  setIsFixed,
  setIsFixedAndMigrate,
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
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Types (CacheCapacity (..), newEmptyCache, useNoCache)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
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
import Cardano.DbSync.Util.Constraint (dbConstraintNamesExists)
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
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, HeaderHash, Point (..), fromRawHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig, configSecurityParam)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (pInfoConfig))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)
import Ouroboros.Network.Block (BlockNo (..), Point (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point

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

getIsConsumedFixed :: SyncEnv -> IO (Maybe Word64)
getIsConsumedFixed env =
  case (DB.pcmPruneTxOut pcm, DB.pcmConsumedTxOut pcm) of
    (False, True) -> Just <$> DB.runDbIohkNoLogging backend (DB.queryWrongConsumedBy txOutTableType)
    _ -> pure Nothing
  where
    txOutTableType = getTxOutTableType env
    pcm = soptPruneConsumeMigration $ envOptions env
    backend = envBackend env

noneFixed :: FixesRan -> Bool
noneFixed NoneFixRan = True
noneFixed _ = False

isDataFixed :: FixesRan -> Bool
isDataFixed DataFixRan = True
isDataFixed _ = False

getIsSyncFixed :: SyncEnv -> IO FixesRan
getIsSyncFixed = readTVarIO . envIsFixed

setIsFixed :: SyncEnv -> FixesRan -> IO ()
setIsFixed env fr = do
  atomically $ writeTVar (envIsFixed env) fr

setIsFixedAndMigrate :: SyncEnv -> FixesRan -> IO ()
setIsFixedAndMigrate env fr = do
  envRunDelayedMigration env DB.Fix
  atomically $ writeTVar (envIsFixed env) fr

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

whenConsumeOrPruneTxOut :: (MonadIO m) => SyncEnv -> m () -> m ()
whenConsumeOrPruneTxOut env =
  when (DB.pcmConsumedTxOut $ getPruneConsume env)

whenPruneTxOut :: (MonadIO m) => SyncEnv -> m () -> m ()
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

getSlotHash :: SqlBackend -> SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash backend = DB.runDbIohkNoLogging backend . DB.querySlotHash

hasLedgerState :: SyncEnv -> Bool
hasLedgerState syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger _ -> True
    NoLedger _ -> False

getDbLatestBlockInfo :: SqlBackend -> IO (Maybe TipInfo)
getDbLatestBlockInfo backend = do
  runMaybeT $ do
    block <- MaybeT $ DB.runDbIohkNoLogging backend DB.queryLatestBlock
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
  mblk <- getDbLatestBlockInfo (envBackend env)
  pure $ maybe Point.Origin (Point.At . bBlockNo) mblk

logDbState :: SyncEnv -> IO ()
logDbState env = do
  mblk <- getDbLatestBlockInfo (envBackend env)
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
  maybeTip <- getDbLatestBlockInfo (envBackend env)
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin

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
  Bool ->
  RunMigration ->
  IO SyncEnv
mkSyncEnv trce backend connectionString syncOptions protoInfo nw nwMagic systemStart syncNodeConfigFromFile syncNP ranMigrations runMigrationFnc = do
  dbCNamesVar <- newTVarIO =<< dbConstraintNamesExists backend
  cache <-
    if soptCache syncOptions
      then
        newEmptyCache
          CacheCapacity
            { cacheCapacityStake = 100000
            , cacheCapacityDatum = 250000
            , cacheCapacityMultiAsset = 250000
            , cacheCapacityTx = 100000
            }
      else pure useNoCache
  consistentLevelVar <- newTVarIO Unchecked
  fixDataVar <- newTVarIO $ if ranMigrations then DataFixRan else NoneFixRan
  indexesVar <- newTVarIO $ enpForceIndexes syncNP
  bts <- getBootstrapInProgress trce (isTxOutConsumedBootstrap' syncNodeConfigFromFile) backend
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
    isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions

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
              ranMigration
              runMigrationFnc

-- | 'True' is for in memory points and 'False' for on disk
getLatestPoints :: SyncEnv -> IO [(CardanoPoint, Bool)]
getLatestPoints env = do
  case envLedgerEnv env of
    HasLedger hasLedgerEnv -> do
      snapshotPoints <- listKnownSnapshots hasLedgerEnv
      verifySnapshotPoint env snapshotPoints
    NoLedger _ -> do
      -- Brings the 5 latest.
      lastPoints <- DB.runDbIohkNoLogging (envBackend env) DB.queryLatestPoints
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
      hashes <- getSlotHash (envBackend env) (lsfSlotNo lsf)
      let valid = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
      case valid of
        Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convertToDiskPoint slot hash
        _ -> pure Nothing
    validLedgerFileToPoint (InMemory pnt) = do
      case pnt of
        GenesisPoint -> pure Nothing
        BlockPoint slotNo hsh -> do
          hashes <- getSlotHash (envBackend env) slotNo
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
