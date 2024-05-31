{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Api (
  extractInsertOptions,
  fullInsertOptions,
  onlyUTxOInsertOptions,
  onlyGovInsertOptions,
  disableAllInsertOptions,
  setConsistentLevel,
  getConsistentLevel,
  isConsistent,
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
  getSafeBlockNoDiff,
  getPruneInterval,
  whenConsumeOrPruneTxOut,
  whenPruneTxOut,
  getHasConsumedOrPruneTxOut,
  getSkipTxIn,
  getPrunes,
  verifySnapshotPoint,
  getInsertOptions,
  getTopLevelConfig,
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

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (newEmptyCache, useNoCache)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.AppT (
  App,
  ConsistentLevel (..), EpochState (..), FixesRan (..), HasLedgerEnv (..), InsertOptions (..), LedgerEnv (..), MonadAppDB (..), NoLedgerEnv (..), SyncEnv (..), SyncOptions (..), askInsertOptions, askTrace, liftAtomicallyT)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Ledger.Event (LedgerEvent (..))
import Cardano.DbSync.Ledger.State (
  getHeaderHash,
  hashToAnnotation,
  listKnownSnapshots,
 )
import Cardano.DbSync.Ledger.Types (LedgerStateFile (..), SnapshotPoint (..))
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  readTVarIO,
  writeTVar,
 )
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Strict.Maybe as Strict
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, HeaderHash, Point (..), fromRawHash)
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig, configSecurityParam)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (pInfoConfig))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)
import Ouroboros.Network.Block (BlockNo (..), Point (..))
import qualified Ouroboros.Network.Point as Point

setConsistentLevel :: ConsistentLevel -> App ()
setConsistentLevel cst = do
  trce <- askTrace
  consistentLevel <- asks envConsistentLevel
  liftIO $ logInfo trce $ "Setting ConsistencyLevel to " <> textShow cst
  liftIO $ atomically $ writeTVar consistentLevel cst

getConsistentLevel :: App ConsistentLevel
getConsistentLevel = liftIO . readTVarIO =<< asks envConsistentLevel

isConsistent :: App Bool
isConsistent = do
  cst <- getConsistentLevel
  case cst of
    Consistent -> pure True
    _other -> pure False

noneFixed :: FixesRan -> Bool
noneFixed NoneFixRan = True
noneFixed _ = False

isDataFixed :: FixesRan -> Bool
isDataFixed DataFixRan = True
isDataFixed _ = False

getIsSyncFixed :: App FixesRan
getIsSyncFixed = liftIO . readTVarIO =<< asks envIsFixed

setIsFixed :: FixesRan -> App ()
setIsFixed fr = do
  isFixed <- asks envIsFixed
  liftIO $ atomically $ writeTVar isFixed fr

setIsFixedAndMigrate :: FixesRan -> App ()
setIsFixedAndMigrate fr = do
  runDelayedMigration <- asks envRunDelayedMigration
  isFixed <- asks envIsFixed
  runDelayedMigration DB.Fix
  liftIO $ atomically $ writeTVar isFixed fr

getDisableInOutState :: App Bool
getDisableInOutState = do
  bootStrap <- asks envBootstrap
  insertOps <- askInsertOptions
  bst <- liftIO $ readTVarIO bootStrap
  pure $ bst || not (ioInOut insertOps)

getRanIndexes :: App Bool
getRanIndexes = liftIO . readTVarIO =<< asks envIndexes

runIndexMigrations :: App ()
runIndexMigrations = do
  syncEnv <- ask
  trce <- askTrace
  indexes <- asks envIndexes
  haveRan <- liftIO $ readTVarIO indexes
  unless haveRan $ do
    envRunDelayedMigration syncEnv DB.Indexes
    liftIO $ logInfo trce "Indexes were created"
    liftIO $ atomically $ writeTVar indexes True

initPruneConsumeMigration :: Bool -> Bool -> Bool -> Bool -> DB.PruneConsumeMigration
initPruneConsumeMigration consumed pruneTxOut bootstrap forceTxIn' =
  DB.PruneConsumeMigration
    { DB.pcmPruneTxOut = pruneTxOut || bootstrap
    , DB.pcmConsumeOrPruneTxOut = consumed || pruneTxOut || bootstrap
    , DB.pcmSkipTxIn = not forceTxIn' && (consumed || pruneTxOut || bootstrap)
    }

getPruneConsume :: App DB.PruneConsumeMigration
getPruneConsume = soptPruneConsumeMigration <$> asks envOptions

runExtraMigrationsMaybe :: App ()
runExtraMigrationsMaybe = do
  backend <- asks envBackend
  trce <- askTrace
  pcm <- getPruneConsume
  safeBlockNodiff <- getSafeBlockNoDiff
  liftIO $ logInfo trce $ textShow pcm
  liftIO $
    DB.runDbIohkNoLogging backend $
      DB.runExtraMigrations trce safeBlockNodiff pcm

getSafeBlockNoDiff :: App Word64
getSafeBlockNoDiff = (* 2) <$> getSecurityParam

getPruneInterval :: App Word64
getPruneInterval = (* 10) <$> getSecurityParam

whenConsumeOrPruneTxOut :: App () -> App ()
whenConsumeOrPruneTxOut action = getPruneConsume >>= flip when action . DB.pcmConsumeOrPruneTxOut

whenPruneTxOut :: App () -> App ()
whenPruneTxOut action = getPruneConsume >>= flip when action . DB.pcmPruneTxOut

getHasConsumedOrPruneTxOut :: App Bool
getHasConsumedOrPruneTxOut =
  getPruneConsume <&> DB.pcmConsumeOrPruneTxOut

getSkipTxIn :: App Bool
getSkipTxIn = getPruneConsume <&> DB.pcmSkipTxIn

getPrunes :: App Bool
getPrunes = getPruneConsume <&> DB.pcmPruneTxOut

extractInsertOptions :: SyncPreConfig -> SyncInsertOptions
extractInsertOptions cfg =
  case pcInsertConfig cfg of
    FullInsertOptions -> fullInsertOptions
    OnlyUTxOInsertOptions -> onlyUTxOInsertOptions
    OnlyGovInsertOptions -> onlyGovInsertOptions
    DisableAllInsertOptions -> disableAllInsertOptions
    SyncInsertConfig opts -> opts

fullInsertOptions :: SyncInsertOptions
fullInsertOptions =
  SyncInsertOptions
    { sioTxOut = TxOutEnable
    , sioLedger = LedgerEnable
    , sioShelley = ShelleyEnable
    , sioRewards = RewardsConfig True
    , sioMultiAsset = MultiAssetEnable
    , sioMetadata = MetadataEnable
    , sioPlutus = PlutusEnable
    , sioGovernance = GovernanceConfig True
    , sioOffchainPoolData = OffchainPoolDataConfig True
    , sioJsonType = JsonTypeText
    }

onlyUTxOInsertOptions :: SyncInsertOptions
onlyUTxOInsertOptions =
  SyncInsertOptions
    { sioTxOut = TxOutBootstrap (ForceTxIn False)
    , sioLedger = LedgerIgnore
    , sioShelley = ShelleyDisable
    , sioRewards = RewardsConfig True
    , sioMultiAsset = MultiAssetDisable
    , sioMetadata = MetadataDisable
    , sioPlutus = PlutusDisable
    , sioGovernance = GovernanceConfig False
    , sioOffchainPoolData = OffchainPoolDataConfig False
    , sioJsonType = JsonTypeText
    }

onlyGovInsertOptions :: SyncInsertOptions
onlyGovInsertOptions =
  disableAllInsertOptions
    { sioLedger = LedgerEnable
    , sioGovernance = GovernanceConfig True
    }

disableAllInsertOptions :: SyncInsertOptions
disableAllInsertOptions =
  SyncInsertOptions
    { sioTxOut = TxOutDisable
    , sioLedger = LedgerDisable
    , sioShelley = ShelleyDisable
    , sioRewards = RewardsConfig False
    , sioMultiAsset = MultiAssetDisable
    , sioMetadata = MetadataDisable
    , sioPlutus = PlutusDisable
    , sioOffchainPoolData = OffchainPoolDataConfig False
    , sioGovernance = GovernanceConfig False
    , sioJsonType = JsonTypeText
    }

initCurrentEpochNo :: CurrentEpochNo
initCurrentEpochNo =
  CurrentEpochNo
    { cenEpochNo = Strict.Nothing
    }

generateNewEpochEvents :: SlotDetails -> App [LedgerEvent]
generateNewEpochEvents details = do
  epochState <- asks envEpochState
  !oldEpochState <- liftIO $ readTVarIO epochState
  liftAtomicallyT $ writeTVar epochState newEpochState
  pure $ maybeToList (newEpochEvent oldEpochState)
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

getInsertOptions :: SyncEnv -> InsertOptions
getInsertOptions = soptInsertOptions . envOptions

getSlotHash :: SlotNo -> App [(SlotNo, ByteString)]
getSlotHash slotNum = do
  backend <- asks envBackend
  dbQueryToApp $ DB.runDbIohkNoLogging backend $ DB.querySlotHash slotNum

hasLedgerState :: SyncEnv -> Bool
hasLedgerState syncEnv =
  case envLedgerEnv syncEnv of
    HasLedger _ -> True
    NoLedger _ -> False

getDbLatestBlockInfo :: SqlBackend -> App (Maybe TipInfo)
getDbLatestBlockInfo backend = do
  runMaybeT $ do
    block <- MaybeT $ dbQueryToApp $ DB.runDbIohkNoLogging backend DB.queryLatestBlock
    -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
    -- era, but we need to make the types match, hence `fromMaybe`.
    pure $
      TipInfo
        { bHash = DB.blockHash block
        , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
        , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
        , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
        }

getDbTipBlockNo :: SyncEnv -> App (Point.WithOrigin BlockNo)
getDbTipBlockNo env = do
  mblk <- getDbLatestBlockInfo (envBackend env)
  pure $ maybe Point.Origin (Point.At . bBlockNo) mblk

logDbState :: App ()
logDbState = do
  syncEnv <- ask
  trce <- askTrace
  mblk <- getDbLatestBlockInfo (envBackend syncEnv)
  case mblk of
    Nothing -> liftIO $ logInfo trce "Database is empty"
    Just tip -> liftIO $ logInfo trce $ mconcat ["Database tip is at ", showTip tip]
  where
    showTip :: TipInfo -> Text
    showTip tipInfo =
      mconcat
        [ "slot "
        , DB.textShow (unSlotNo $ bSlotNo tipInfo)
        , ", block "
        , DB.textShow (unBlockNo $ bBlockNo tipInfo)
        ]

getCurrentTipBlockNo :: SyncEnv -> App (WithOrigin BlockNo)
getCurrentTipBlockNo env = do
  maybeTip <- getDbLatestBlockInfo (envBackend env)
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin

-- | 'True' is for in memory points and 'False' for on disk
getLatestPoints :: App [(CardanoPoint, Bool)]
getLatestPoints = do
  SyncEnv {envBackend, envLedgerEnv} <- ask
  case envLedgerEnv of
    HasLedger hasLedgerEnv -> do
      snapshotPoints <- listKnownSnapshots hasLedgerEnv
      verifySnapshotPoint snapshotPoints
    NoLedger _ -> do
      -- Brings the 5 latest.
      lastPoints <- dbQueryToApp $ DB.runDbIohkNoLogging envBackend DB.queryLatestPoints
      pure $ mapMaybe convert lastPoints
  where
    convert (Nothing, _) = Nothing
    convert (Just slot, bs) = convertToDiskPoint (SlotNo slot) bs

verifySnapshotPoint :: [SnapshotPoint] -> App [(CardanoPoint, Bool)]
verifySnapshotPoint snapPoints =
  catMaybes <$> mapM validLedgerFileToPoint snapPoints
  where
    validLedgerFileToPoint :: SnapshotPoint -> App (Maybe (CardanoPoint, Bool))
    validLedgerFileToPoint snapshotPoint =
      case snapshotPoint of
        OnDisk lsf -> do
          hashes <- getSlotHash (lsfSlotNo lsf)
          let valid = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
          case valid of
            Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convertToDiskPoint slot hash
            _other -> pure Nothing
        (InMemory pnt) -> do
          case pnt of
            GenesisPoint -> pure Nothing
            BlockPoint slotNo hsh -> do
              hashes <- getSlotHash slotNo
              let valid = find (\(_, dbHash) -> getHeaderHash hsh == dbHash) hashes
              case valid of
                Just (dbSlotNo, _) | slotNo == dbSlotNo -> pure $ Just (pnt, True)
                _other -> pure Nothing

convertToDiskPoint :: SlotNo -> ByteString -> Maybe (CardanoPoint, Bool)
convertToDiskPoint slot hashBlob = (,False) <$> convertToPoint slot hashBlob

convertToPoint :: SlotNo -> ByteString -> Maybe CardanoPoint
convertToPoint slot hashBlob =
  Point . Point.block slot <$> convertHashBlob hashBlob
  where
    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

getSecurityParam :: App Word64
getSecurityParam = do
  ledgerEnv <- asks envLedgerEnv
  pure $ case ledgerEnv of
    HasLedger hle -> getMaxRollbacks $ leProtocolInfo hle
    NoLedger nle -> getMaxRollbacks $ nleProtocolInfo nle

getMaxRollbacks ::
  (ConsensusProtocol (BlockProtocol blk)) =>
  ProtocolInfo blk ->
  Word64
getMaxRollbacks = maxRollbacks . configSecurityParam . pInfoConfig
