{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Ledger.State (
  applyBlock,
  defaultApplyResult,
  readCurrentStateUnsafe,
  getGovExpiresAt,
  mkHasLedgerEnv,
  applyBlockAndSnapshot,
  initCardanoLedgerState,
  loadLedgerAtPoint,
  hashToAnnotation,
  getHeaderHash,
  getStakeSlice,
  findProposedCommittee,
  writeLedgerState,
  ledgerDbCurrent,

  -- * Re-exports from Snapshot
  module Cardano.DbSync.Ledger.Snapshot,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncOptions (..))
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Cardano.Util as Cardano
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error (SyncNodeError (..), fromEitherSTM)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Ledger.Snapshot
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.BaseTypes (StrictMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core as Shelley
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Governance as Shelley
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), sumAdaPots)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import Cardano.Slotting.Slot (
  EpochNo (..),
  SlotNo (..),
  WithOrigin (..),
  fromWithOrigin,
 )
import Control.Concurrent.Class.MonadSTM.Strict (
  atomically,
  newTVarIO,
  readTVar,
  writeTVar,
 )
import Control.Concurrent.STM.TBQueue (newTBQueueIO)
import Control.ResourceRegistry (runWithTempRegistry)
import qualified Control.Tracer as Tracer
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.IO.Exception (userError)
import Lens.Micro ((%~), (^.), (^?))
import Ouroboros.Consensus.Block (
  WithOrigin (..),
  blockHash,
  blockIsEBB,
  blockPrevHash,
 )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block (ConwayEra, LedgerState (..))
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract (LedgerResult)
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend hiding (Trace)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMem
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq hiding (StateRef)
import Ouroboros.Network.Block (HeaderHash, pointSlot)
import System.FS.API (SomeHasFS (..), mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath (splitDirectories, (</>))
import System.Mem (performMajorGC)
import System.Random (genWord64, newStdGen)

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

-- -- | Create a simple in-memory 'LedgerTablesHandle' from 'LedgerTables ValuesMK'.
-- -- Each handle closes over an IORef that is written to at most once (by pushDiffs).
-- mkHandleFromValues ::
--   LedgerTables (ExtLedgerState CardanoBlock) ValuesMK ->
--   IO (LedgerTablesHandle IO (ExtLedgerState CardanoBlock))
-- mkHandleFromValues tables = do
--   ref <- newIORef tables
--   pure
--     LedgerTablesHandle
--       { close = pure ()
--       , transfer = \_ -> pure ()
--       , duplicate = \reg -> do
--           current <- readIORef ref
--           (rk, h) <- allocate reg (\_ -> mkHandleFromValues current) close
--           pure (rk, h)
--       , read = \_st keys -> do
--           vals <- readIORef ref
--           pure $ ltliftA2 restrictValuesMK vals keys
--       , readRange = \_st _ -> do
--           vals <- readIORef ref
--           pure (vals, Nothing)
--       , readAll = \_st -> readIORef ref
--       , pushDiffs = \_oldSt newSt -> do
--           let diffs = ltprj newSt
--           vals <- readIORef ref
--           writeIORef ref $ ltliftA2 applyDiffsMK vals diffs
--       , takeHandleSnapshot = \_st _name -> pure Nothing
--       , tablesSize = pure 0
--       }

-- | Push a new DbSyncStateRef and prune old ones. Returns the new DB and
-- any pruned DbSyncStateRefs whose handles should be closed.
pushLedgerDB :: LedgerDB -> DbSyncStateRef -> (LedgerDB, [DbSyncStateRef])
pushLedgerDB db st =
  pruneLedgerDb
    100
    db
      { ledgerDbCheckpoints = st SSeq.<| ledgerDbCheckpoints db
      }

-- | Prune snapshots until we have at most @k@ snapshots in the LedgerDB.
-- Returns the pruned DB and the dropped DbSyncStateRefs whose handles should be closed.
pruneLedgerDb :: Word64 -> LedgerDB -> (LedgerDB, [DbSyncStateRef])
pruneLedgerDb k db =
  let (!kept, !dropped) = SSeq.splitAt (fromIntegral k) (ledgerDbCheckpoints db)
   in (db {ledgerDbCheckpoints = kept}, toList dropped)
{-# INLINE pruneLedgerDb #-}

-- | The current DbSyncStateRef at the tip of the chain
ledgerDbCurrent :: LedgerDB -> DbSyncStateRef
ledgerDbCurrent db = case ledgerDbCheckpoints db of
  sr SSeq.:<| _ -> sr
  SSeq.Empty -> panic "ledgerDbCurrent: empty LedgerDB"

mkHasLedgerEnv ::
  Trace IO Text ->
  Consensus.ProtocolInfo CardanoBlock ->
  LedgerStateDir ->
  Ledger.Network ->
  Word64 ->
  SystemStart ->
  SyncOptions ->
  LedgerBackend ->
  IO HasLedgerEnv
mkHasLedgerEnv trce protoInfo dir nw maxLovelaceSupply systemStart syncOptions backend = do
  -- Clean up any legacy .lstate files from previous versions
  migrateOldSnapshots dir trce
  svar <- newTVarIO Strict.Nothing
  intervar <- newTVarIO Strict.Nothing
  snapQueue <- newTBQueueIO 5

  let codecConfig = configCodec $ Consensus.pInfoConfig protoInfo
      someHasFS = SomeHasFS $ ioHasFS (MountPoint $ unLedgerStateDir dir)
      snapTracer = Tracer.nullTracer -- TODO: wire up snapshot tracing
  (snapMgr, initGen, loadSnap) <- case backend of
    LedgerBackendInMemory -> do
      res <-
        runWithTempRegistry $
          (,())
            <$> mkResources (Proxy @CardanoBlock) Tracer.nullTracer InMem.InMemArgs someHasFS
      let sm = snapshotManager (Proxy @CardanoBlock) res codecConfig snapTracer someHasFS
          ig = do
            let initState = Consensus.pInfoInitLedger protoInfo
            createAndPopulateStateRefFromGenesis Tracer.nullTracer res initState
          ld ds = do
            eResult <-
              runExceptT $
                openStateRefFromSnapshot Tracer.nullTracer codecConfig someHasFS res ds
            case eResult of
              Left err -> pure $ Left $ textShow err
              Right (cRef, _pt) -> pure $ Right cRef
      pure (sm, ig, ld)
    LedgerBackendLSM mPath -> do
      let lsmPath = fromMaybe (unLedgerStateDir dir </> "lsm") mPath
      salt <- fst . genWord64 <$> newStdGen
      let args = LSM.LSMArgs (mkFsPath $ splitDirectories lsmPath) salt (LSM.stdMkBlockIOFS lsmPath)
      res <-
        runWithTempRegistry $
          (,())
            <$> mkResources (Proxy @CardanoBlock) Tracer.nullTracer args someHasFS
      let sm = snapshotManager (Proxy @CardanoBlock) res codecConfig snapTracer someHasFS
          ig = do
            let initState = Consensus.pInfoInitLedger protoInfo
            createAndPopulateStateRefFromGenesis Tracer.nullTracer res initState
          ld ds = do
            eResult <-
              runExceptT $
                openStateRefFromSnapshot Tracer.nullTracer codecConfig someHasFS res ds
            case eResult of
              Left err -> pure $ Left $ textShow err
              Right (cRef, _pt) -> pure $ Right cRef
      pure (sm, ig, ld)

  pure
    HasLedgerEnv
      { leTrace = trce
      , leUseLedger = ioUseLedger $ soptInsertOptions syncOptions
      , leHasRewards = ioRewards $ soptInsertOptions syncOptions
      , leProtocolInfo = protoInfo
      , leDir = dir
      , leNetwork = nw
      , leMaxSupply = maxLovelaceSupply
      , leSystemStart = systemStart
      , leAbortOnPanic = soptAbortOnInvalid syncOptions
      , leSnapshotNearTipEpoch = sicNearTipEpoch $ soptSnapshotInterval syncOptions
      , leInterpreter = intervar
      , leStateVar = svar
      , leSnapshotQueue = snapQueue
      , leLedgerBackend = backend
      , leSnapshotManager = snapMgr
      , leInitGenesis = initGen
      , leLoadSnapshot = loadSnap
      }

getTopLevelconfigHasLedger :: HasLedgerEnv -> TopLevelConfig CardanoBlock
getTopLevelconfigHasLedger = Consensus.pInfoConfig . leProtocolInfo

readCurrentStateUnsafe :: HasLedgerEnv -> IO (ExtLedgerState CardanoBlock EmptyMK)
readCurrentStateUnsafe hle =
  atomically
    (clsState . srState . ledgerDbCurrent <$> readStateUnsafe hle)

-- TODO make this type safe. We make the assumption here that the first message of
-- the chainsync protocol is 'RollbackTo'.
readStateUnsafe :: HasLedgerEnv -> STM LedgerDB
readStateUnsafe env = do
  mState <- readTVar $ leStateVar env
  case mState of
    Strict.Nothing -> throwSTM $ userError "LedgerState.readStateUnsafe: Ledger state is not found"
    Strict.Just st -> pure st

applyBlockAndSnapshot :: HasLedgerEnv -> CardanoBlock -> Bool -> IO (ApplyResult, Bool)
applyBlockAndSnapshot ledgerEnv blk isCons = do
  (oldRef, appResult, pruned) <- applyBlock ledgerEnv blk
  -- 864000 seconds = 10 days; consider synced "near tip" if within 10 days of current time
  tookSnapshot <- storeSnapshotAndCleanupMaybe ledgerEnv oldRef appResult isCons (isSyncedWithinSeconds (apSlotDetails appResult) 864000)
  -- Close pruned states. If a snapshot was taken, wait for the snapshot thread
  -- to finish writing before closing, since it may still need the table handles.
  forM_ pruned $ \sr -> do
    atomically $ readTVar (srCanClose sr) >>= check
    close (srTables sr)
  pure (appResult, tookSnapshot)

-- | Apply a block: delegates to tickThenReapplyCheckHash which handles
-- LedgerDB reads, handle duplication, block application, and LedgerDB update.
-- Returns the old DbSyncStateRef (for snapshotting), ApplyResult, and pruned DbSyncStateRefs (to close).
applyBlock :: HasLedgerEnv -> CardanoBlock -> IO (DbSyncStateRef, ApplyResult, [DbSyncStateRef])
applyBlock env blk = do
  time <- getCurrentTime
  !result <-
    either throwIO pure
      =<< tickThenReapplyCheckHash
        env
        (ExtLedgerCfg (getTopLevelconfigHasLedger env))
        blk
  let (oldRef, newResult, pruned) = result
      !oldCls = srState oldRef
  -- Build ApplyResult (STM for slot details and epoch detection)
  appResult <- atomically $ do
    let ledgerEventsFull = mapMaybe (convertAuxLedgerEvent (leHasRewards env)) (Consensus.lrEvents newResult)
        (ledgerEvents, deposits) = splitDeposits ledgerEventsFull
        !newLedgerState = finaliseDrepDistr $ clsState (Consensus.lrResult newResult)
    !details <- getSlotDetails env (ledgerState newLedgerState) time (cardanoBlockSlotNo blk)
    !newEpoch <- fromEitherSTM $ mkOnNewEpoch (clsState oldCls) newLedgerState (findAdaPots ledgerEvents)
    let !newEpochBlockNo = applyToEpochBlockNo (isByronLedger newLedgerState) (isJust newEpoch) (clsEpochBlockNo oldCls)
        !newState = (Consensus.lrResult newResult) {clsState = newLedgerState, clsEpochBlockNo = newEpochBlockNo}
    pure $
      if leUseLedger env
        then
          ApplyResult
            { apPrices = getPrices newState
            , apGovExpiresAfter = getGovExpiration newState
            , apPoolsRegistered = getRegisteredPools oldCls
            , apNewEpoch = maybeToStrict newEpoch
            , apOldLedger = Strict.Just oldCls
            , apDeposits = maybeToStrict $ Generic.getDeposits newLedgerState
            , apSlotDetails = details
            , apStakeSlice = getStakeSlice env newState False
            , apEvents = ledgerEvents
            , apGovActionState = getGovState newLedgerState
            , apDepositsMap = DepositsMap deposits
            }
        else defaultApplyResult details
  pure (oldRef, appResult, pruned)
  where
    mkOnNewEpoch :: ExtLedgerState CardanoBlock mk -> ExtLedgerState CardanoBlock mk -> Maybe AdaPots -> Either SyncNodeError (Maybe Generic.NewEpoch)
    mkOnNewEpoch oldState newState mPots = do
      -- pass on error when trying to get ledgerEpochNo
      case (prevEpochE, currEpochE) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right Nothing, Right (Just (EpochNo 0))) -> Right $ Just $ mkNewEpoch (EpochNo 0)
        (Right (Just prevEpoch), Right (Just currEpoch))
          | unEpochNo currEpoch == 1 + unEpochNo prevEpoch ->
              Right $ Just $ mkNewEpoch currEpoch
        _ -> Right Nothing
      where
        prevEpochE = ledgerEpochNo env oldState
        currEpochE = ledgerEpochNo env newState

        mkNewEpoch :: EpochNo -> Generic.NewEpoch
        mkNewEpoch currEpoch =
          Generic.NewEpoch
            { Generic.neEpoch = currEpoch
            , Generic.neIsEBB = isJust $ blockIsEBB blk
            , Generic.neAdaPots = fixUTxOPots <$> maybeToStrict mPots
            , Generic.neEpochUpdate = Generic.epochUpdate newState
            , Generic.neDRepState = maybeToStrict $ getDrepState newState
            , Generic.neEnacted = maybeToStrict $ getGovState newState
            , Generic.nePoolDistr = maybeToStrict $ Generic.getPoolDistr newState
            }

        fixUTxOPots :: AdaPots -> AdaPots
        fixUTxOPots adaPots =
          adaPots
            { utxoAdaPot =
                Coin $
                  fromIntegral (leMaxSupply env) - unCoin (sumAdaPots adaPots)
            }

    getDrepState :: ExtLedgerState CardanoBlock mk -> Maybe (DRepPulsingState ConwayEra)
    getDrepState ls = ls ^? newEpochStateT . Shelley.newEpochStateDRepPulsingStateL

    finaliseDrepDistr :: ExtLedgerState CardanoBlock mk -> ExtLedgerState CardanoBlock mk
    finaliseDrepDistr ledger =
      ledger & newEpochStateT %~ forceDRepPulsingState @ConwayEra

getGovState :: ExtLedgerState CardanoBlock mk -> Maybe (ConwayGovState ConwayEra)
getGovState ls = case ledgerState ls of
  LedgerStateConway cls ->
    Just $ Consensus.shelleyLedgerState cls ^. Shelley.newEpochStateGovStateL
  _ -> Nothing

getStakeSlice :: HasLedgerEnv -> CardanoLedgerState -> Bool -> Generic.StakeSliceRes
getStakeSlice env cls isMigration =
  case clsEpochBlockNo cls of
    EpochBlockNo n ->
      Generic.getStakeSlice
        (leProtocolInfo env)
        n
        (clsState cls)
        isMigration
    ByronEpochBlockNo -> Generic.NoSlices

storeSnapshotAndCleanupMaybe ::
  HasLedgerEnv ->
  DbSyncStateRef ->
  ApplyResult ->
  Bool ->
  SyncState ->
  IO Bool
storeSnapshotAndCleanupMaybe env oldState appResult isCons syncState =
  case maybeFromStrict (apNewEpoch appResult) of
    Just newEpoch
      | newEpochNo <- unEpochNo (Generic.neEpoch newEpoch)
      , newEpochNo > 0
      , -- Snapshot every epoch when near tip, every 10 epochs when lagging, or always for epoch >= threshold
        (isCons && syncState == SyncFollowing) || (newEpochNo `mod` 10 == 0) || newEpochNo >= leSnapshotNearTipEpoch env ->
          do
            -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
            liftIO $ saveCleanupState env oldState (Just $ EpochNo $ newEpochNo - 1)
            pure True
    _ -> pure False

hashToAnnotation :: ByteString -> ByteString
hashToAnnotation = Base16.encode . BS.take 5

-- -------------------------------------------------------------------------------------------------

loadLedgerAtPoint :: HasLedgerEnv -> CardanoPoint -> IO (Either [DiskSnapshot] CardanoLedgerState)
loadLedgerAtPoint hasLedgerEnv point = do
  mLedgerDB <- atomically $ readTVar $ leStateVar hasLedgerEnv
  let (mStates, dropped) = rollbackLedger mLedgerDB
  case mStates of
    Nothing -> do
      writeLedgerState hasLedgerEnv Strict.Nothing
      closeDroppedHandles dropped
      performMajorGC
      mst <- findStateFromSnapshot hasLedgerEnv point
      case mst of
        Right sr -> do
          writeLedgerState hasLedgerEnv (Strict.Just . LedgerDB $ SSeq.singleton sr)
          logInfo (leTrace hasLedgerEnv) $ mconcat ["Found snapshot for ", renderPoint point]
          pure $ Right (srState sr)
        Left dss -> pure $ Left dss
    Just states' -> do
      logInfo (leTrace hasLedgerEnv) $ mconcat ["Found in memory ledger snapshot at ", renderPoint point]
      let ledgerDB' = LedgerDB states'
      let sr = ledgerDbCurrent ledgerDB'
      deleteNewerSnapshots hasLedgerEnv point
      writeLedgerState hasLedgerEnv $ Strict.Just ledgerDB'
      closeDroppedHandles dropped
      pure $ Right (srState sr)
  where
    -- \| Returns (kept states or Nothing, dropped states to close)
    rollbackLedger ::
      Strict.Maybe LedgerDB ->
      (Maybe (StrictSeq DbSyncStateRef), [DbSyncStateRef])
    rollbackLedger mLedgerDB = case mLedgerDB of
      Strict.Nothing -> (Nothing, [])
      Strict.Just ledgerDB ->
        let allEntries = toList $ ledgerDbCheckpoints ledgerDB
            (newer, older) =
              List.span
                (\sr -> Consensus.getTipSlot (clsState (srState sr)) > pointSlot point)
                allEntries
            kept = SSeq.fromList older
         in if SSeq.null kept
              then (Nothing, allEntries)
              else (Just kept, newer)

    -- \| Close handles from dropped states.
    -- Waits for the snapshot writer to finish if any handle is still being used.
    closeDroppedHandles :: [DbSyncStateRef] -> IO ()
    closeDroppedHandles refs = forM_ refs $ \sr -> do
      atomically $ readTVar (srCanClose sr) >>= check
      close (srTables sr)

writeLedgerState :: HasLedgerEnv -> Strict.Maybe LedgerDB -> IO ()
writeLedgerState env mLedgerDb = atomically $ writeTVar (leStateVar env) mLedgerDb

getRegisteredPools :: CardanoLedgerState -> Set.Set PoolKeyHash
getRegisteredPools st =
  case ledgerState $ clsState st of
    LedgerStateByron _ -> Set.empty
    LedgerStateShelley sts -> getRegisteredPoolShelley sts
    LedgerStateAllegra sts -> getRegisteredPoolShelley sts
    LedgerStateMary sts -> getRegisteredPoolShelley sts
    LedgerStateAlonzo ats -> getRegisteredPoolShelley ats
    LedgerStateBabbage bts -> getRegisteredPoolShelley bts
    LedgerStateConway stc -> getRegisteredPoolShelley stc
    LedgerStateDijkstra stc -> getRegisteredPoolShelley stc

getRegisteredPoolShelley ::
  forall p era mk.
  Shelley.EraCertState era =>
  LedgerState (ShelleyBlock p era) mk ->
  Set.Set PoolKeyHash
getRegisteredPoolShelley lState =
  Map.keysSet $
    let certState =
          Shelley.lsCertState $
            Shelley.esLState $
              Shelley.nesEs $
                Consensus.shelleyLedgerState lState
     in certState ^. Shelley.certPStateL . Shelley.psStakePoolsL

isByronLedger :: ExtLedgerState CardanoBlock mk -> Bool
isByronLedger st = case ledgerState st of
  LedgerStateByron _ -> True
  _ -> False

applyToEpochBlockNo :: Bool -> Bool -> EpochBlockNo -> EpochBlockNo
applyToEpochBlockNo True _ _ = ByronEpochBlockNo -- Byron era
applyToEpochBlockNo _ True _ = EpochBlockNo 0 -- Shelley+ new epoch
applyToEpochBlockNo _ _ (EpochBlockNo n) = EpochBlockNo (n + 1)
applyToEpochBlockNo _ _ ByronEpochBlockNo = EpochBlockNo 0 -- first Shelley block

ledgerEpochNo :: HasLedgerEnv -> ExtLedgerState CardanoBlock mk -> Either SyncNodeError (Maybe EpochNo)
ledgerEpochNo env cls =
  case Consensus.ledgerTipSlot (ledgerState cls) of
    Origin -> Right Nothing
    NotOrigin slot ->
      case runExcept $ epochInfoEpoch epochInfo slot of
        Left err -> Left $ SNErrLedgerState $ "unable to use slot: " <> show slot <> "to get ledgerEpochNo: " <> show err
        Right en -> Right (Just en)
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo = epochInfoLedger (configLedger $ getTopLevelconfigHasLedger env) (hardForkLedgerStatePerEra $ ledgerState cls)

-- | Apply a block to the current LedgerDB state. Reads the current DbSyncStateRef,
-- duplicates its handle, applies the block, pushes diffs, creates a new DbSyncStateRef,
-- and updates the LedgerDB. Returns the old DbSyncStateRef (for snapshotting),
-- the new CardanoLedgerState (for ApplyResult), and pruned DbSyncStateRefs (to close).
tickThenReapplyCheckHash ::
  HasLedgerEnv ->
  ExtLedgerCfg CardanoBlock ->
  CardanoBlock ->
  IO
    ( Either
        SyncNodeError
        ( DbSyncStateRef -- old state ref (for snapshotting)
        , LedgerResult (ExtLedgerState CardanoBlock) CardanoLedgerState -- new state
        , [DbSyncStateRef] -- pruned refs to close
        )
    )
tickThenReapplyCheckHash env cfg block = do
  -- Read the current state from LedgerDB
  (ledgerDB, oldRef) <- atomically $ do
    !db <- readStateUnsafe env
    pure (db, ledgerDbCurrent db)
  let !oldCls = srState oldRef
  if blockPrevHash block == Consensus.ledgerTipHash (ledgerState (clsState oldCls))
    then do
      -- Create a new handle first, then read from it
      let keys = Consensus.getBlockKeySets block
      restrictedTables <- read (srTables oldRef) (clsState oldCls) keys
      -- Attach the tables to the ledger state and apply the block
      let ledgerState' = Consensus.withLedgerTables (clsState oldCls) restrictedTables
          newLedgerResult =
            Consensus.tickThenReapplyLedgerResult Consensus.ComputeLedgerEvents cfg block ledgerState'
          newLedgerState = forgetLedgerTables $ Consensus.lrResult newLedgerResult
          isNewEpoch = case (ledgerEpochNo env (clsState oldCls), ledgerEpochNo env newLedgerState) of
            (Right oldE, Right newE) -> oldE /= newE
            _ -> False
          !newEpochBlockNo = applyToEpochBlockNo (isByronLedger newLedgerState) isNewEpoch (clsEpochBlockNo oldCls)
          -- Build pure CardanoLedgerState from result
          newCls =
            fmap
              ( \stt ->
                  CardanoLedgerState
                    { clsState = forgetLedgerTables stt
                    , clsEpochBlockNo = newEpochBlockNo
                    }
              )
              newLedgerResult
      -- Push diffs to the new handle
      newHandle <- duplicateWithDiffs (srTables oldRef) (clsState oldCls) (Consensus.lrResult newLedgerResult)
      -- Create new DbSyncStateRef and push to LedgerDB
      canClose <- newTVarIO True
      let !newRef =
            DbSyncStateRef
              { srState = Consensus.lrResult newCls
              , srTables = newHandle
              , srCanClose = canClose
              }
      let (!ledgerDB', !prunedRefs) = pushLedgerDB ledgerDB newRef
      atomically $ writeTVar (leStateVar env) (Strict.Just ledgerDB')
      pure $ Right (oldRef, newCls, prunedRefs)
    else
      pure . Left $
        SNErrLedgerState $
          mconcat
            [ "Ledger state hash mismatch. Ledger head is slot "
            , show
                ( unSlotNo $
                    fromWithOrigin
                      (SlotNo 0)
                      (Consensus.ledgerTipSlot $ ledgerState (clsState oldCls))
                )
            , " hash "
            , Text.unpack $
                renderByteArray (Cardano.unChainHash (Consensus.ledgerTipHash $ ledgerState (clsState oldCls)))
            , " but block previous hash is "
            , Text.unpack $
                renderByteArray (Cardano.unChainHash $ blockPrevHash block)
            , " and block current hash is "
            , Text.unpack $
                renderByteArray (SBS.fromShort . Consensus.getOneEraHash $ blockHash block)
            , "."
            ]

getHeaderHash :: HeaderHash CardanoBlock -> ByteString
getHeaderHash bh = SBS.fromShort (Consensus.getOneEraHash bh)

getSlotDetails :: HasLedgerEnv -> LedgerState CardanoBlock mk -> UTCTime -> SlotNo -> STM SlotDetails
getSlotDetails env st time slot = do
  minter <- readTVar $ leInterpreter env
  details <- case minter of
    Strict.Just inter -> case queryWith inter of
      Left _ -> queryNewInterpreter
      Right sd -> pure sd
    Strict.Nothing -> queryNewInterpreter
  pure $ details {sdCurrentTime = time}
  where
    hfConfig = configLedger $ getTopLevelconfigHasLedger env

    queryNewInterpreter :: STM SlotDetails
    queryNewInterpreter =
      let !inter = History.mkInterpreter $ hardForkSummary hfConfig st
       in case queryWith inter of
            Left err -> throwSTM err
            Right sd -> do
              writeTVar (leInterpreter env) (Strict.Just inter)
              pure sd

    queryWith :: CardanoInterpreter -> Either History.PastHorizonException SlotDetails
    queryWith inter =
      History.interpretQuery inter (querySlotDetails (leSystemStart env) slot)

getPrices :: CardanoLedgerState -> Strict.Maybe Prices
getPrices st = case ledgerState $ clsState st of
  LedgerStateAlonzo als ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState als)
          ^. Shelley.curPParamsEpochStateL
            . Alonzo.ppPricesL
      )
  LedgerStateBabbage bls ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState bls)
          ^. Shelley.curPParamsEpochStateL
            . Alonzo.ppPricesL
      )
  LedgerStateConway bls ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState bls)
          ^. Shelley.curPParamsEpochStateL
            . Alonzo.ppPricesL
      )
  _ -> Strict.Nothing

getGovExpiration :: CardanoLedgerState -> Strict.Maybe Ledger.EpochInterval
getGovExpiration st = case ledgerState $ clsState st of
  LedgerStateConway bls ->
    Strict.Just $
      Shelley.nesEs (Consensus.shelleyLedgerState bls)
        ^. (Shelley.curPParamsEpochStateL . Shelley.ppGovActionLifetimeL)
  _ -> Strict.Nothing

findAdaPots :: [LedgerEvent] -> Maybe AdaPots
findAdaPots = go
  where
    go [] = Nothing
    go (LedgerAdaPots p : _) = Just p
    go (_ : rest) = go rest

-- | Given an committee action id and the current GovState, return the proposed committee.
-- If it's not a Committee action or is not included in the proposals, return Nothing.
findProposedCommittee :: GovActionId -> ConwayGovState ConwayEra -> Either Text (Maybe (Committee ConwayEra))
findProposedCommittee gaId cgs = do
  (rootCommittee, updateList) <- findRoot gaId
  computeCommittee rootCommittee updateList
  where
    ps = cgsProposals cgs
    findRoot = findRootRecursively []

    findRootRecursively :: [GovAction ConwayEra] -> GovActionId -> Either Text (StrictMaybe (Committee ConwayEra), [GovAction ConwayEra])
    findRootRecursively acc gid = do
      gas <- fromNothing ("Didn't find proposal " <> textShow gid) $ proposalsLookupId gid ps
      let ga = pProcGovAction (gasProposalProcedure gas)
      case ga of
        NoConfidence _ -> Right (Ledger.SNothing, acc)
        UpdateCommittee Ledger.SNothing _ _ _ -> Right (cgsCommittee cgs, ga : acc)
        UpdateCommittee gpid _ _ _
          | gpid == ps ^. pRootsL . grCommitteeL . prRootL ->
              Right (cgsCommittee cgs, ga : acc)
        UpdateCommittee (Ledger.SJust gpid) _ _ _ -> findRootRecursively (ga : acc) (unGovPurposeId gpid)
        _ -> Left "Found invalid gov action referenced by committee"

    computeCommittee :: StrictMaybe (Committee ConwayEra) -> [GovAction ConwayEra] -> Either Text (Maybe (Committee ConwayEra))
    computeCommittee sCommittee actions =
      Ledger.strictMaybeToMaybe <$> foldM applyCommitteeUpdate sCommittee actions

    applyCommitteeUpdate scommittee = \case
      UpdateCommittee _ toRemove toAdd q -> Right $ Ledger.SJust $ updatedCommittee toRemove toAdd q scommittee
      _ -> Left "Unexpected gov action." -- Should never happen since the accumulator only includes UpdateCommittee
    fromNothing err = maybe (Left err) Right
