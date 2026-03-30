{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Ledger.State (
  applyBlock,
  defaultApplyResult,
  readCurrentStateUnsafe,
  getGovExpiresAt,
  mkHasLedgerEnv,
  mkHandleFromValues,
  applyBlockAndSnapshot,
  listLedgerStateFilesOrdered,
  listKnownSnapshots,
  loadLedgerStateFromFile,
  findLedgerStateFile,
  loadLedgerAtPoint,
  hashToAnnotation,
  getHeaderHash,
  runLedgerStateWriteThread,
  getStakeSlice,
  findProposedCommittee,
  writeLedgerState,
  saveCleanupState,
  ledgerDbCurrent,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Cardano.Binary (Decoder, DecoderError, Encoding)
import qualified Cardano.Binary as Serialize
import Cardano.DbSync.Api.Types (InsertOptions (..), LedgerEnv (..), SyncOptions (..))
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Cardano.Util as Cardano
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error (SyncNodeError (..), fromEitherSTM)
import Cardano.DbSync.Ledger.Event
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
  at,
  fromWithOrigin,
 )
import Control.Concurrent.Class.MonadSTM.Strict (
  atomically,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import qualified Control.Exception as Exception
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Exception (userError)
import Lens.Micro ((%~), (^.), (^?))
import Ouroboros.Consensus.Block (
  CodecConfig,
  Point (..),
  WithOrigin (..),
  blockHash,
  blockIsEBB,
  blockPrevHash,
  castPoint,
  pointSlot,
 )
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
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
import Control.ResourceRegistry (ResourceRegistry, allocate, unsafeNewRegistry)
import Data.IORef (newIORef, readIORef, writeIORef)
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerTables, ValuesMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import Ouroboros.Consensus.Ledger.Tables.Combinators (ltliftA2)
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffsMK, forgetLedgerTables, ltprj, restrictValuesMK)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend (Backend (mkResources, newHandleFromValues))
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq (LedgerTablesHandle (..))
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM (Args (LSMArgs), newLSMLedgerTablesHandle, sessionResource, stdMkBlockIOFS)
import Data.String (fromString)
import qualified Database.LSMTree as LSMTree
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import Ouroboros.Network.Block (HeaderHash, Point (..))
import qualified Ouroboros.Network.Point as Point
import System.Directory (doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (dropExtension, splitDirectories, takeExtension, takeFileName, (</>))
import System.FS.API (SomeHasFS (..), mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.Mem (performMajorGC)
import System.Random (genWord64, newStdGen)
import qualified Control.Tracer as Tracer
import Prelude (String)

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

-- | Create a simple in-memory 'LedgerTablesHandle' from 'LedgerTables ValuesMK'.
-- Each handle closes over an IORef that is written to at most once (by pushDiffs).
mkHandleFromValues ::
  LedgerTables (ExtLedgerState CardanoBlock) ValuesMK ->
  IO (LedgerTablesHandle IO (ExtLedgerState CardanoBlock))
mkHandleFromValues tables = do
  ref <- newIORef tables
  pure
    LedgerTablesHandle
      { close = pure ()
      , transfer = \_ -> pure ()
      , duplicate = \reg -> do
          current <- readIORef ref
          (rk, h) <- allocate reg (\_ -> mkHandleFromValues current) close
          pure (rk, h)
      , read = \_st keys -> do
          vals <- readIORef ref
          pure $ ltliftA2 restrictValuesMK vals keys
      , readRange = \_st _ -> do
          vals <- readIORef ref
          pure (vals, Nothing)
      , readAll = \_st -> readIORef ref
      , pushDiffs = \_oldSt newSt -> do
          let diffs = ltprj newSt
          vals <- readIORef ref
          writeIORef ref $ ltliftA2 applyDiffsMK vals diffs
      , takeHandleSnapshot = \_st _name -> pure Nothing
      , tablesSize = pure 0
      }

-- | Push a new StateRef and prune old ones. Returns the new DB and
-- any pruned StateRefs whose handles should be closed.
pushLedgerDB :: LedgerDB -> StateRef -> (LedgerDB, [StateRef])
pushLedgerDB db st =
  pruneLedgerDb
    5
    db
      { ledgerDbCheckpoints = st SSeq.<| ledgerDbCheckpoints db
      }

-- | Prune snapshots until we have at most @k@ snapshots in the LedgerDB.
-- Returns the pruned DB and the dropped StateRefs whose handles should be closed.
pruneLedgerDb :: Word64 -> LedgerDB -> (LedgerDB, [StateRef])
pruneLedgerDb k db =
  let (!kept, !dropped) = SSeq.splitAt (fromIntegral k) (ledgerDbCheckpoints db)
  in (db {ledgerDbCheckpoints = kept}, toList dropped)
{-# INLINE pruneLedgerDb #-}

-- | The current StateRef at the tip of the chain
ledgerDbCurrent :: LedgerDB -> StateRef
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
  svar <- newTVarIO Strict.Nothing
  intervar <- newTVarIO Strict.Nothing
  swQueue <- newTBQueueIO 5 -- Should be relatively shallow.
  -- Registry must be created lazily from the chain sync thread, as
  -- ResourceRegistry enforces thread tracking.
  registryVar <- newTVarIO Strict.Nothing
  let getRegistry = readTVarIO registryVar >>= \case
        Strict.Just reg -> pure reg
        Strict.Nothing -> do
          reg <- unsafeNewRegistry
          atomically $ writeTVar registryVar (Strict.Just reg)
          pure reg
  (mkHandle, mkHandleFromSnap) <- case backend of
    LedgerBackendInMemory ->
      pure (\_st tables -> mkHandleFromValues tables, Nothing)
    LedgerBackendLSM mPath -> do
      let lsmPath = fromMaybe (unLedgerStateDir dir </> "lsm") mPath
          -- lsmTracer = Tracer.Tracer $ \msg -> logInfo trce (Text.pack $ show msg)
          lsmTracer = Tracer.Tracer $ \_ -> pure ()
      gen <- newStdGen
      let (salt, _) = genWord64 gen
          args = LSMArgs (mkFsPath $ splitDirectories lsmPath) salt (stdMkBlockIOFS lsmPath)
      resourcesVar <- newTVarIO Strict.Nothing
      let getResources = do
            reg <- getRegistry
            readTVarIO resourcesVar >>= \case
              Strict.Just res -> pure (reg, res)
              Strict.Nothing -> do
                res <- mkResources (Proxy :: Proxy CardanoBlock) lsmTracer args reg (SomeHasFS $ ioHasFS $ MountPoint lsmPath)
                atomically $ writeTVar resourcesVar (Strict.Just res)
                pure (reg, res)
      let mkH = \st tables -> do
            (reg, resources) <- getResources
            newHandleFromValues lsmTracer reg resources (Consensus.withLedgerTables st tables)
      let handleFromSnap = \snapshotName -> do
            (reg, resources) <- getResources
            let session = sessionResource resources
            (rk, table) <- allocate reg
              (\_ -> LSMTree.openTableFromSnapshot session (fromString snapshotName) (LSMTree.SnapshotLabel "UTxO table"))
              LSMTree.closeTable
            newLSMLedgerTablesHandle lsmTracer 0 (rk, table)
      pure (mkH, Just handleFromSnap)
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
      , leStateWriteQueue = swQueue
      , leRegistry = getRegistry
      , leLedgerBackend = backend
      , leMkLedgerHandle = mkHandle
      , leMkLedgerHandleFromSnapshot = mkHandleFromSnap
      }

initCardanoLedgerState :: HasLedgerEnv -> IO StateRef
initCardanoLedgerState env = do
  let initState = Consensus.pInfoInitLedger (leProtocolInfo env)
  tablesHandle <- leMkLedgerHandle env (forgetLedgerTables initState) (Consensus.projectLedgerTables initState)
  canClose <- newTVarIO True
  pure
    StateRef
      { srState = CardanoLedgerState
          { clsState = forgetLedgerTables initState
          , clsEpochBlockNo = GenesisEpochBlockNo
          }
      , srTables = tablesHandle
      , srCanClose = canClose
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
    -- atomically $ readTVar (srCanClose sr) >>= check
    close (srTables sr)
  pure (appResult, tookSnapshot)

-- | Apply a block: delegates to tickThenReapplyCheckHash which handles
-- LedgerDB reads, handle duplication, block application, and LedgerDB update.
-- Returns the old StateRef (for snapshotting), ApplyResult, and pruned StateRefs (to close).
applyBlock :: HasLedgerEnv -> CardanoBlock -> IO (StateRef, ApplyResult, [StateRef])
applyBlock env blk = do
  time <- getCurrentTime
  reg <- leRegistry env
  !result <-
    either throwIO pure =<<
      tickThenReapplyCheckHash
        env
        reg
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
    let !newEpochBlockNo = applyToEpochBlockNo (isJust $ blockIsEBB blk) (isJust newEpoch) (clsEpochBlockNo oldCls)
        !newState = (Consensus.lrResult newResult) { clsState = newLedgerState, clsEpochBlockNo = newEpochBlockNo }
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

    applyToEpochBlockNo :: Bool -> Bool -> EpochBlockNo -> EpochBlockNo
    applyToEpochBlockNo True _ _ = EBBEpochBlockNo
    applyToEpochBlockNo _ True _ = EpochBlockNo 0
    applyToEpochBlockNo _ _ (EpochBlockNo n) = EpochBlockNo (n + 1)
    applyToEpochBlockNo _ _ GenesisEpochBlockNo = EpochBlockNo 0
    applyToEpochBlockNo _ _ EBBEpochBlockNo = EpochBlockNo 0

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
    _ -> Generic.NoSlices

storeSnapshotAndCleanupMaybe ::
  HasLedgerEnv ->
  StateRef ->
  ApplyResult ->
  Bool ->
  SyncState ->
  IO Bool
storeSnapshotAndCleanupMaybe _ _ _ _ _ = pure False
-- storeSnapshotAndCleanupMaybe env oldState appResult isCons syncState =
--   case maybeFromStrict (apNewEpoch appResult) of
--     Just newEpoch
--       | newEpochNo <- unEpochNo (Generic.neEpoch newEpoch)
--       , newEpochNo > 0
--       , -- Snapshot every epoch when near tip, every 10 epochs when lagging, or always for epoch >= threshold
--         (isCons && syncState == SyncFollowing) || (newEpochNo `mod` 10 == 0) || newEpochNo >= leSnapshotNearTipEpoch env ->
--           do
--             -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
--             liftIO $ saveCleanupState env oldState (Just $ EpochNo $ newEpochNo - 1)
--             pure True
--     _ -> pure False

saveCurrentLedgerState :: HasLedgerEnv -> StateRef -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env lState mEpochNo = do
  case mkLedgerStateFilename (leDir env) (clsState (srState lState)) mEpochNo of
    Origin -> pure () -- we don't store genesis
    At file -> do
      exists <- doesFileExist file
      if exists
        then
          logInfo (leTrace env) $
            mconcat
              ["File ", Text.pack file, " exists"]
        else do
          atomically $ writeTVar (srCanClose lState) False
          atomically $ writeTBQueue (leStateWriteQueue env) (file, lState)

runLedgerStateWriteThread :: Trace IO Text -> LedgerEnv -> IO ()
runLedgerStateWriteThread tracer lenv =
  case lenv of
    HasLedger le -> ledgerStateWriteLoop tracer (leStateWriteQueue le) (configCodec $ getTopLevelconfigHasLedger le) (leLedgerBackend le)
    NoLedger _ -> forever $ threadDelay 600000000 -- 10 minutes

ledgerStateWriteLoop :: Trace IO Text -> TBQueue (FilePath, StateRef) -> CodecConfig CardanoBlock -> LedgerBackend -> IO ()
ledgerStateWriteLoop tracer swQueue codecConfig backend =
  loop
  where
    loop :: IO ()
    loop = do
      (file, ledger) <- atomically $ readTBQueue swQueue -- Blocks until the queue has elements.
      writeLedgerStateFile file ledger
      atomically $ writeTVar (srCanClose ledger) True
      loop

    encodeExt :: ExtLedgerState CardanoBlock EmptyMK -> Encoding
    encodeExt =
      Consensus.encodeExtLedgerState
        (encodeDisk codecConfig)
        (encodeDisk codecConfig)
        (encodeDisk codecConfig)
        . forgetLedgerTables

    writeLedgerStateFile :: FilePath -> StateRef -> IO ()
    writeLedgerStateFile file ledger = do
      let cls = srState ledger
      startTime <- getCurrentTime
      case backend of
        LedgerBackendInMemory -> do
          -- Read all tables from the handle for serialization (single file).
          tables <- readAll (srTables ledger) (clsState cls)
          LBS.writeFile file $
            Serialize.serialize $
              encodeCardanoLedgerStateInMemory
                encodeExt
                (clsState cls)
                (clsEpochBlockNo cls)
                tables
        LedgerBackendLSM _mPath -> do
          -- LSM: tables are on disk, only encode state + epoch.
          LBS.writeFile file $
            Serialize.serialize $
              encodeCardanoLedgerStateLSM
                encodeExt
                (clsState cls)
                (clsEpochBlockNo cls)
          -- Save LSM tables snapshot alongside the state file.
          let snapshotName = dropExtension $ takeFileName file
          void $ takeHandleSnapshot (srTables ledger) (clsState cls) snapshotName
      endTime <- getCurrentTime
      logInfo tracer $
        mconcat
          [ "Asynchronously wrote a ledger snapshot to "
          , Text.pack file
          , " in "
          , textShow (diffUTCTime endTime startTime)
          , "."
          ]

mkLedgerStateFilename :: LedgerStateDir -> ExtLedgerState CardanoBlock mk -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo =
  lsfFilePath
    . dbPointToFileName dir mEpochNo
    <$> getPoint (Consensus.ledgerTipPoint @CardanoBlock (ledgerState ledger))

saveCleanupState :: HasLedgerEnv -> StateRef -> Maybe EpochNo -> IO ()
saveCleanupState env ledger mEpochNo = do
  let st = clsState (srState ledger)
  saveCurrentLedgerState env ledger mEpochNo
  cleanupLedgerStateFiles env $
    fromWithOrigin (SlotNo 0) (Consensus.ledgerTipSlot $ ledgerState st)

hashToAnnotation :: ByteString -> ByteString
hashToAnnotation = Base16.encode . BS.take 5

mkRawHash :: HeaderHash CardanoBlock -> ByteString
mkRawHash = toRawHash (Proxy @CardanoBlock)

mkShortHash :: HeaderHash CardanoBlock -> ByteString
mkShortHash = hashToAnnotation . mkRawHash

dbPointToFileName :: LedgerStateDir -> Maybe EpochNo -> Point.Block SlotNo (HeaderHash CardanoBlock) -> LedgerStateFile
dbPointToFileName (LedgerStateDir stateDir) mEpochNo (Point.Block slot hash) =
  LedgerStateFile
    { lsfSlotNo = slot
    , lsfHash = shortHash
    , lsNewEpoch = maybeToStrict mEpochNo
    , lsfFilePath =
        mconcat
          [ stateDir </> show (unSlotNo slot)
          , "-"
          , BS.unpack shortHash
          , epochSuffix
          , ".lstate"
          ]
    }
  where
    shortHash :: ByteString
    shortHash = mkShortHash hash

    epochSuffix :: String
    epochSuffix =
      case mEpochNo of
        Nothing -> ""
        Just epoch -> "-" ++ show (unEpochNo epoch)

parseLedgerStateFileName :: LedgerStateDir -> FilePath -> Maybe LedgerStateFile
parseLedgerStateFileName (LedgerStateDir stateDir) fp =
  case break (== '-') (dropExtension fp) of
    (slotStr, '-' : hashEpoch) -> do
      slot <- readMaybe slotStr
      case break (== '-') hashEpoch of
        (hash, '-' : suffix) | Just epochNo <- readMaybe suffix -> do
          Just $ build (BS.pack hash) slot (Just epochNo)
        (hash, "") ->
          Just $ build (BS.pack hash) slot Nothing
        _otherwise -> Nothing
    _otherwise -> Nothing
  where
    build :: ByteString -> Word64 -> Maybe Word64 -> LedgerStateFile
    build hash slot mEpochNo =
      LedgerStateFile
        { lsfSlotNo = SlotNo slot
        , lsfHash = hash
        , lsNewEpoch = maybeToStrict $ EpochNo <$> mEpochNo
        , lsfFilePath = stateDir </> fp
        }

-- -------------------------------------------------------------------------------------------------

cleanupLedgerStateFiles :: HasLedgerEnv -> SlotNo -> IO ()
cleanupLedgerStateFiles env slotNo = do
  files <- listLedgerStateFilesOrdered (leDir env)
  let (epochBoundary, valid, invalid) = foldr groupFiles ([], [], []) files
  -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
  deleteAndLogFiles env "invalid" invalid
  -- Remove all but 2 most recent state files.
  deleteAndLogStateFile env "old" (List.drop 2 valid)
  -- Remove all but 3 most recent epoch boundary state files.
  deleteAndLogStateFile env "old epoch boundary" (List.drop 3 epochBoundary)
  where
    groupFiles ::
      LedgerStateFile ->
      ([LedgerStateFile], [LedgerStateFile], [FilePath]) ->
      ([LedgerStateFile], [LedgerStateFile], [FilePath]) -- (epochBoundary, valid, invalid)
    groupFiles lFile (epochBoundary, regularFile, invalid)
      | lsfSlotNo lFile > slotNo =
          (epochBoundary, regularFile, lsfFilePath lFile : invalid)
      | Strict.Just _ <- lsNewEpoch lFile =
          (lFile : epochBoundary, regularFile, invalid)
      | otherwise =
          (epochBoundary, lFile : regularFile, invalid)

loadLedgerAtPoint :: HasLedgerEnv -> CardanoPoint -> IO (Either [LedgerStateFile] CardanoLedgerState)
loadLedgerAtPoint hasLedgerEnv point = do
  mLedgerDB <- atomically $ readTVar $ leStateVar hasLedgerEnv
  -- First try to find the ledger in memory
  let mStates = rollbackLedger mLedgerDB
  case mStates of
    Nothing -> do
      -- Ledger states are growing to become very big in memory.
      -- Before parsing the new ledger state we need to make sure the old states
      -- are or can be garbage collected.
      -- TODO: re-enable once we duplicate handles before queuing snapshots
      -- case mLedgerDB of
      --   Strict.Nothing -> pure ()
      --   Strict.Just db -> mapM_ (close . clsTables) (reverse . NE.toList $ ledgerDbCheckpoints db)
      writeLedgerState hasLedgerEnv Strict.Nothing
      performMajorGC
      mst <- findStateFromPoint hasLedgerEnv point
      case mst of
        Right sr -> do
          writeLedgerState hasLedgerEnv (Strict.Just . LedgerDB $ SSeq.singleton sr)
          logInfo (leTrace hasLedgerEnv) $ mconcat ["Found snapshot file for ", renderPoint point]
          pure $ Right (srState sr)
        Left lsfs -> pure $ Left lsfs
    Just states' -> do
      logInfo (leTrace hasLedgerEnv) $ mconcat ["Found in memory ledger snapshot at ", renderPoint point]
      let ledgerDB' = LedgerDB states'
      let sr = ledgerDbCurrent ledgerDB'
      deleteNewerFiles hasLedgerEnv point
      writeLedgerState hasLedgerEnv $ Strict.Just ledgerDB'
      pure $ Right (srState sr)
  where
    rollbackLedger ::
      Strict.Maybe LedgerDB ->
      Maybe (StrictSeq StateRef)
    rollbackLedger mLedgerDB = case mLedgerDB of
      Strict.Nothing -> Nothing
      Strict.Just ledgerDB ->
        -- Drop states newer than the rollback point (list is newest-first)
        let kept = SSeq.fromList $ dropWhile
              (\sr -> Consensus.getTipSlot (clsState (srState sr)) > pointSlot point)
              (toList $ ledgerDbCheckpoints ledgerDB)
        in if SSeq.null kept then Nothing else Just kept

deleteNewerFiles :: HasLedgerEnv -> CardanoPoint -> IO ()
deleteNewerFiles env point = do
  files <- listLedgerStateFilesOrdered (leDir env)
  -- Genesis can be reproduced from configuration.
  -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      deleteAndLogStateFile env "newer" files
    At blk -> do
      let (newerFiles, _found, _olderFiles) =
            findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      deleteAndLogStateFile env "newer" newerFiles

deleteAndLogFiles :: HasLedgerEnv -> Text -> [FilePath] -> IO ()
deleteAndLogFiles env descr files =
  case files of
    [] -> pure ()
    [fl] -> do
      logInfo (leTrace env) $ mconcat ["Removing ", descr, " file ", Text.pack fl]
      removeStateFileAndLSMSnapshot env fl
    _ -> do
      logInfo (leTrace env) $ mconcat ["Removing ", descr, " files ", textShow files]
      mapM_ (removeStateFileAndLSMSnapshot env) files

-- | Remove a ledger state file and its corresponding LSM snapshot directory (if any).
removeStateFileAndLSMSnapshot :: HasLedgerEnv -> FilePath -> IO ()
removeStateFileAndLSMSnapshot env fp = do
  safeRemoveFile fp
  case leLedgerBackend env of
    LedgerBackendLSM mPath -> do
      let lsmPath = fromMaybe (unLedgerStateDir (leDir env) </> "lsm") mPath
          snapshotName = dropExtension $ takeFileName fp
      safeRemoveDirectory (lsmPath </> "snapshots" </> snapshotName)
    LedgerBackendInMemory -> pure ()

deleteAndLogStateFile :: HasLedgerEnv -> Text -> [LedgerStateFile] -> IO ()
deleteAndLogStateFile env descr lsfs = deleteAndLogFiles env descr (lsfFilePath <$> lsfs)

findStateFromPoint :: HasLedgerEnv -> CardanoPoint -> IO (Either [LedgerStateFile] StateRef)
findStateFromPoint env point = do
  files <- listLedgerStateFilesOrdered (leDir env)
  -- Genesis can be reproduced from configuration.
  -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      deleteAndLogStateFile env "newer" files
      Right <$> initCardanoLedgerState env  -- returns StateRef
    At blk -> do
      let (newerFiles, found, olderFiles) =
            findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      deleteAndLogStateFile env "newer" newerFiles
      case found of
        Just lsf -> do
          mState <- loadLedgerStateFromFile (leMkLedgerHandle env) (leMkLedgerHandleFromSnapshot env) (leLedgerBackend env) (leTrace env) (getTopLevelconfigHasLedger env) False point lsf
          case mState of
            Left err -> do
              deleteLedgerFile err lsf
              logNewerFiles olderFiles
              pure $ Left olderFiles
            Right st -> pure $ Right st
        Nothing -> do
          logNewerFiles olderFiles
          pure $ Left olderFiles
  where
    deleteLedgerFile :: Text -> LedgerStateFile -> IO ()
    deleteLedgerFile err lsf = do
      logWarning (leTrace env) $
        mconcat
          [ "Failed to parse ledger state file "
          , Text.pack (lsfFilePath lsf)
          , " with error '"
          , err
          , "'. Deleting it."
          ]
      removeStateFileAndLSMSnapshot env $ lsfFilePath lsf

    logNewerFiles :: [LedgerStateFile] -> IO ()
    logNewerFiles lsfs =
      logWarning (leTrace env) $
        case lsfs of
          [] -> "Rollback failed. No more ledger state files."
          (x : _) -> mconcat ["Needs to Rollback further to slot ", textShow (unSlotNo $ lsfSlotNo x)]

-- Splits the files based on the comparison with the given point. It will return
-- a list of newer files, a file at the given point if found and a list of older
-- files. All lists of files should be ordered most recent first.
--
-- Newer files can be deleted
-- File at the exact point can be used to initial the LedgerState
-- Older files can be used to rollback even further.
--
-- Files with same slot, but different hash are considered newer.
findLedgerStateFile ::
  [LedgerStateFile] ->
  (SlotNo, ByteString) ->
  ([LedgerStateFile], Maybe LedgerStateFile, [LedgerStateFile])
findLedgerStateFile files pointPair =
  go [] files
  where
    go newerFiles [] = (reverse newerFiles, Nothing, [])
    go newerFiles (file : rest) =
      case comparePointToFile file pointPair of
        EQ -> (reverse newerFiles, Just file, rest) -- found the file we were looking for
        LT -> (reverse newerFiles, Nothing, file : rest) -- found an older file first
        GT -> go (file : newerFiles) rest -- keep looking on older files

comparePointToFile :: LedgerStateFile -> (SlotNo, ByteString) -> Ordering
comparePointToFile lsf (blSlotNo, blHash) =
  case compare (lsfSlotNo lsf) blSlotNo of
    EQ ->
      if hashToAnnotation blHash == lsfHash lsf
        then EQ
        else GT
    x -> x

loadLedgerStateFromFile ::
  (ExtLedgerState CardanoBlock EmptyMK -> LedgerTables (ExtLedgerState CardanoBlock) ValuesMK -> IO (LedgerTablesHandle IO (ExtLedgerState CardanoBlock))) ->
  Maybe (String -> IO (LedgerTablesHandle IO (ExtLedgerState CardanoBlock))) ->
  LedgerBackend -> Trace IO Text -> TopLevelConfig CardanoBlock -> Bool -> CardanoPoint -> LedgerStateFile -> IO (Either Text StateRef)
loadLedgerStateFromFile mkHandle mkHandleFromSnap backend tracer config delete point lsf = do
  mst <- safeReadFile (lsfFilePath lsf)
  case mst of
    Left err -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure (Left err)
    Right st -> pure $ Right st
  where
    safeReadFile :: FilePath -> IO (Either Text StateRef)
    safeReadFile fp = do
      startTime <- getCurrentTime
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (err :: IOException) -> pure $ Left (Text.pack $ displayException err)
        Right bs -> do
          mediumTime <- getCurrentTime
          cls <- case backend of
            LedgerBackendInMemory ->
              case decodeInMemory bs of
                Left err -> pure $ Left $ textShow err
                Right (lState, eBlockNo, lTables) -> do
                  tablesHandle <- mkHandle lState lTables
                  canClose <- newTVarIO True
                  pure $ Right StateRef
                    { srState = CardanoLedgerState
                        { clsState = lState
                        , clsEpochBlockNo = eBlockNo
                        }
                    , srTables = tablesHandle
                    , srCanClose = canClose
                    }
            LedgerBackendLSM _mPath ->
              case decodeLSM bs of
                Left err -> pure $ Left $ textShow err
                Right (lState, eBlockNo) -> do
                  let snapshotName = dropExtension $ takeFileName fp
                  case mkHandleFromSnap of
                    Nothing -> pure $ Left "LSM snapshot restore not available"
                    Just restoreFromSnap -> do
                      eHandle <- Exception.try $ restoreFromSnap snapshotName
                      case eHandle of
                        Left (err :: Exception.SomeException) ->
                          pure $ Left $ "Failed to restore LSM snapshot '" <> Text.pack snapshotName <> "': " <> Text.pack (Exception.displayException err)
                        Right tablesHandle -> do
                          canClose <- newTVarIO True
                          pure $ Right StateRef
                            { srState = CardanoLedgerState
                                { clsState = lState
                                , clsEpochBlockNo = eBlockNo
                                }
                            , srTables = tablesHandle
                            , srCanClose = canClose
                            }
          case cls of
            Left err -> pure $ Left err
            Right ls -> do
              endTime <- getCurrentTime
              logInfo tracer $
                mconcat
                  [ "Found snapshot file for "
                  , renderPoint point
                  , ". It took "
                  , textShow (diffUTCTime mediumTime startTime)
                  , " to read from disk and "
                  , textShow (diffUTCTime endTime mediumTime)
                  , " to parse."
                  ]
              pure $ Right ls

    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec config

    decodeExt :: (forall s. Decoder s (ExtLedgerState CardanoBlock EmptyMK))
    decodeExt =
      Consensus.decodeExtLedgerState
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)

    decodeInMemory :: ByteString -> Either DecoderError (ExtLedgerState CardanoBlock EmptyMK, EpochBlockNo, LedgerTables (ExtLedgerState CardanoBlock) ValuesMK)
    decodeInMemory =
      Serialize.decodeFullDecoder "Ledger state file" (decodeCardanoLedgerStateInMemory decodeExt) . LBS.fromStrict

    decodeLSM :: ByteString -> Either DecoderError (ExtLedgerState CardanoBlock EmptyMK, EpochBlockNo)
    decodeLSM =
      Serialize.decodeFullDecoder "Ledger state file" (decodeCardanoLedgerStateLSM decodeExt) . LBS.fromStrict

getSlotNoSnapshot :: SnapshotPoint -> WithOrigin SlotNo
getSlotNoSnapshot (OnDisk lsf) = at $ lsfSlotNo lsf
getSlotNoSnapshot (InMemory cp) = pointSlot cp

listKnownSnapshots :: HasLedgerEnv -> IO [SnapshotPoint]
listKnownSnapshots env = do
  inMem <- fmap InMemory <$> listMemorySnapshots env
  onDisk <- fmap OnDisk <$> listLedgerStateFilesOrdered (leDir env)
  pure $ List.sortOn (Down . getSlotNoSnapshot) $ inMem <> onDisk

listMemorySnapshots :: HasLedgerEnv -> IO [CardanoPoint]
listMemorySnapshots env = do
  mState <- atomically $ readTVar $ leStateVar env
  case mState of
    Strict.Nothing -> pure []
    Strict.Just ledgerDB ->
      pure $
        filter
          notGenesis
          (castPoint . Consensus.getTip . clsState . srState <$> getEdgePoints ledgerDB)
  where
    getEdgePoints ldb =
      case toList $ ledgerDbCheckpoints ldb of
        [] -> []
        [a] -> [a]
        (h : ls) -> catMaybes [Just h, lastMay ls]
    notGenesis GenesisPoint = False
    notGenesis (BlockPoint _ _) = True

-- Get a list of the ledger state files order most recent
listLedgerStateFilesOrdered :: LedgerStateDir -> IO [LedgerStateFile]
listLedgerStateFilesOrdered dir = do
  files <- filter isLedgerStateFile <$> listDirectory (unLedgerStateDir dir)
  pure . List.sortBy revSlotNoOrder $ mapMaybe (parseLedgerStateFileName dir) files
  where
    isLedgerStateFile :: FilePath -> Bool
    isLedgerStateFile fp = takeExtension fp == ".lstate"

    revSlotNoOrder :: LedgerStateFile -> LedgerStateFile -> Ordering
    revSlotNoOrder a b = compare (lsfSlotNo b) (lsfSlotNo a)

writeLedgerState :: HasLedgerEnv -> Strict.Maybe LedgerDB -> IO ()
writeLedgerState env mLedgerDb = atomically $ writeTVar (leStateVar env) mLedgerDb

-- | Remove given file path and ignore any IOExceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

-- | Remove given directory recursively and ignore any IOExceptions.
safeRemoveDirectory :: FilePath -> IO ()
safeRemoveDirectory fp = handle (\(_ :: IOException) -> pure ()) $ removeDirectoryRecursive fp

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

-- | Apply a block to the current LedgerDB state. Reads the current StateRef,
-- duplicates its handle, applies the block, pushes diffs, creates a new StateRef,
-- and updates the LedgerDB. Returns the old StateRef (for snapshotting),
-- the new CardanoLedgerState (for ApplyResult), and pruned StateRefs (to close).
tickThenReapplyCheckHash ::
  HasLedgerEnv ->
  ResourceRegistry IO ->
  ExtLedgerCfg CardanoBlock ->
  CardanoBlock ->
  IO
    ( Either
        SyncNodeError
        ( StateRef -- old state ref (for snapshotting)
        , LedgerResult (ExtLedgerState CardanoBlock) CardanoLedgerState -- new state
        , [StateRef] -- pruned refs to close
        )
    )
tickThenReapplyCheckHash env registry cfg block = do
  -- Read the current state from LedgerDB
  (ledgerDB, oldRef) <- atomically $ do
    !db <- readStateUnsafe env
    pure (db, ledgerDbCurrent db)
  let !oldCls = srState oldRef
  if blockPrevHash block == Consensus.ledgerTipHash (ledgerState (clsState oldCls))
    then do
      -- Create a new handle first, then read from it
      (_rk, newHandle) <- duplicate (srTables oldRef) registry
      let keys = Consensus.getBlockKeySets block
      restrictedTables <- read newHandle (clsState oldCls) keys
      -- Attach the tables to the ledger state and apply the block
      let ledgerState' = Consensus.withLedgerTables (clsState oldCls) restrictedTables
          newLedgerResult =
            Consensus.tickThenReapplyLedgerResult Consensus.ComputeLedgerEvents cfg block ledgerState'
          -- Build pure CardanoLedgerState from result
          newCls = fmap
            ( \stt -> CardanoLedgerState
                { clsState = forgetLedgerTables stt
                , clsEpochBlockNo = clsEpochBlockNo oldCls
                }
            )
            newLedgerResult
      -- Push diffs to the new handle
      pushDiffs newHandle (clsState oldCls) (Consensus.lrResult newLedgerResult)
      -- Create new StateRef and push to LedgerDB
      canClose <- newTVarIO True
      let !newRef = StateRef
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
