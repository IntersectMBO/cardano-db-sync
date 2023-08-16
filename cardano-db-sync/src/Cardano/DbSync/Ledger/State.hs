{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Ledger.State (
  applyBlock,
  defaultApplyResult,
  mkHasLedgerEnv,
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
  getSliceMeta,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Cardano.Binary (Decoder, DecoderError)
import qualified Cardano.Binary as Serialize
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Cardano.Util as Cardano
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Shelley.AdaPots (AdaPots)
import Cardano.Ledger.Shelley.LedgerState (EpochState (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Block (BlockNo (..))
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
  writeTVar,
 )
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import qualified Control.Exception as Exception

import qualified Data.ByteString.Base16 as Base16
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif

import Cardano.DbSync.Api.Types (LedgerEnv (..), SyncOptions (..))
import Cardano.DbSync.Error (SyncNodeError (..), fromEitherSTM)
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
import Lens.Micro ((^.))
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
import Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract (
  LedgerResult (..),
  getTip,
  ledgerTipHash,
  ledgerTipPoint,
  ledgerTipSlot,
  tickThenReapplyLedgerResult,
 )
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Network.AnchoredSeq (AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Block (HeaderHash, Point (..), blockNo)
import qualified Ouroboros.Network.Point as Point
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (dropExtension, takeExtension, (</>))
import System.Mem (performMajorGC)
import Prelude (String, id)

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

pushLedgerDB :: LedgerDB -> CardanoLedgerState -> LedgerDB
pushLedgerDB db st =
  pruneLedgerDb
    10
    db
      { ledgerDbCheckpoints = ledgerDbCheckpoints db :> st
      }

-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
pruneLedgerDb :: Word64 -> LedgerDB -> LedgerDB
pruneLedgerDb k db =
  db {ledgerDbCheckpoints = AS.anchorNewest k (ledgerDbCheckpoints db)}
{-# INLINE pruneLedgerDb #-}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB -> CardanoLedgerState
ledgerDbCurrent = either id id . AS.head . ledgerDbCheckpoints

mkHasLedgerEnv ::
  Trace IO Text ->
  Consensus.ProtocolInfo CardanoBlock ->
  LedgerStateDir ->
  Ledger.Network ->
  SystemStart ->
  SyncOptions ->
  IO HasLedgerEnv
mkHasLedgerEnv trce protoInfo dir nw systemStart syncOptions = do
  svar <- newTVarIO Strict.Nothing
  intervar <- newTVarIO Strict.Nothing
  swQueue <- newTBQueueIO 5 -- Should be relatively shallow.
  pure
    HasLedgerEnv
      { leTrace = trce
      , leProtocolInfo = protoInfo
      , leDir = dir
      , leNetwork = nw
      , leSystemStart = systemStart
      , leAbortOnPanic = soptAbortOnInvalid syncOptions
      , leSnapshotEveryFollowing = snapshotEveryFollowing syncOptions
      , leSnapshotEveryLagging = snapshotEveryLagging syncOptions
      , leInterpreter = intervar
      , leStateVar = svar
      , leStateWriteQueue = swQueue
      }

initCardanoLedgerState :: Consensus.ProtocolInfo CardanoBlock -> CardanoLedgerState
initCardanoLedgerState pInfo =
  CardanoLedgerState
    { clsState = Consensus.pInfoInitLedger pInfo
    , clsEpochBlockNo = GenesisEpochBlockNo
    }

getTopLevelconfigHasLedger :: HasLedgerEnv -> TopLevelConfig CardanoBlock
getTopLevelconfigHasLedger = Consensus.pInfoConfig . leProtocolInfo

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
  (oldState, appResult) <- applyBlock ledgerEnv blk
  tookSnapshot <- storeSnapshotAndCleanupMaybe ledgerEnv oldState appResult (blockNo blk) isCons (isSyncedWithinSeconds (apSlotDetails appResult) 600)
  pure (appResult, tookSnapshot)

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: HasLedgerEnv -> CardanoBlock -> IO (CardanoLedgerState, ApplyResult)
applyBlock env blk = do
  time <- getCurrentTime
  atomically $ do
    !ledgerDB <- readStateUnsafe env
    let oldState = ledgerDbCurrent ledgerDB
    !result <- fromEitherSTM $ tickThenReapplyCheckHash (ExtLedgerCfg (getTopLevelconfigHasLedger env)) blk (clsState oldState)
    let !ledgerEventsFull = mapMaybe convertAuxLedgerEvent (lrEvents result)
    let !(ledgerEvents, deposits) = splitDeposits ledgerEventsFull
    let !newLedgerState = lrResult result
    !details <- getSlotDetails env (ledgerState newLedgerState) time (cardanoBlockSlotNo blk)
    !newEpoch <- fromEitherSTM $ mkNewEpoch (clsState oldState) newLedgerState (findAdaPots ledgerEvents)
    let !newEpochBlockNo = applyToEpochBlockNo (isJust $ blockIsEBB blk) (isJust newEpoch) (clsEpochBlockNo oldState)
    let !newState = CardanoLedgerState newLedgerState newEpochBlockNo
    let !ledgerDB' = pushLedgerDB ledgerDB newState
    writeTVar (leStateVar env) (Strict.Just ledgerDB')
    let !appResult =
          ApplyResult
            { apPrices = getPrices newState
            , apPoolsRegistered = getRegisteredPools oldState
            , apNewEpoch = maybeToStrict newEpoch
            , apOldLedger = Strict.Just oldState
            , apSlotDetails = details
            , apStakeSlice = getStakeSlice env newState False
            , apEvents = ledgerEvents
            , apDepositsMap = DepositsMap deposits
            }
    pure (oldState, appResult)
  where
    mkNewEpoch :: ExtLedgerState CardanoBlock -> ExtLedgerState CardanoBlock -> Maybe AdaPots -> Either SyncNodeError (Maybe Generic.NewEpoch)
    mkNewEpoch oldState newState mPots = do
      let currEpochE = ledgerEpochNo env newState
          prevEpochE = ledgerEpochNo env oldState
      -- pass on error when trying to get ledgerEpochNo
      case (currEpochE, prevEpochE) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right currEpoch, Right prevEpoch) -> do
          if currEpoch /= prevEpoch + 1
            then Right Nothing
            else
              Right $
                Just $
                  Generic.NewEpoch
                    { Generic.neEpoch = currEpoch
                    , Generic.neIsEBB = isJust $ blockIsEBB blk
                    , Generic.neAdaPots = maybeToStrict mPots
                    , Generic.neEpochUpdate = Generic.epochUpdate newState
                    }

    applyToEpochBlockNo :: Bool -> Bool -> EpochBlockNo -> EpochBlockNo
    applyToEpochBlockNo True _ _ = EBBEpochBlockNo
    applyToEpochBlockNo _ True _ = EpochBlockNo 0
    applyToEpochBlockNo _ _ (EpochBlockNo n) = EpochBlockNo (n + 1)
    applyToEpochBlockNo _ _ GenesisEpochBlockNo = EpochBlockNo 0
    applyToEpochBlockNo _ _ EBBEpochBlockNo = EpochBlockNo 0

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

getSliceMeta :: Generic.StakeSliceRes -> Maybe (Bool, EpochNo)
getSliceMeta (Generic.Slice (Generic.StakeSlice epochNo _) isFinal) = Just (isFinal, epochNo)
getSliceMeta _ = Nothing

storeSnapshotAndCleanupMaybe ::
  HasLedgerEnv ->
  CardanoLedgerState ->
  ApplyResult ->
  BlockNo ->
  Bool ->
  SyncState ->
  IO Bool
storeSnapshotAndCleanupMaybe env oldState appResult blkNo isCons syncState =
  case maybeFromStrict (apNewEpoch appResult) of
    Just newEpoch -> do
      let newEpochNo = Generic.neEpoch newEpoch
      -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
      liftIO $ saveCleanupState env oldState (Just $ newEpochNo - 1)
      pure True
    Nothing ->
      if timeToSnapshot syncState blkNo && isCons
        then do
          liftIO $ saveCleanupState env oldState Nothing
          pure True
        else pure False
  where
    timeToSnapshot :: SyncState -> BlockNo -> Bool
    timeToSnapshot syncSt bNo =
      case (syncSt, unBlockNo bNo) of
        (SyncFollowing, bno) -> bno `mod` leSnapshotEveryFollowing env == 0
        (SyncLagging, _) -> False

saveCurrentLedgerState :: HasLedgerEnv -> CardanoLedgerState -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env lState mEpochNo = do
  case mkLedgerStateFilename (leDir env) (clsState lState) mEpochNo of
    Origin -> pure () -- we don't store genesis
    At file -> do
      exists <- doesFileExist file
      if exists
        then
          logInfo (leTrace env) $
            mconcat
              ["File ", Text.pack file, " exists"]
        else atomically $ writeTBQueue (leStateWriteQueue env) (file, lState)

runLedgerStateWriteThread :: Trace IO Text -> LedgerEnv -> IO ()
runLedgerStateWriteThread tracer lenv =
  case lenv of
    HasLedger le -> ledgerStateWriteLoop tracer (leStateWriteQueue le) (configCodec $ getTopLevelconfigHasLedger le)
    NoLedger _ -> forever $ threadDelay 600000000 -- 10 minutes

ledgerStateWriteLoop :: Trace IO Text -> TBQueue (FilePath, CardanoLedgerState) -> CodecConfig CardanoBlock -> IO ()
ledgerStateWriteLoop tracer swQueue codecConfig =
  loop
  where
    loop :: IO ()
    loop = do
      (file, ledger) <- atomically $ readTBQueue swQueue -- Blocks until the queue has elements.
      writeLedgerStateFile file ledger
      loop

    writeLedgerStateFile :: FilePath -> CardanoLedgerState -> IO ()
    writeLedgerStateFile file ledger = do
      startTime <- getCurrentTime
      -- TODO: write the builder directly.
      -- BB.writeFile file $ toBuilder $
      LBS.writeFile file $
        Serialize.serialize $
          encodeCardanoLedgerState
            ( Consensus.encodeExtLedgerState
                (encodeDisk codecConfig)
                (encodeDisk codecConfig)
                (encodeDisk codecConfig)
            )
            ledger
      endTime <- getCurrentTime
      logInfo tracer $
        mconcat
          [ "Asynchronously wrote a ledger snapshot to "
          , Text.pack file
          , " in "
          , textShow (diffUTCTime endTime startTime)
          , "."
          ]

mkLedgerStateFilename :: LedgerStateDir -> ExtLedgerState CardanoBlock -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo =
  lsfFilePath . dbPointToFileName dir mEpochNo
    <$> getPoint (ledgerTipPoint @CardanoBlock (ledgerState ledger))

saveCleanupState :: HasLedgerEnv -> CardanoLedgerState -> Maybe EpochNo -> IO ()
saveCleanupState env ledger mEpochNo = do
  let st = clsState ledger
  saveCurrentLedgerState env ledger mEpochNo
  cleanupLedgerStateFiles env $
    fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState st)

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
  -- Remove all but 6 most recent state files.
  deleteAndLogStateFile env "old" (List.drop 3 valid)
  -- Remove all but 6 most recent epoch boundary state files.
  deleteAndLogStateFile env "old epoch boundary" (List.drop 6 epochBoundary)
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
  let mAnchoredSeq = rollbackLedger mLedgerDB
  case mAnchoredSeq of
    Nothing -> do
      -- Ledger states are growing to become very big in memory.
      -- Before parsing the new ledger state we need to make sure the old states
      -- are or can be garbage collected.
      writeLedgerState hasLedgerEnv Strict.Nothing
      performMajorGC
      mst <- findStateFromPoint hasLedgerEnv point
      case mst of
        Right st -> do
          writeLedgerState hasLedgerEnv (Strict.Just . LedgerDB $ AS.Empty st)
          logInfo (leTrace hasLedgerEnv) $ mconcat ["Found snapshot file for ", renderPoint point]
          pure $ Right st
        Left lsfs -> pure $ Left lsfs
    Just anchoredSeq' -> do
      logInfo (leTrace hasLedgerEnv) $ mconcat ["Found in memory ledger snapshot at ", renderPoint point]
      let ledgerDB' = LedgerDB anchoredSeq'
      let st = ledgerDbCurrent ledgerDB'
      deleteNewerFiles hasLedgerEnv point
      writeLedgerState hasLedgerEnv $ Strict.Just ledgerDB'
      pure $ Right st
  where
    rollbackLedger ::
      Strict.Maybe LedgerDB ->
      Maybe (AnchoredSeq (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState)
    rollbackLedger mLedgerDB = case mLedgerDB of
      Strict.Nothing -> Nothing
      Strict.Just ledgerDB ->
        AS.rollback (pointSlot point) (const True) (ledgerDbCheckpoints ledgerDB)

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
      safeRemoveFile fl
    _ -> do
      logInfo (leTrace env) $ mconcat ["Removing ", descr, " files ", textShow files]
      mapM_ safeRemoveFile files

deleteAndLogStateFile :: HasLedgerEnv -> Text -> [LedgerStateFile] -> IO ()
deleteAndLogStateFile env descr lsfs = deleteAndLogFiles env descr (lsfFilePath <$> lsfs)

findStateFromPoint :: HasLedgerEnv -> CardanoPoint -> IO (Either [LedgerStateFile] CardanoLedgerState)
findStateFromPoint env point = do
  files <- listLedgerStateFilesOrdered (leDir env)
  -- Genesis can be reproduced from configuration.
  -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      deleteAndLogStateFile env "newer" files
      pure . Right $ initCardanoLedgerState (leProtocolInfo env)
    At blk -> do
      let (newerFiles, found, olderFiles) =
            findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      deleteAndLogStateFile env "newer" newerFiles
      case found of
        Just lsf -> do
          mState <- loadLedgerStateFromFile (leTrace env) (getTopLevelconfigHasLedger env) False point lsf
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
      safeRemoveFile $ lsfFilePath lsf

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

loadLedgerStateFromFile :: Trace IO Text -> TopLevelConfig CardanoBlock -> Bool -> CardanoPoint -> LedgerStateFile -> IO (Either Text CardanoLedgerState)
loadLedgerStateFromFile tracer config delete point lsf = do
  mst <- safeReadFile (lsfFilePath lsf)
  case mst of
    Left err -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure (Left err)
    Right st -> pure $ Right st
  where
    safeReadFile :: FilePath -> IO (Either Text CardanoLedgerState)
    safeReadFile fp = do
      startTime <- getCurrentTime
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (err :: IOException) -> pure $ Left (Text.pack $ displayException err)
        Right bs -> do
          mediumTime <- getCurrentTime
          case decode bs of
            Left err -> pure $ Left $ textShow err
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

    decode :: ByteString -> Either DecoderError CardanoLedgerState
    decode = do
      Serialize.decodeFullDecoder
        "Ledger state file"
        decodeState
        . LBS.fromStrict

    decodeState :: (forall s. Decoder s CardanoLedgerState)
    decodeState =
      decodeCardanoLedgerState $
        Consensus.decodeExtLedgerState
          (decodeDisk codecConfig)
          (decodeDisk codecConfig)
          (decodeDisk codecConfig)

getSlotNoSnapshot :: SnapshotPoint -> WithOrigin SlotNo
getSlotNoSnapshot (OnDisk lsf) = at $ lsfSlotNo lsf
getSlotNoSnapshot (InMemory cp) = pointSlot cp

listKnownSnapshots :: HasLedgerEnv -> IO [SnapshotPoint]
listKnownSnapshots env = do
  inMem <- fmap InMemory <$> listMemorySnapshots env
  onDisk <- fmap OnDisk <$> listLedgerStateFilesOrdered (leDir env)
  pure $ reverse $ List.sortOn getSlotNoSnapshot $ inMem <> onDisk

listMemorySnapshots :: HasLedgerEnv -> IO [CardanoPoint]
listMemorySnapshots env = do
  mState <- atomically $ readTVar $ leStateVar env
  case mState of
    Strict.Nothing -> pure []
    Strict.Just ledgerDB ->
      pure $
        filter
          notGenesis
          (castPoint . getTip . clsState <$> getEdgePoints ledgerDB)
  where
    getEdgePoints ldb =
      case AS.toNewestFirst $ ledgerDbCheckpoints ldb of
        [] -> []
        [a] -> [a]
        ls -> [List.head ls, List.last ls]
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

-- | Remove given file path and ignore any IOEXceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

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

getRegisteredPoolShelley ::
  forall p era.
  EraCrypto era ~ StandardCrypto =>
  LedgerState (ShelleyBlock p era) ->
  Set.Set PoolKeyHash
getRegisteredPoolShelley lState =
  Map.keysSet $
    Shelley.psStakePoolParams $
      Shelley.certPState $
        Shelley.lsCertState $
          Shelley.esLState $
            Shelley.nesEs $
              Consensus.shelleyLedgerState lState

ledgerEpochNo :: HasLedgerEnv -> ExtLedgerState CardanoBlock -> Either SyncNodeError EpochNo
ledgerEpochNo env cls =
  case ledgerTipSlot (ledgerState cls) of
    Origin -> Right 0 -- An empty chain is in epoch 0
    NotOrigin slot ->
      case runExcept $ epochInfoEpoch epochInfo slot of
        Left err -> Left $ SNErrLedgerState $ "unable to use slot: " <> show slot <> "to get ledgerEpochNo: " <> show err
        Right en -> Right en
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo = epochInfoLedger (configLedger $ getTopLevelconfigHasLedger env) (hardForkLedgerStatePerEra $ ledgerState cls)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash ::
  ExtLedgerCfg CardanoBlock ->
  CardanoBlock ->
  ExtLedgerState CardanoBlock ->
  Either SyncNodeError (LedgerResult (ExtLedgerState CardanoBlock) (ExtLedgerState CardanoBlock))
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapplyLedgerResult cfg block lsb
    else
      Left $
        SNErrLedgerState $
          mconcat
            [ "Ledger state hash mismatch. Ledger head is slot "
            , show (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
            , " hash "
            , Text.unpack $ renderByteArray (Cardano.unChainHash (ledgerTipHash $ ledgerState lsb))
            , " but block previous hash is "
            , Text.unpack $ renderByteArray (Cardano.unChainHash $ blockPrevHash block)
            , " and block current hash is "
            , Text.unpack $ renderByteArray (SBS.fromShort . Consensus.getOneEraHash $ blockHash block)
            , "."
            ]

getHeaderHash :: HeaderHash CardanoBlock -> ByteString
getHeaderHash bh = SBS.fromShort (Consensus.getOneEraHash bh)

getSlotDetails :: HasLedgerEnv -> LedgerState CardanoBlock -> UTCTime -> SlotNo -> STM SlotDetails
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
      ( esPp
          ( Shelley.nesEs $
              Consensus.shelleyLedgerState als
          )
          ^. Alonzo.ppPricesL
      )
  LedgerStateBabbage bls ->
    Strict.Just
      ( esPp
          ( Shelley.nesEs $
              Consensus.shelleyLedgerState bls
          )
          ^. Alonzo.ppPricesL
      )
  _ -> Strict.Nothing

findAdaPots :: [LedgerEvent] -> Maybe AdaPots
findAdaPots = go
  where
    go [] = Nothing
    go (LedgerAdaPots p : _) = Just p
    go (_ : rest) = go rest
