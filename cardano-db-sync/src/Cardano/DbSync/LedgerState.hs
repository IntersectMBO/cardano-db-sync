{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.LedgerState
  ( BulkOperation (..)
  , CardanoLedgerState (..)
  , IndexCache (..)
  , LedgerEnv (..)
  , LedgerEvent (..)
  , LedgerStateSnapshot (..)
  , LedgerStateFile (..)
  , mkLedgerEnv
  , applyBlock
  , saveCleanupState
  , listLedgerStateFilesOrdered
  , loadLedgerStateFromFile
  , writeLedgerState
  , findStateFromPoint
  , findLedgerStateFile
  , loadLedgerAtPoint
  , hashToAnnotation
  , getHeaderHash
  , ledgerTipBlockNo
  , getPoolParams
  , getAlonzoPParams
  ) where

import           Prelude (String, id)

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Cardano.Binary (DecoderError)
import qualified Cardano.Binary as Serialize

import qualified Cardano.Db as DB

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin)
import           Cardano.Ledger.Core (PParams)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import           Cardano.Ledger.Shelley.Constraints (UsesValue)
import           Cardano.Ledger.Shelley.LedgerState (EpochState (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley

import           Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Cardano.Util as Cardano
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Generic.StakeCred
import           Cardano.DbSync.Era.Shelley.Generic.StakePoolKeyHash
import           Cardano.DbSync.LedgerEvent
import           Cardano.DbSync.StateQuery
import           Cardano.DbSync.Types hiding (CardanoBlock)
import           Cardano.DbSync.Util

import           Cardano.Prelude hiding (atomically)
import           Cardano.Slotting.Block (BlockNo (..))

import           Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..), fromWithOrigin)

import qualified Control.Exception as Exception
import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, TBQueue, atomically,
                   newEmptyTMVarIO, newTBQueueIO, newTVarIO, readTVar, writeTVar)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           Ouroboros.Consensus.Block (CodecConfig, Point (..), WithOrigin (..), blockHash,
                   blockIsEBB, blockPoint, blockPrevHash, pointSlot)
import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAlonzo,
                   StandardCrypto)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import           Ouroboros.Consensus.Ledger.Abstract (LedgerResult (..), getTipSlot, ledgerTipHash,
                   ledgerTipPoint, ledgerTipSlot, tickThenReapplyLedgerResult)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

import           Ouroboros.Network.AnchoredSeq (Anchorable (..), AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (HeaderHash, Point (..))
import qualified Ouroboros.Network.Point as Point

import           System.Directory (doesFileExist, listDirectory, removeFile)
import           System.FilePath (dropExtension, takeExtension, (</>))
import           System.Mem (performMajorGC)


-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

-- 'CardanoPoint' indicates at which point the 'BulkOperation' became available.
-- It is only used in case of a rollback.
data BulkOperation
  = BulkRewardChunk !EpochNo !CardanoPoint !IndexCache ![(StakeCred, Set Generic.Reward)]
  | BulkRewardReport !EpochNo !CardanoPoint !Int !Coin
  | BulkStakeDistChunk !EpochNo !CardanoPoint !IndexCache ![(StakeCred, (Coin, StakePoolKeyHash))]
  | BulkStakeDistReport !EpochNo !CardanoPoint !Int

data IndexCache = IndexCache
  { icAddressCache :: !(Map StakeCred DB.StakeAddressId)
  , icPoolCache :: !(Map StakePoolKeyHash DB.PoolHashId)
  }

data LedgerEnv = LedgerEnv
  { leTrace :: Trace IO Text
  , leProtocolInfo :: !(Consensus.ProtocolInfo IO CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leSystemStart :: !SystemStart
  , leAbortOnPanic :: !Bool
  , leInterpreter :: !(StrictTVar IO (Maybe CardanoInterpreter))
  , leStateVar :: !(StrictTVar IO (Maybe LedgerDB))
  , leEventState :: !(StrictTVar IO LedgerEventState)
  , lePoolRewards :: !(StrictTMVar IO Generic.Rewards)
  , leMirRewards :: !(StrictTMVar IO Generic.Rewards)
  -- The following do not really have anything to do with maintaining ledger
  -- state. They are here due to the ongoing headaches around the split between
  -- `cardano-sync` and `cardano-db-sync`.
  , leIndexCache :: !(StrictTVar IO IndexCache)
  , leBulkOpQueue :: !(TBQueue IO BulkOperation)
  , leOfflineWorkQueue :: !(TBQueue IO PoolFetchRetry)
  , leOfflineResultQueue :: !(TBQueue IO FetchResult)
  , leEpochSyncTime :: !(StrictTVar IO UTCTime)
  , leStableEpochSlot :: !EpochSlot
  }

data LedgerEventState = LedgerEventState
  { lesInitialized :: !Bool
  , lesEpochNo :: !(Maybe EpochNo)
  , lesLastRewardsEpoch :: !(Maybe EpochNo)
  , lesLastStateDistEpoch :: !(Maybe EpochNo)
  , lesLastAdded :: !CardanoPoint
  }

topLevelConfig :: LedgerEnv -> TopLevelConfig CardanoBlock
topLevelConfig = Consensus.pInfoConfig . leProtocolInfo

newtype CardanoLedgerState = CardanoLedgerState
  { clsState :: ExtLedgerState CardanoBlock
  }

data LedgerStateFile = LedgerStateFile
  { lsfSlotNo :: !SlotNo
  , lsfHash :: !ByteString
  , lsNewEpoch :: !(Maybe EpochNo)
  , lsfFilePath :: !FilePath
  } deriving Show

data LedgerStateSnapshot = LedgerStateSnapshot
  { lssState :: !CardanoLedgerState
  , lssOldState :: !CardanoLedgerState
  , lssNewEpoch :: !(Strict.Maybe Generic.NewEpoch) -- Only Just for a single block at the epoch boundary
  , lssSlotDetails :: !SlotDetails
  , lssPoint :: !CardanoPoint
  , lssEvents :: ![LedgerEvent]
  }

newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: AnchoredSeq (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState
  }

pushLedgerDB :: LedgerDB -> CardanoLedgerState -> LedgerDB
pushLedgerDB db st =
  pruneLedgerDb 10 db
    { ledgerDbCheckpoints = ledgerDbCheckpoints db :> st
    }

-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
pruneLedgerDb :: Word64 -> LedgerDB -> LedgerDB
pruneLedgerDb k db =
  db { ledgerDbCheckpoints = AS.anchorNewest k (ledgerDbCheckpoints db) }

instance Anchorable (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . clsState

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB -> CardanoLedgerState
ledgerDbCurrent = either id id . AS.head . ledgerDbCheckpoints

mkLedgerEnv
    :: Trace IO Text -> Consensus.ProtocolInfo IO CardanoBlock -> LedgerStateDir
    -> Ledger.Network -> EpochSlot -> SystemStart -> Bool
    -> IO LedgerEnv
mkLedgerEnv trce protocolInfo dir nw stableEpochSlot systemStart aop = do
    svar <- newTVarIO Nothing
    evar <- newTVarIO initLedgerEventState
    ivar <- newTVarIO $ IndexCache mempty mempty
    intervar <- newTVarIO Nothing
    -- 2.5 days worth of slots. If we try to stick more than this number of
    -- items in the queue, bad things are likely to happen.
    boq <- newTBQueueIO 10800
    owq <- newTBQueueIO 100
    orq <- newTBQueueIO 100
    est <- newTVarIO =<< getCurrentTime
    prvar <- newEmptyTMVarIO
    mrvar <- newEmptyTMVarIO
    pure LedgerEnv
      { leTrace = trce
      , leProtocolInfo = protocolInfo
      , leDir = dir
      , leNetwork = nw
      , leSystemStart = systemStart
      , leAbortOnPanic = aop
      , leInterpreter = intervar
      , leStateVar = svar
      , leEventState = evar
      , lePoolRewards = prvar
      , leMirRewards = mrvar
      , leIndexCache = ivar
      , leBulkOpQueue = boq
      , leOfflineWorkQueue = owq
      , leOfflineResultQueue  = orq
      , leEpochSyncTime = est
      , leStableEpochSlot = stableEpochSlot
      }
  where
    initLedgerEventState :: LedgerEventState
    initLedgerEventState =
      LedgerEventState
        { lesInitialized = False
        , lesEpochNo = Nothing
        , lesLastRewardsEpoch = Nothing
        , lesLastStateDistEpoch = Nothing
        , lesLastAdded = GenesisPoint
        }


initCardanoLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> CardanoLedgerState
initCardanoLedgerState pInfo = CardanoLedgerState
      { clsState = Consensus.pInfoInitLedger pInfo
      }

-- TODO make this type safe. We make the assumption here that the first message of
-- the chainsync protocol is 'RollbackTo'.
readStateUnsafe :: LedgerEnv -> STM LedgerDB
readStateUnsafe env = do
    mState <- readTVar $ leStateVar env
    case mState of
      Nothing -> panic "LedgerState.readStateUnsafe: Ledger state is not found"
      Just st -> pure st

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: LedgerEnv -> CardanoBlock -> IO LedgerStateSnapshot
applyBlock env blk = do
    -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
    -- be any contention on this variable, so putting everything inside 'atomically'
    -- is fine.
    time <- getCurrentTime
    atomically $ do
      ledgerDB <- readStateUnsafe env
      let oldState = ledgerDbCurrent ledgerDB
      let !result = applyBlk (ExtLedgerCfg (topLevelConfig env)) blk (clsState oldState)
      let !newState = oldState { clsState = lrResult result }
      details <- getSlotDetails env (ledgerState $ clsState newState) time (cardanoBlockSlotNo blk)
      let !ledgerDB' = pushLedgerDB ledgerDB newState
      writeTVar (leStateVar env) (Just ledgerDB')
      oldEventState <- readTVar (leEventState env)
      events <- generateEvents env oldEventState  details newState (blockPoint blk)
      pure $ LedgerStateSnapshot
                { lssState = newState
                , lssOldState = oldState
                , lssNewEpoch = maybeToStrict $ mkNewEpoch oldState newState
                , lssSlotDetails = details
                , lssPoint = blockPoint blk
                , lssEvents = events ++ mapMaybe (convertAuxLedgerEvent (leNetwork env)) (lrEvents result)
                }
  where
    applyBlk
        :: ExtLedgerCfg CardanoBlock -> CardanoBlock
        -> ExtLedgerState CardanoBlock
        -> LedgerResult (ExtLedgerState CardanoBlock) (ExtLedgerState CardanoBlock)
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> panic err
        Right result -> result

    mkNewEpoch :: CardanoLedgerState -> CardanoLedgerState -> Maybe Generic.NewEpoch
    mkNewEpoch oldState newState =
      if ledgerEpochNo env newState /= ledgerEpochNo env oldState + 1
        then Nothing
        else
          Just $
            Generic.NewEpoch
              { Generic.neEpoch = ledgerEpochNo env newState
              , Generic.neIsEBB = isJust $ blockIsEBB blk
              , Generic.neAdaPots = maybeToStrict $ getAdaPots newState
              , Generic.neEpochUpdate = Generic.epochUpdate (clsState newState)
              }

generateEvents :: LedgerEnv -> LedgerEventState -> SlotDetails -> CardanoLedgerState -> CardanoPoint -> STM [LedgerEvent]
generateEvents env oldEventState details cls pnt = do
    writeTVar (leEventState env) newEventState
    pure $ catMaybes
            [ newEpochEvent
            , LedgerRewards details <$> rewards
            , LedgerStakeDist <$> stakeDist
            ]
  where
    currentEpochNo :: EpochNo
    currentEpochNo = sdEpochNo details

    newEpochEvent :: Maybe LedgerEvent
    newEpochEvent =
      case lesEpochNo oldEventState of
        Nothing -> Just $ LedgerStartAtEpoch currentEpochNo
        Just oldEpoch ->
          if currentEpochNo == 1 + oldEpoch
            then Just $ LedgerNewEpoch currentEpochNo (getSyncStatus details)
            else Nothing

    -- Want the rewards event to be delivered once only, on a single slot.
    rewards :: Maybe Generic.Rewards
    rewards =
      case lesLastRewardsEpoch oldEventState of
        Nothing -> mkRewards
        Just oldRewardEpoch ->
          if sdEpochSlot details >= leStableEpochSlot env && oldRewardEpoch < currentEpochNo
            then mkRewards
            else Nothing


    mkRewards :: Maybe Generic.Rewards
    mkRewards = Generic.epochRewards (leNetwork env) (sdEpochNo details) (clsState cls)

    stakeDist :: Maybe Generic.StakeDist
    stakeDist =
      case lesLastStateDistEpoch oldEventState of
        Nothing -> mkStakeDist
        Just oldStakeEpoch ->
          if oldStakeEpoch < currentEpochNo
            then mkStakeDist
            else Nothing

    mkStakeDist :: Maybe Generic.StakeDist
    mkStakeDist = Generic.epochStakeDist (leNetwork env) (sdEpochNo details) (clsState cls)

    newEventState :: LedgerEventState
    newEventState =
      LedgerEventState
        { lesInitialized = True
        , lesEpochNo = Just currentEpochNo
        , lesLastRewardsEpoch =
            if isJust rewards
              then Just currentEpochNo
              else lesLastRewardsEpoch oldEventState
        , lesLastStateDistEpoch =
            if isJust stakeDist
              then Just currentEpochNo
              else lesLastStateDistEpoch oldEventState
        , lesLastAdded =
            if isNothing rewards && isNothing stakeDist
              then lesLastAdded oldEventState
              else pnt
        }

saveCurrentLedgerState :: LedgerEnv -> ExtLedgerState CardanoBlock -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env ledger mEpochNo = do
    case mkLedgerStateFilename (leDir env) ledger mEpochNo of
      Origin -> pure () -- we don't store genesis
      At file -> do
        exists <- doesFileExist file
        if exists then
          logInfo (leTrace env) $ mconcat
            ["File ", Text.pack file, " exists"]
        else do
          LBS.writeFile file $
            Serialize.serializeEncoding $
              Consensus.encodeExtLedgerState
                 (encodeDisk codecConfig)
                 (encodeDisk codecConfig)
                 (encodeDisk codecConfig)
                 ledger
          logInfo (leTrace env) $ mconcat ["Took a ledger snapshot at ", Text.pack file]
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec (topLevelConfig env)

mkLedgerStateFilename :: LedgerStateDir -> ExtLedgerState CardanoBlock -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo = lsfFilePath . dbPointToFileName dir mEpochNo
    <$> getPoint (ledgerTipPoint (Proxy @CardanoBlock) (ledgerState ledger))

saveCleanupState :: LedgerEnv -> CardanoLedgerState -> Maybe EpochNo -> IO ()
saveCleanupState env ledger mEpochNo = do
  let st = clsState ledger
  saveCurrentLedgerState env st mEpochNo
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
      , lsNewEpoch = mEpochNo
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
      (slotStr, '-': hashEpoch) -> do
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
        , lsNewEpoch = EpochNo <$> mEpochNo
        , lsfFilePath = stateDir </> fp
        }

-- -------------------------------------------------------------------------------------------------

cleanupLedgerStateFiles :: LedgerEnv -> SlotNo -> IO ()
cleanupLedgerStateFiles env slotNo = do
    files <- listLedgerStateFilesOrdered (leDir env)
    let (epochBoundary, valid, invalid) = foldr groupFiles ([], [], []) files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    deleteAndLogFiles env "invalid" invalid
    -- Remove all but 2 most recent state files.
    deleteAndLogStateFile env "valid" (List.drop 2 valid)
    -- Remove all but 5 most recent epoch boundary state files.
    deleteAndLogStateFile env "epoch boundary" (List.drop 5 epochBoundary)
  where
    groupFiles :: LedgerStateFile
               -> ([LedgerStateFile], [LedgerStateFile], [FilePath])
               -> ([LedgerStateFile], [LedgerStateFile], [FilePath]) -- (epochBoundary, valid, invalid)
    groupFiles lFile (epochBoundary, regularFile, invalid)
      | lsfSlotNo lFile > slotNo =
        (epochBoundary, regularFile, lsfFilePath lFile : invalid)
      | Just _ <- lsNewEpoch lFile =
        (lFile : epochBoundary, regularFile, invalid)
      | otherwise =
        (epochBoundary, lFile : regularFile, invalid)

loadLedgerAtPoint :: LedgerEnv -> CardanoPoint -> IO (Either [LedgerStateFile] CardanoLedgerState)
loadLedgerAtPoint env point = do
    mLedgerDB <- atomically $ readTVar $ leStateVar env
    -- First try to find the ledger in memory
    let mAnchoredSeq = rollbackLedger mLedgerDB
    case mAnchoredSeq of
      Nothing -> do
        -- Ledger states are growing to become very big in memory.
        -- Before parsing the new ledger state we need to make sure the old states
        -- are or can be garbage collected.
        writeLedgerState env Nothing
        performMajorGC
        mst <- findStateFromPoint env point
        case mst of
          Right st -> do
            writeLedgerState env (Just . LedgerDB $ AS.Empty st)
            logInfo (leTrace env) $ mconcat [ "Found snapshot file for ", renderPoint point ]
            pure $ Right st
          Left lsfs -> pure $ Left lsfs
      Just anchoredSeq' -> do
        logInfo (leTrace env) $ mconcat ["Found in memory ledger snapshot at ", renderPoint point ]
        let ledgerDB' = LedgerDB anchoredSeq'
        let st = ledgerDbCurrent ledgerDB'
        deleteNewerFiles env point
        writeLedgerState env $ Just ledgerDB'
        pure $ Right st
  where
    rollbackLedger
        :: Maybe LedgerDB
        -> Maybe (AnchoredSeq (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState)
    rollbackLedger mLedgerDB = do
      ledgerDB <- mLedgerDB
      AS.rollback (pointSlot point) (const True) (ledgerDbCheckpoints ledgerDB)

deleteNewerFiles :: LedgerEnv -> CardanoPoint -> IO ()
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

deleteAndLogFiles :: LedgerEnv -> Text -> [FilePath] -> IO ()
deleteAndLogFiles env descr files = unless (null files) $ do
  logInfo (leTrace env) $ mconcat ["Removing ", descr, " files ", textShow files]
  mapM_ safeRemoveFile files

deleteAndLogStateFile :: LedgerEnv -> Text -> [LedgerStateFile] -> IO ()
deleteAndLogStateFile env descr lsfs = deleteAndLogFiles env descr (lsfFilePath <$> lsfs)

findStateFromPoint :: LedgerEnv -> CardanoPoint -> IO (Either [LedgerStateFile] CardanoLedgerState)
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
          mState <- loadLedgerStateFromFile (topLevelConfig env) False lsf
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
      logWarning (leTrace env) $ mconcat
        [ "Failed to parse ledger state file ", Text.pack (lsfFilePath  lsf)
        , " with error '", err, "'. Deleting it."
        ]
      safeRemoveFile $ lsfFilePath lsf

    logNewerFiles :: [LedgerStateFile] -> IO ()
    logNewerFiles lsfs =
      logWarning (leTrace env) $
        case lsfs of
          [] -> "Rollback failed. No more ledger state files."
          (x:_) -> mconcat [ "Rolling back further to slot ", textShow (unSlotNo $ lsfSlotNo x) ]

-- Splits the files based on the comparison with the given point. It will return
-- a list of newer files, a file at the given point if found and a list of older
-- files. All lists of files should be ordered most recent first.
--
-- Newer files can be deleted
-- File at the exact point can be used to initial the LedgerState
-- Older files can be used to rollback even further.
--
-- Files with same slot, but different hash are considered newer.
findLedgerStateFile
    :: [LedgerStateFile] -> (SlotNo, ByteString)
    -> ([LedgerStateFile], Maybe LedgerStateFile, [LedgerStateFile])
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

loadLedgerStateFromFile :: TopLevelConfig CardanoBlock -> Bool -> LedgerStateFile -> IO (Either Text CardanoLedgerState)
loadLedgerStateFromFile config delete lsf = do
    mst <- safeReadFile (lsfFilePath lsf)
    case mst of
      Left err -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure (Left err)
      Right st -> pure . Right $ CardanoLedgerState { clsState = st }
  where
    safeReadFile :: FilePath -> IO (Either Text (ExtLedgerState CardanoBlock))
    safeReadFile fp = do
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (err :: IOException) -> pure $ Left (Text.pack $ displayException err)
        Right bs ->
          case decode bs of
            Left err -> pure $ Left $ textShow err
            Right ls -> pure $ Right ls

    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec config

    decode :: ByteString -> Either DecoderError (ExtLedgerState CardanoBlock)
    decode =
      Serialize.decodeFullDecoder
          "Ledger state file"
          (Consensus.decodeExtLedgerState
            (decodeDisk codecConfig)
            (decodeDisk codecConfig)
            (decodeDisk codecConfig))
        . LBS.fromStrict

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

writeLedgerState :: LedgerEnv -> Maybe LedgerDB -> IO ()
writeLedgerState env mLedgerDb = atomically $ writeTVar (leStateVar env) mLedgerDb

-- | Remove given file path and ignore any IOEXceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

getPoolParams :: CardanoLedgerState -> Set.Set (KeyHash 'StakePool StandardCrypto)
getPoolParams st =
    case ledgerState $ clsState st of
      LedgerStateByron _ -> Set.empty
      LedgerStateShelley sts -> getPoolParamsShelley sts
      LedgerStateAllegra sts -> getPoolParamsShelley sts
      LedgerStateMary sts -> getPoolParamsShelley sts
      LedgerStateAlonzo ats -> getPoolParamsShelley ats

getPoolParamsShelley
    :: forall era. (Crypto era ~ StandardCrypto)
    => LedgerState (ShelleyBlock era)
    -> Set.Set (KeyHash 'StakePool StandardCrypto)
getPoolParamsShelley lState =
  Map.keysSet $ Shelley._pParams $ Shelley._pstate $ Shelley._delegationState
              $ Shelley.esLState $ Shelley.nesEs $ Consensus.shelleyLedgerState lState

-- We only compute 'AdaPots' for later eras. This is a time consuming
-- function and we only want to run it on epoch boundaries.
getAdaPots :: CardanoLedgerState -> Maybe Shelley.AdaPots
getAdaPots st =
    case ledgerState $ clsState st of
      LedgerStateByron _ -> Nothing
      LedgerStateShelley sts -> Just $ totalAdaPots sts
      LedgerStateAllegra sta -> Just $ totalAdaPots sta
      LedgerStateMary stm -> Just $ totalAdaPots stm
      LedgerStateAlonzo sta -> Just $ totalAdaPots sta

ledgerEpochNo :: LedgerEnv -> CardanoLedgerState -> EpochNo
ledgerEpochNo env cls =
    case ledgerTipSlot (ledgerState (clsState cls)) of
      Origin -> 0 -- An empty chain is in epoch 0
      NotOrigin slot ->
        case runExcept $ epochInfoEpoch epochInfo slot of
          Left err -> panic $ "ledgerEpochNo: " <> textShow err
          Right en -> en
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo = epochInfoLedger (configLedger $ topLevelConfig env) (hardForkLedgerStatePerEra . ledgerState $ clsState cls)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash
    :: ExtLedgerCfg CardanoBlock -> CardanoBlock
    -> ExtLedgerState CardanoBlock
    -> Either Text (LedgerResult (ExtLedgerState CardanoBlock) (ExtLedgerState CardanoBlock))
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapplyLedgerResult cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
                  , " hash ", renderByteArray (Cardano.unChainHash (ledgerTipHash $ ledgerState lsb))
                  , " but block previous hash is "
                  , renderByteArray (Cardano.unChainHash $ blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray (SBS.fromShort . Consensus.getOneEraHash $ blockHash block), "."
                  ]

totalAdaPots
    :: forall era. UsesValue era
    => LedgerState (ShelleyBlock era)
    -> Shelley.AdaPots
totalAdaPots = Shelley.totalAdaPotsES . Shelley.nesEs . Consensus.shelleyLedgerState

getHeaderHash :: HeaderHash CardanoBlock -> ByteString
getHeaderHash bh = SBS.fromShort (Consensus.getOneEraHash bh)

-- | This will fail if the state is not a 'LedgerStateAlonzo'
getAlonzoPParams :: CardanoLedgerState -> PParams StandardAlonzo
getAlonzoPParams cls =
  case ledgerState $ clsState cls of
    LedgerStateAlonzo als -> esPp $ Shelley.nesEs $ Consensus.shelleyLedgerState als
    _ -> panic "Expected LedgerStateAlonzo after an Alonzo Block"

-- | This should be exposed by 'consensus'.
ledgerTipBlockNo :: ExtLedgerState blk -> WithOrigin BlockNo
ledgerTipBlockNo = fmap Consensus.annTipBlockNo . Consensus.headerStateTip . Consensus.headerState

getSlotDetails :: LedgerEnv -> LedgerState CardanoBlock -> UTCTime -> SlotNo -> STM SlotDetails
getSlotDetails env st time slot = do
    minter <- readTVar $ leInterpreter env
    details <- case minter of
      Just inter -> case queryWith inter of
        Left _ -> queryNewInterpreter
        Right sd -> return sd
      Nothing -> queryNewInterpreter
    pure $ details { sdCurrentTime = time }
  where
    hfConfig = configLedger $ Consensus.pInfoConfig $ leProtocolInfo env

    queryNewInterpreter :: STM SlotDetails
    queryNewInterpreter =
      let inter = History.mkInterpreter $ hardForkSummary hfConfig st
      in case queryWith inter of
        Left err -> throwSTM err
        Right sd -> do
          writeTVar (leInterpreter env) (Just inter)
          return sd

    queryWith inter =
      History.interpretQuery inter (querySlotDetails (leSystemStart env) slot)

