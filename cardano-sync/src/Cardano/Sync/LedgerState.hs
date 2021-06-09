{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync.LedgerState
  ( BulkOperation (..)
  , CardanoLedgerState (..)
  , IndexCache (..)
  , LedgerEnv (..)
  , LedgerEvent (..)
  , LedgerStateSnapshot (..)
  , LedgerStateFile (..)
  , mkLedgerEnv
  , applyBlock
  , saveLedgerStateMaybe
  , listLedgerStateFilesOrdered
  , loadLedgerStateFromFile
  , findStateFromPoint
  , findLedgerStateFile
  , loadLedgerAtPoint
  , hashToAnnotation
  , getHeaderHash
  , getPoolParams
  ) where

import           Prelude (String)

import           Cardano.Binary (DecoderError)
import qualified Cardano.Binary as Serialize

import           Cardano.Db (SyncState (..))
import qualified Cardano.Db as DB

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin)
import           Cardano.Ledger.Era
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Shelley.Constraints (UsesValue)
import qualified Cardano.Ledger.Val as Val

import           Cardano.Sync.Config.Types
import qualified Cardano.Sync.Era.Cardano.Util as Cardano
import           Cardano.Sync.Era.Shelley.Generic (StakeCred)
import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Types hiding (CardanoBlock)
import           Cardano.Sync.Util

import           Cardano.Prelude hiding (atomically)
import           Cardano.Slotting.Block (BlockNo (..))

import           Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..), fromWithOrigin)

import qualified Control.Exception as Exception
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, TBQueue, atomically, newTBQueueIO,
                   newTVarIO, readTVar, writeTVar)
import           Control.Monad.Extra (firstJustM, fromMaybeM)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)

import           Ouroboros.Consensus.Block (CodecConfig, WithOrigin (..), blockHash, blockIsEBB,
                   blockPrevHash, withOrigin)
import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import           Ouroboros.Consensus.Ledger.Abstract (ledgerTipHash, ledgerTipPoint, ledgerTipSlot,
                   tickThenReapply)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

import           Ouroboros.Network.Block (HeaderHash, Point (..))
import qualified Ouroboros.Network.Point as Point

import           Shelley.Spec.Ledger.LedgerState (AccountState, EpochState, UTxOState)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           System.Directory (listDirectory, removeFile)
import           System.FilePath (dropExtension, takeExtension, (</>))

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

data BulkOperation
  = BulkRewardChunk !EpochNo !IndexCache ![(StakeCred, Set (Shelley.Reward StandardCrypto))]
  | BulkOrphanedRewardChunk !EpochNo !IndexCache ![(StakeCred, Set (Shelley.Reward StandardCrypto))]
  | BulkRewardReport !EpochNo !Int !Int
  | BulkStakeDistChunk !EpochNo !IndexCache ![(StakeCred, (Coin, PoolKeyHash))]
  | BuldStakeDistReport !EpochNo !Int


data IndexCache = IndexCache
  { icAddressCache :: !(Map Generic.StakeCred DB.StakeAddressId)
  , icPoolCache :: !(Map PoolKeyHash DB.PoolHashId)
  }

data LedgerEnv = LedgerEnv
  { leProtocolInfo :: !(Consensus.ProtocolInfo IO CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leStateVar :: !(StrictTVar IO CardanoLedgerState)
  , leEventState :: !(StrictTVar IO LedgerEventState)
  -- The following do not really have anything to do with maintaining ledger
  -- state. They are here due to the ongoing headaches around the split between
  -- `cardano-sync` and `cardano-db-sync`.
  , leIndexCache :: !(StrictTVar IO IndexCache)
  , leBulkOpQueue :: !(TBQueue IO BulkOperation)
  , leOfflineWorkQueue :: !(TBQueue IO PoolFetchRetry)
  , leOfflineResultQueue :: !(TBQueue IO FetchResult)
  , leEpochSyncTime :: !(StrictTVar IO (Maybe UTCTime))
  }

data LedgerEvent
  = LedgerNewEpoch !EpochNo !SyncState
  | LedgerRewards !SlotDetails !Generic.Rewards
  | LedgerStakeDist !Generic.StakeDist
  deriving Eq

data LedgerEventState = LedgerEventState
  { lesInitialized :: !Bool
  , lesEpochNo :: !EpochNo
  , lesLastRewardsEpoch :: !EpochNo
  , lesLastStateDistEpoch :: !EpochNo
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
  , lssEvents :: ![LedgerEvent]
  }

mkLedgerEnv :: Consensus.ProtocolInfo IO CardanoBlock
            -> LedgerStateDir
            -> Ledger.Network
            -> SlotNo
            -> Bool
            -> IO LedgerEnv
mkLedgerEnv protocolInfo dir network slot deleteFiles = do
    when deleteFiles $
      deleteNewerLedgerStateFiles dir slot
    st <- findLatestLedgerState protocolInfo dir deleteFiles
    svar <- newTVarIO st
    evar <- newTVarIO initLedgerEventState
    ivar <- newTVarIO $ IndexCache mempty mempty
    -- 2.5 days worth of slots. If we try to stick more than this number of
    -- items in the queue, bad things are likely to happen.
    boq <- newTBQueueIO 10800
    owq <- newTBQueueIO 100
    orq <- newTBQueueIO 100
    est <- newTVarIO Nothing
    pure LedgerEnv
      { leProtocolInfo = protocolInfo
      , leDir = dir
      , leNetwork = network
      , leStateVar = svar
      , leEventState = evar
      , leIndexCache = ivar
      , leBulkOpQueue = boq
      , leOfflineWorkQueue = owq
      , leOfflineResultQueue  = orq
      , leEpochSyncTime = est
      }
  where
    initLedgerEventState :: LedgerEventState
    initLedgerEventState =
      LedgerEventState
        { lesInitialized = False
        , lesEpochNo = EpochNo 0
        , lesLastRewardsEpoch = EpochNo 0
        , lesLastStateDistEpoch = EpochNo 0
        }


initCardanoLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> CardanoLedgerState
initCardanoLedgerState pInfo = CardanoLedgerState
      { clsState = Consensus.pInfoInitLedger pInfo
      }

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: LedgerEnv -> CardanoBlock -> SlotDetails -> IO LedgerStateSnapshot
applyBlock env blk details =
    -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
    -- be any contention on this variable, so putting everything inside 'atomically'
    -- is fine.
    atomically $ do
      oldState <- readTVar (leStateVar env)
      let !newState = oldState { clsState = applyBlk (ExtLedgerCfg (topLevelConfig env)) blk (clsState oldState) }
      writeTVar (leStateVar env) newState
      oldEventState <- readTVar (leEventState env)
      events <- generateEvents env oldEventState  details newState
      pure $ LedgerStateSnapshot
                { lssState = newState
                , lssOldState = oldState
                , lssNewEpoch = maybeToStrict $ mkNewEpoch oldState newState
                , lssSlotDetails = details
                , lssEvents = events
                }
  where
    applyBlk
        :: ExtLedgerCfg CardanoBlock -> CardanoBlock
        -> ExtLedgerState CardanoBlock
        -> ExtLedgerState CardanoBlock
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> panic err
        Right result -> result

    mkNewEpoch oldState newState =
      if ledgerEpochNo env newState == ledgerEpochNo env oldState + 1
        then
          let epochNo = ledgerEpochNo env newState in
          Just $
            Generic.NewEpoch
              { Generic.neEpoch = epochNo
              , Generic.neIsEBB = isJust $ blockIsEBB blk
              , Generic.neAdaPots = maybeToStrict $ getAdaPots newState
              , Generic.neEpochUpdate = Generic.epochUpdate (clsState newState)
              }
        else Nothing

generateEvents :: LedgerEnv -> LedgerEventState -> SlotDetails -> CardanoLedgerState -> STM [LedgerEvent]
generateEvents env oldEventState details cls = do
    writeTVar (leEventState env) newEventState
    pure $ catMaybes
            [ if currentEpochNo == 1 + lesEpochNo oldEventState
                then Just (LedgerNewEpoch currentEpochNo (getSyncStatus details))
                else Nothing
            , LedgerRewards details <$> rewards
            , LedgerStakeDist <$> stakeDist
            ]
  where
    currentEpochNo :: EpochNo
    currentEpochNo = sdEpochNo details

    rewards :: Maybe Generic.Rewards
    rewards =
      -- Want the rewards event to be delivered once only, on a single slot.
      if lesLastRewardsEpoch oldEventState < currentEpochNo && lesInitialized oldEventState
        then Generic.epochRewards (leNetwork env) (sdEpochNo details) (clsState cls)
        else Nothing

    stakeDist :: Maybe Generic.StakeDist
    stakeDist =
      if lesLastStateDistEpoch oldEventState < currentEpochNo && lesInitialized oldEventState
        then Generic.epochStakeDist (leNetwork env) (sdEpochNo details) (clsState cls)
        else Nothing

    newEventState :: LedgerEventState
    newEventState =
      LedgerEventState
        { lesInitialized = True
        , lesEpochNo = currentEpochNo
        , lesLastRewardsEpoch =
            if isJust rewards || not (lesInitialized oldEventState)
              then currentEpochNo
              else lesLastRewardsEpoch oldEventState
        , lesLastStateDistEpoch =
            if isJust stakeDist || not (lesInitialized oldEventState)
              then currentEpochNo
              else lesLastStateDistEpoch oldEventState
        }

-- Delete ledger state files for slots later than the provided SlotNo.
deleteNewerLedgerStateFiles :: LedgerStateDir -> SlotNo -> IO ()
deleteNewerLedgerStateFiles stateDir slotNo = do
    delFiles <- filter isNewer <$> listLedgerStateFilesOrdered stateDir
    mapM_ (safeRemoveFile . lsfFilePath) delFiles
  where
    isNewer :: LedgerStateFile -> Bool
    isNewer lsf = lsfSlotNo lsf > slotNo

saveCurrentLedgerState :: LedgerEnv -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env mEpochNo = do
    ledger <- atomically $ readTVar (leStateVar env)
    case mkLedgerStateFilename (leDir env) ledger mEpochNo of
      Origin -> pure () -- we don't store genesis
      At file -> LBS.writeFile file $
        Serialize.serializeEncoding $
          Consensus.encodeExtLedgerState
             (encodeDisk codecConfig)
             (encodeDisk codecConfig)
             (encodeDisk codecConfig)
             (clsState ledger)
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec (topLevelConfig env)

saveLedgerStateMaybe :: LedgerEnv -> LedgerStateSnapshot -> SyncState -> IO ()
saveLedgerStateMaybe env snapshot synced = do
  writeLedgerState env ledger
  case (synced, lssNewEpoch snapshot) of
    (_, Strict.Just newEpoch) | not (Generic.neIsEBB newEpoch) ->
      saveCleanupState (Just $ Generic.neEpoch newEpoch) -- Save ledger states on epoch boundaries, unless they are EBBs
    (SyncFollowing, Strict.Nothing) ->
      saveCleanupState Nothing   -- If following, save every state.
    (SyncLagging, Strict.Nothing) | block `mod` 2000 == 0 ->
      saveCleanupState Nothing   -- Only save state ocassionally.
    _ -> pure ()
  where
    block :: Word64
    block = withOrigin 0 unBlockNo $ ledgerTipBlockNo ledger

    ledger :: ExtLedgerState CardanoBlock
    ledger = clsState $ lssState snapshot

    saveCleanupState :: Maybe EpochNo -> IO ()
    saveCleanupState mEpochNo = do
      saveCurrentLedgerState env mEpochNo
      cleanupLedgerStateFiles env $
        fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState ledger)

findLatestLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> LedgerStateDir -> Bool -> IO CardanoLedgerState
findLatestLedgerState pInfo dir deleteFiles =
    fromMaybeM (pure $ initCardanoLedgerState pInfo)
               (findLatestLedgerStateDisk config dir deleteFiles)
  where
    config = Consensus.pInfoConfig pInfo

findLatestLedgerStateDisk :: TopLevelConfig CardanoBlock -> LedgerStateDir -> Bool -> IO (Maybe CardanoLedgerState)
findLatestLedgerStateDisk config dir deleteFiles = do
  files <- listLedgerStateFilesOrdered dir
  firstJustM (loadLedgerStateFromFile config deleteFiles) files

mkLedgerStateFilename :: LedgerStateDir -> CardanoLedgerState -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo = lsfFilePath . dbPointToFileName dir mEpochNo
    <$> getPoint (ledgerTipPoint (Proxy @CardanoBlock) (ledgerState $ clsState ledger))

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
    epochSuffix = case mEpochNo of
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

-- | This should be exposed by 'consensus'.
ledgerTipBlockNo :: ExtLedgerState blk -> WithOrigin BlockNo
ledgerTipBlockNo = fmap Consensus.annTipBlockNo . Consensus.headerStateTip . Consensus.headerState

-- -------------------------------------------------------------------------------------------------

cleanupLedgerStateFiles :: LedgerEnv -> SlotNo -> IO ()
cleanupLedgerStateFiles env slotNo = do
    files <- listLedgerStateFilesOrdered (leDir env)
    let (epochBoundary, valid, invalid) = foldr groupFiles ([], [], []) files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    mapM_ safeRemoveFile invalid
    -- Remove all but 8 most recent state files.
    mapM_ (safeRemoveFile . lsfFilePath) (List.drop 8 valid)
    -- Remove all but 2 most recent epoch boundary state files.
    mapM_ (safeRemoveFile . lsfFilePath) (List.drop 2 epochBoundary)
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

loadLedgerAtPoint :: LedgerEnv -> CardanoPoint -> Bool -> IO (Either [LedgerStateFile] CardanoLedgerState)
loadLedgerAtPoint env point delFiles = do
    mst <- findStateFromPoint env point delFiles
    case mst of
      Right st -> do
        writeLedgerState env (clsState st)
        pure $ Right st
      Left lsfs -> pure $ Left lsfs

findStateFromPoint :: LedgerEnv -> CardanoPoint -> Bool -> IO (Either [LedgerStateFile] CardanoLedgerState)
findStateFromPoint env point delFiles = do
  files <- listLedgerStateFilesOrdered (leDir env)
    -- Genesis can be reproduced from configuration.
    -- TODO: We can make this a monadic action (reread config from disk) to save some memory.
  case getPoint point of
    Origin -> do
      when delFiles $
        mapM_ (safeRemoveFile . lsfFilePath) files
      pure . Right $ initCardanoLedgerState (leProtocolInfo env)
    At blk -> do
      let (filesToDelete, found) = findLedgerStateFile files (Point.blockPointSlot blk, mkRawHash $ Point.blockPointHash blk)
      when delFiles $
        mapM_ (safeRemoveFile . lsfFilePath) filesToDelete
      case found of
        Right lsf -> do
          mState <- loadLedgerStateFromFile (topLevelConfig env) False lsf
          case mState of
            Nothing -> do
              when delFiles $ safeRemoveFile $ lsfFilePath lsf
              panic $ "findStateFromPoint failed to parse required state file: "
                   <> Text.pack (lsfFilePath lsf)
            Just st -> pure $ Right st
        Left lsfs -> pure $ Left lsfs

-- Tries to find a file which matches the given point.
-- It returns the file if we found one, or a list older files. These can
-- be used to rollback even further.
-- It will also return a list of files that are newer than the point. These files should
-- be deleted. Files with same slot, but different hash are considered newer.
findLedgerStateFile
    :: [LedgerStateFile] -> (SlotNo, ByteString)
    -> ([LedgerStateFile], Either [LedgerStateFile] LedgerStateFile)
findLedgerStateFile files pointPair =
        go [] files
      where
        go delFiles [] = (delFiles, Left [])
        go delFiles (file : rest) =
          case comparePointToFile file pointPair of
            EQ -> (delFiles, Right file) -- found the file we were looking for
            LT -> (delFiles, Left $ file : rest) -- found an older file first
            GT -> go (file : delFiles) rest -- keep looking on older files

comparePointToFile :: LedgerStateFile -> (SlotNo, ByteString) -> Ordering
comparePointToFile lsf (blSlotNo, blHash) =
  case compare (lsfSlotNo lsf) blSlotNo of
    EQ ->
      if hashToAnnotation blHash == lsfHash lsf
        then EQ
        else GT
    x -> x

loadLedgerStateFromFile :: TopLevelConfig CardanoBlock -> Bool -> LedgerStateFile -> IO (Maybe CardanoLedgerState)
loadLedgerStateFromFile config delete lsf = do
    mst <- safeReadFile (lsfFilePath lsf)
    case mst of
      Nothing -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure Nothing
      Just st -> pure . Just $ CardanoLedgerState { clsState = st }
  where
    safeReadFile :: FilePath -> IO (Maybe (ExtLedgerState CardanoBlock))
    safeReadFile fp = do
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (_ :: IOException) -> pure Nothing
        Right bs ->
          case decode bs of
            Left _err -> do
              safeRemoveFile fp
              pure Nothing
            Right ls -> pure $ Just ls

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

writeLedgerState :: LedgerEnv -> ExtLedgerState CardanoBlock -> IO ()
writeLedgerState env st = atomically $ writeTVar (leStateVar env) (CardanoLedgerState st)

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
getAdaPots :: CardanoLedgerState -> Maybe Generic.AdaPots
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
    -> Either Text (ExtLedgerState CardanoBlock)
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapply cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
                  , " hash ", renderByteArray (Cardano.unChainHash (ledgerTipHash $ ledgerState lsb))
                  , " but block previous hash is "
                  , renderByteArray (Cardano.unChainHash $ blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray (BSS.fromShort . Consensus.getOneEraHash $ blockHash block), "."
                  ]

totalAdaPots
    :: forall era. UsesValue era
    => LedgerState (ShelleyBlock era)
    -> Generic.AdaPots
totalAdaPots lState =
    Generic.AdaPots
      { Generic.apTreasury = Shelley._treasury accountState
      , Generic.apReserves = Shelley._reserves accountState
      , Generic.apRewards = rewards
      , Generic.apUtxo = utxo
      , Generic.apDeposits = Shelley._deposited uState
      , Generic.apFees = Shelley._fees uState
      }
  where
    eState :: EpochState era
    eState = Shelley.nesEs $ Consensus.shelleyLedgerState lState

    accountState :: AccountState
    accountState = Shelley.esAccountState eState

    slState :: Shelley.LedgerState era
    slState = Shelley.esLState eState

    uState :: UTxOState era
    uState = Shelley._utxoState slState

    rewards :: Coin
    rewards = fold (Map.elems (Shelley._rewards . Shelley._dstate $ Shelley._delegationState slState))

    utxo :: Coin
    utxo = Val.coin $ Shelley.balance (Shelley._utxo uState)

getHeaderHash :: HeaderHash CardanoBlock -> ByteString
getHeaderHash bh = BSS.fromShort (Consensus.getOneEraHash bh)
