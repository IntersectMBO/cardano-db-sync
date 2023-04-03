{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.LedgerState (
  CardanoLedgerState (..),
  HasLedgerEnv (..),
  LedgerEvent (..),
  ApplyResult (..),
  LedgerStateFile (..),
  SnapshotPoint (..),
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
  saveCleanupState,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Cardano.Binary (Decoder, DecoderError, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as Serialize
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Era.Cardano.Util as Cardano
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.LedgerEvent
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
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
  StrictTVar,
  atomically,
  newTVarIO,
  readTVar,
  writeTVar,
 )
import qualified Control.Exception as Exception

-- import           Codec.CBOR.Write (toBuilder)
import qualified Data.ByteString.Base16 as Base16

-- import           Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
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
  getTipSlot,
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
import Ouroboros.Network.AnchoredSeq (Anchorable (..), AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Block (HeaderHash, Point (..), blockNo)
import qualified Ouroboros.Network.Point as Point
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (dropExtension, takeExtension, (</>))
import System.Mem (performMajorGC)
import Prelude (String, fail, id)

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use readTVarIO" -}

data HasLedgerEnv = HasLedgerEnv
  { leTrace :: Trace IO Text
  , leProtocolInfo :: !(Consensus.ProtocolInfo IO CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leSystemStart :: !SystemStart
  , leAbortOnPanic :: !Bool
  , leSnapshotEveryFollowing :: !Word64
  , leSnapshotEveryLagging :: !Word64
  , leInterpreter :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar :: !(StrictTVar IO (Strict.Maybe LedgerDB))
  }

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(ExtLedgerState CardanoBlock)
  , clsEpochBlockNo :: !EpochBlockNo
  }

-- The height of the block in the current Epoch. We maintain this
-- data next to the ledger state and store it in the same blob file.
data EpochBlockNo
  = GenesisEpochBlockNo
  | EBBEpochBlockNo
  | EpochBlockNo !Word64

instance ToCBOR EpochBlockNo where
  toCBOR GenesisEpochBlockNo = toCBOR (0 :: Word8)
  toCBOR EBBEpochBlockNo = toCBOR (1 :: Word8)
  toCBOR (EpochBlockNo n) =
    toCBOR (2 :: Word8) <> toCBOR n

instance FromCBOR EpochBlockNo where
  fromCBOR = do
    tag :: Word8 <- fromCBOR
    case tag of
      0 -> pure GenesisEpochBlockNo
      1 -> pure EBBEpochBlockNo
      2 -> EpochBlockNo <$> fromCBOR
      n -> fail $ "unexpected EpochBlockNo value " <> show n

encodeCardanoLedgerState :: (ExtLedgerState CardanoBlock -> Encoding) -> CardanoLedgerState -> Encoding
encodeCardanoLedgerState encodeExt cls =
  mconcat
    [ encodeExt (clsState cls)
    , toCBOR (clsEpochBlockNo cls)
    ]

decodeCardanoLedgerState ::
  (forall s. Decoder s (ExtLedgerState CardanoBlock)) ->
  (forall s. Decoder s CardanoLedgerState)
decodeCardanoLedgerState decodeExt = do
  ldgrState <- decodeExt
  CardanoLedgerState ldgrState <$> fromCBOR

data LedgerStateFile = LedgerStateFile
  { lsfSlotNo :: !SlotNo
  , lsfHash :: !ByteString
  , lsNewEpoch :: !(Strict.Maybe EpochNo)
  , lsfFilePath :: !FilePath
  }
  deriving (Show)

-- The result of applying a new block. This includes all the data that insertions require.
data ApplyResult = ApplyResult
  { apPrices :: !(Strict.Maybe Prices) -- prices after the block application
  , apPoolsRegistered :: !(Set.Set PoolKeyHash) -- registered before the block application
  , apNewEpoch :: !(Strict.Maybe Generic.NewEpoch) -- Only Just for a single block at the epoch boundary
  , apSlotDetails :: !SlotDetails
  , apStakeSlice :: !Generic.StakeSliceRes
  , apEvents :: ![LedgerEvent]
  }

defaultApplyResult :: SlotDetails -> ApplyResult
defaultApplyResult slotDetails =
  ApplyResult
    { apPrices = Strict.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch = Strict.Nothing
    , apSlotDetails = slotDetails
    , apStakeSlice = Generic.NoSlices
    , apEvents = []
    }

newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: AnchoredSeq (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState
  }

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

instance Anchorable (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . clsState

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: LedgerDB -> CardanoLedgerState
ledgerDbCurrent = either id id . AS.head . ledgerDbCheckpoints

mkHasLedgerEnv ::
  Trace IO Text ->
  Consensus.ProtocolInfo IO CardanoBlock ->
  LedgerStateDir ->
  Ledger.Network ->
  SystemStart ->
  Bool ->
  Word64 ->
  Word64 ->
  IO HasLedgerEnv
mkHasLedgerEnv trce protoInfo dir nw systemStart aop snapshotEveryFollowing snapshotEveryLagging = do
  svar <- newTVarIO Strict.Nothing
  intervar <- newTVarIO Strict.Nothing
  pure
    HasLedgerEnv
      { leTrace = trce
      , leProtocolInfo = protoInfo
      , leDir = dir
      , leNetwork = nw
      , leSystemStart = systemStart
      , leAbortOnPanic = aop
      , leSnapshotEveryFollowing = snapshotEveryFollowing
      , leSnapshotEveryLagging = snapshotEveryLagging
      , leInterpreter = intervar
      , leStateVar = svar
      }

initCardanoLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> CardanoLedgerState
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
    Strict.Nothing -> panic "LedgerState.readStateUnsafe: Ledger state is not found"
    Strict.Just st -> pure st

applyBlockAndSnapshot :: HasLedgerEnv -> CardanoBlock -> IO (ApplyResult, Bool)
applyBlockAndSnapshot ledgerEnv blk = do
  (oldState, appResult) <- applyBlock ledgerEnv blk
  tookSnapshot <- storeSnapshotAndCleanupMaybe ledgerEnv oldState appResult (blockNo blk) (isSyncedWithinSeconds (apSlotDetails appResult) 600)
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
    let !result = applyBlk (ExtLedgerCfg (getTopLevelconfigHasLedger env)) blk (clsState oldState)
    let !newLedgerState = lrResult result
    !details <- getSlotDetails env (ledgerState newLedgerState) time (cardanoBlockSlotNo blk)
    let !newEpoch = mkNewEpoch (clsState oldState) newLedgerState
    let !newEpochBlockNo = applyToEpochBlockNo (isJust $ blockIsEBB blk) (isJust newEpoch) (clsEpochBlockNo oldState)
    let !newState = CardanoLedgerState newLedgerState newEpochBlockNo
    let !ledgerDB' = pushLedgerDB ledgerDB newState
    writeTVar (leStateVar env) (Strict.Just ledgerDB')
    let !ledgerEvents = mapMaybe convertAuxLedgerEvent (lrEvents result)
    let !appResult =
          ApplyResult
            { apPrices = getPrices newState
            , apPoolsRegistered = getRegisteredPools oldState
            , apNewEpoch = maybeToStrict newEpoch
            , apSlotDetails = details
            , apStakeSlice = stakeSlice newState details
            , apEvents = ledgerEvents
            }
    pure (oldState, appResult)
  where
    applyBlk ::
      ExtLedgerCfg CardanoBlock ->
      CardanoBlock ->
      ExtLedgerState CardanoBlock ->
      LedgerResult (ExtLedgerState CardanoBlock) (ExtLedgerState CardanoBlock)
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> panic err
        Right result -> result

    mkNewEpoch :: ExtLedgerState CardanoBlock -> ExtLedgerState CardanoBlock -> Maybe Generic.NewEpoch
    mkNewEpoch oldState newState =
      if ledgerEpochNo env newState /= ledgerEpochNo env oldState + 1
        then Nothing
        else
          Just $
            Generic.NewEpoch
              { Generic.neEpoch = ledgerEpochNo env newState
              , Generic.neIsEBB = isJust $ blockIsEBB blk
              , Generic.neAdaPots = maybeToStrict $ getAdaPots newState
              , Generic.neEpochUpdate = Generic.epochUpdate newState
              }

    applyToEpochBlockNo :: Bool -> Bool -> EpochBlockNo -> EpochBlockNo
    applyToEpochBlockNo True _ _ = EBBEpochBlockNo
    applyToEpochBlockNo _ True _ = EpochBlockNo 0
    applyToEpochBlockNo _ _ (EpochBlockNo n) = EpochBlockNo (n + 1)
    applyToEpochBlockNo _ _ GenesisEpochBlockNo = EpochBlockNo 0
    applyToEpochBlockNo _ _ EBBEpochBlockNo = EpochBlockNo 0

    stakeSliceMinSize :: Word64
    stakeSliceMinSize = 2000

    stakeSlice :: CardanoLedgerState -> SlotDetails -> Generic.StakeSliceRes
    stakeSlice cls details =
      case clsEpochBlockNo cls of
        EpochBlockNo n ->
          Generic.getStakeSlice
            (leProtocolInfo env)
            (sdEpochNo details)
            n
            stakeSliceMinSize
            (clsState cls)
        _ -> Generic.NoSlices

storeSnapshotAndCleanupMaybe ::
  HasLedgerEnv ->
  CardanoLedgerState ->
  ApplyResult ->
  BlockNo ->
  SyncState ->
  IO Bool
storeSnapshotAndCleanupMaybe env oldState appResult blkNo syncState =
  case maybeFromStrict (apNewEpoch appResult) of
    Just newEpoch -> do
      let newEpochNo = Generic.neEpoch newEpoch
      -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
      liftIO $ saveCleanupState env oldState (Just $ newEpochNo - 1)
      pure True
    Nothing ->
      if timeToSnapshot syncState blkNo
        then do
          liftIO $ saveCleanupState env oldState Nothing
          pure True
        else pure False
  where
    timeToSnapshot :: SyncState -> BlockNo -> Bool
    timeToSnapshot syncSt bNo =
      case (syncSt, unBlockNo bNo) of
        (SyncFollowing, bno) -> bno `mod` leSnapshotEveryFollowing env == 0
        (SyncLagging, bno) -> bno `mod` leSnapshotEveryLagging env == 0

saveCurrentLedgerState :: HasLedgerEnv -> CardanoLedgerState -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env ledger mEpochNo = do
  case mkLedgerStateFilename (leDir env) (clsState ledger) mEpochNo of
    Origin -> pure () -- we don't store genesis
    At file -> do
      exists <- doesFileExist file
      if exists
        then
          logInfo (leTrace env) $
            mconcat
              ["File ", Text.pack file, " exists"]
        else do
          startTime <- getCurrentTime
          -- TODO: write the builder directly.
          -- BB.writeFile file $ toBuilder $
          LBS.writeFile file $
            Serialize.serializeEncoding $
              encodeCardanoLedgerState
                ( Consensus.encodeExtLedgerState
                    (encodeDisk codecConfig)
                    (encodeDisk codecConfig)
                    (encodeDisk codecConfig)
                )
                ledger
          endTime <- getCurrentTime
          logInfo (leTrace env) $
            mconcat
              [ "Took a ledger snapshot at "
              , Text.pack file
              , ". It took "
              , textShow (diffUTCTime endTime startTime)
              , "."
              ]
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec (getTopLevelconfigHasLedger env)

mkLedgerStateFilename :: LedgerStateDir -> ExtLedgerState CardanoBlock -> Maybe EpochNo -> WithOrigin FilePath
mkLedgerStateFilename dir ledger mEpochNo =
  lsfFilePath . dbPointToFileName dir mEpochNo
    <$> getPoint (ledgerTipPoint (Proxy @CardanoBlock) (ledgerState ledger))

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
  deleteAndLogStateFile env "old" (List.drop 6 valid)
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

data SnapshotPoint = OnDisk LedgerStateFile | InMemory CardanoPoint

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

getRegisteredPoolShelley ::
  forall p era.
  (Crypto era ~ StandardCrypto) =>
  LedgerState (ShelleyBlock p era) ->
  Set.Set PoolKeyHash
getRegisteredPoolShelley lState =
  Map.keysSet $
    Shelley._pParams $
      Shelley.dpsPState $
        Shelley.lsDPState $
          Shelley.esLState $
            Shelley.nesEs $
              Consensus.shelleyLedgerState lState

-- We only compute 'AdaPots' for later eras. This is a time consuming
-- function and we only want to run it on epoch boundaries.
getAdaPots :: ExtLedgerState CardanoBlock -> Maybe Shelley.AdaPots
getAdaPots st =
  case ledgerState st of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sts -> Just $ totalAdaPots sts
    LedgerStateAllegra sta -> Just $ totalAdaPots sta
    LedgerStateMary stm -> Just $ totalAdaPots stm
    LedgerStateAlonzo sta -> Just $ totalAdaPots sta
    LedgerStateBabbage stb -> Just $ totalAdaPots stb

ledgerEpochNo :: HasLedgerEnv -> ExtLedgerState CardanoBlock -> EpochNo
ledgerEpochNo env cls =
  case ledgerTipSlot (ledgerState cls) of
    Origin -> 0 -- An empty chain is in epoch 0
    NotOrigin slot ->
      case runExcept $ epochInfoEpoch epochInfo slot of
        Left err -> panic $ "ledgerEpochNo: " <> textShow err
        Right en -> en
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo = epochInfoLedger (configLedger $ getTopLevelconfigHasLedger env) (hardForkLedgerStatePerEra $ ledgerState cls)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash ::
  ExtLedgerCfg CardanoBlock ->
  CardanoBlock ->
  ExtLedgerState CardanoBlock ->
  Either Text (LedgerResult (ExtLedgerState CardanoBlock) (ExtLedgerState CardanoBlock))
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapplyLedgerResult cfg block lsb
    else
      Left $
        mconcat
          [ "Ledger state hash mismatch. Ledger head is slot "
          , textShow (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
          , " hash "
          , renderByteArray (Cardano.unChainHash (ledgerTipHash $ ledgerState lsb))
          , " but block previous hash is "
          , renderByteArray (Cardano.unChainHash $ blockPrevHash block)
          , " and block current hash is "
          , renderByteArray (SBS.fromShort . Consensus.getOneEraHash $ blockHash block)
          , "."
          ]

totalAdaPots ::
  forall p era.
  Core.EraTxOut era =>
  LedgerState (ShelleyBlock p era) ->
  Shelley.AdaPots
totalAdaPots = Shelley.totalAdaPotsES . Shelley.nesEs . Consensus.shelleyLedgerState

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
    Strict.Just $
      Alonzo._prices $
        esPp $
          Shelley.nesEs $
            Consensus.shelleyLedgerState als
  LedgerStateBabbage bls ->
    Strict.Just $
      Babbage._prices $
        esPp $
          Shelley.nesEs $
            Consensus.shelleyLedgerState bls
  _ -> Strict.Nothing
