{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncOptions (..)
  , ConsistentLevel (..)
  , setConsistentLevel
  , getConsistentLevel
  , isConsistent
  , getIsSyncFixed
  , setIsFixed
  , mkSyncEnvFromConfig
  , replaceConnection
  , verifySnapshotPoint
  , getTrace
  , getBackend
  , hasLedgerState
  , getLatestPoints
  , getSlotHash
  , getDbLatestBlockInfo
  , getDbTipBlockNo
  , getCurrentTipBlockNo
  , generateNewEpochEvents
  , logDbState
  , convertToPoint
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))

import qualified Cardano.Db as DB

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))

import           Cardano.DbSync.Cache
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.LocalStateQuery
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, TBQueue, newTBQueueIO, newTVarIO,
                   readTVar, readTVarIO, writeTVar)
import           Control.Monad.Trans.Maybe (MaybeT (..))

import qualified Data.Strict.Maybe as Strict
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           Database.Persist.Postgresql (ConnectionString)
import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Block.Abstract (HeaderHash, Point (..), fromRawHash)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point


data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envConnString :: ConnectionString
  , envBackend :: !(StrictTVar IO (Strict.Maybe SqlBackend))
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envIsFixed :: !(StrictTVar IO Bool)
  , envOptions :: !SyncOptions
  , envCache :: !Cache
  , envOfflineWorkQueue :: !(TBQueue IO PoolFetchRetry)
  , envOfflineResultQueue :: !(TBQueue IO FetchResult)
  , envEpochState :: !(StrictTVar IO EpochState)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envNoLedgerEnv :: !NoLedgerStateEnv -- only used when configured without ledger state.
  , envLedger :: !LedgerEnv
  }

data ConsistentLevel = Consistent | DBAheadOfLedger | Unchecked
  deriving (Show, Eq)

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

getIsSyncFixed :: SyncEnv -> IO Bool
getIsSyncFixed = readTVarIO . envIsFixed

setIsFixed :: SyncEnv -> IO ()
setIsFixed env =
  atomically $ writeTVar (envIsFixed env) True

data SyncOptions = SyncOptions
  { soptExtended :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
  , soptLedger :: !Bool
  , soptSkipFix :: !Bool
  , soptOnlyFix :: !Bool
  , snapshotEveryFollowing :: !Word64
  , snapshotEveryLagging :: !Word64
  }

replaceConnection :: SyncEnv -> SqlBackend -> IO ()
replaceConnection env sqlBackend = do
  atomically $ writeTVar (envBackend env) $ Strict.Just sqlBackend

data EpochState = EpochState
  { esInitialized :: !Bool
  , esEpochNo :: !(Strict.Maybe EpochNo)
  }

initEpochState :: EpochState
initEpochState =
    EpochState
      { esInitialized = False
      , esEpochNo = Strict.Nothing
      }

generateNewEpochEvents :: SyncEnv -> SlotDetails -> STM [LedgerEvent]
generateNewEpochEvents env details = do
    !oldEpochState <- readTVar (envEpochState env)
    writeTVar (envEpochState env) newEpochState
    pure $ maybeToList (newEpochEvent oldEpochState)
  where
    currentEpochNo :: EpochNo
    currentEpochNo = sdEpochNo details

    newEpochEvent :: EpochState -> Maybe LedgerEvent
    newEpochEvent oldEpochState =
      case esEpochNo oldEpochState of
        Strict.Nothing -> Just $ LedgerStartAtEpoch currentEpochNo
        Strict.Just oldEpoch ->
          if currentEpochNo == 1 + oldEpoch
            then Just $ LedgerNewEpoch currentEpochNo (getSyncStatus details)
            else Nothing

    newEpochState :: EpochState
    newEpochState =
      EpochState
        { esInitialized = True
        , esEpochNo = Strict.Just currentEpochNo
        }

getTrace :: SyncEnv -> Trace IO Text
getTrace = leTrace . envLedger

getSlotHash :: SqlBackend -> SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash backend = DB.runDbIohkNoLogging backend . DB.querySlotHash

getBackend :: SyncEnv -> IO SqlBackend
getBackend env = do
    mBackend <- readTVarIO $ envBackend env
    case mBackend of
      Strict.Just conn -> pure conn
      Strict.Nothing -> panic "sql connection not initiated"

hasLedgerState :: SyncEnv -> Bool
hasLedgerState = soptLedger . envOptions

getDbLatestBlockInfo :: SqlBackend -> IO (Maybe TipInfo)
getDbLatestBlockInfo backend = do
  runMaybeT $ do
    block <- MaybeT $ DB.runDbIohkNoLogging backend DB.queryLatestBlock
    -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
    -- era, but we need to make the types match, hence `fromMaybe`.
    pure $ TipInfo
            { bHash = DB.blockHash block
            , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
            , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
            , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
            }

getDbTipBlockNo :: SyncEnv -> IO (Point.WithOrigin BlockNo)
getDbTipBlockNo env =
  getBackend env >>=
    getDbLatestBlockInfo <&>
    maybe Point.Origin (Point.At . bBlockNo)

logDbState :: SyncEnv -> IO ()
logDbState env = do
    backend <- getBackend env
    mblk <- getDbLatestBlockInfo backend
    case mblk of
      Nothing -> logInfo (getTrace env) "Cardano.Db is empty"
      Just tip -> logInfo (getTrace env) $ mconcat [ "Cardano.Db tip is at ", showTip tip ]
  where
    showTip :: TipInfo -> Text
    showTip tipInfo =
      mconcat
        [ "slot ", DB.textShow (unSlotNo $ bSlotNo tipInfo)
        , ", block ", DB.textShow (unBlockNo $ bBlockNo tipInfo)
        ]

getCurrentTipBlockNo :: SyncEnv -> IO (WithOrigin BlockNo)
getCurrentTipBlockNo env = do
  backend <- getBackend env
  maybeTip <- getDbLatestBlockInfo backend
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin

mkSyncEnv
    :: Trace IO Text -> ConnectionString -> SyncOptions -> ProtocolInfo IO CardanoBlock -> Ledger.Network
    -> NetworkMagic -> SystemStart -> LedgerStateDir
    -> IO SyncEnv
mkSyncEnv trce connSring syncOptions protoInfo nw nwMagic systemStart dir = do
  ledgerEnv <- mkLedgerEnv trce protoInfo dir nw systemStart (soptAbortOnInvalid syncOptions)
                 (snapshotEveryFollowing syncOptions) (snapshotEveryLagging syncOptions)
  cache <- if soptCache syncOptions then newEmptyCache 100000 else pure uninitiatedCache
  backendVar <- newTVarIO Strict.Nothing
  consistentLevelVar <- newTVarIO Unchecked
  fixDataVar <- newTVarIO False
  owq <- newTBQueueIO 100
  orq <- newTBQueueIO 100
  epochVar <- newTVarIO initEpochState
  epochSyncTime <- newTVarIO =<< getCurrentTime
  noLegdState <- mkNoLedgerStateEnv trce systemStart
  pure $ SyncEnv
          { envProtocol = SyncProtocolCardano
          , envNetworkMagic = nwMagic
          , envSystemStart = systemStart
          , envConnString = connSring
          , envBackend = backendVar
          , envOptions = syncOptions
          , envConsistentLevel = consistentLevelVar
          , envIsFixed = fixDataVar
          , envCache = cache
          , envOfflineWorkQueue = owq
          , envOfflineResultQueue = orq
          , envEpochState = epochVar
          , envEpochSyncTime = epochSyncTime
          , envNoLedgerEnv = noLegdState
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: Trace IO Text -> ConnectionString -> SyncOptions -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce connSring syncOptions dir genCfg =
  case genCfg of
    GenesisCardano _ bCfg sCfg _
      | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
          pure . Left . NECardanoConfig $
            mconcat
              [ "ProtocolMagicId ", DB.textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
              , " /= ", DB.textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
              ]
      | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
          pure . Left . NECardanoConfig $
            mconcat
              [ "SystemStart ", DB.textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
              , " /= ", DB.textShow (Shelley.sgSystemStart $ scConfig sCfg)
              ]
      | otherwise ->
          Right <$> mkSyncEnv trce connSring syncOptions (mkProtocolInfoCardano genCfg []) (Shelley.sgNetworkId $ scConfig sCfg)
                      (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                      (SystemStart .Byron.gdStartTime $ Byron.configGenesisData bCfg)
                      dir


-- | 'True' is for in memory points and 'False' for on disk
getLatestPoints :: SyncEnv -> IO [(CardanoPoint, Bool)]
getLatestPoints env = do
    if hasLedgerState env
      then do
        snapshotPoints <- listKnownSnapshots $ envLedger env
        verifySnapshotPoint env snapshotPoints
      else do
        -- Brings the 5 latest.
        dbBackend <- getBackend env
        lastPoints <- DB.runDbIohkNoLogging dbBackend DB.queryLatestPoints
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
        backend <- getBackend env
        hashes <- getSlotHash backend (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convertToDiskPoint slot hash
          _ -> pure Nothing
    validLedgerFileToPoint (InMemory pnt) = do
        case pnt of
          GenesisPoint -> pure Nothing
          BlockPoint slotNo hsh -> do
            backend <- getBackend env
            hashes <- getSlotHash backend slotNo
            let valid  = find (\(_, dbHash) -> getHeaderHash hsh == dbHash) hashes
            case valid of
              Just (dbSlotNo, _) | slotNo == dbSlotNo -> pure $ Just (pnt, True)
              _ -> pure Nothing

convertToDiskPoint :: SlotNo -> ByteString -> Maybe (CardanoPoint, Bool)
convertToDiskPoint slot hashBlob = (, False) <$> convertToPoint slot hashBlob

convertToPoint :: SlotNo -> ByteString -> Maybe CardanoPoint
convertToPoint slot hashBlob =
    Point . Point.block slot <$> convertHashBlob hashBlob
  where
    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)
