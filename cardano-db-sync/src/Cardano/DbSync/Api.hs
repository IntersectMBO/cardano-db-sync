{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncOptions (..)
  , mkSyncEnvFromConfig
  , verifyFilePoints
  , getTrace
  , hasLedgerState
  , getLatestPoints
  , getSlotHash
  , getDbLatestBlockInfo
  , getDbTipBlockNo
  , getCurrentTipBlockNo
  , logDbState
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

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, TBQueue, newTBQueueIO, newTVarIO)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point


data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envBackend :: !SqlBackend
  , envOptions :: !SyncOptions
  , envCache :: !Cache
  , envOfflineWorkQueue :: !(TBQueue IO PoolFetchRetry)
  , envOfflineResultQueue :: !(TBQueue IO FetchResult)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envNoLedgerEnv :: !NoLedgerStateEnv -- only used when configured without ledger state.
  , envLedger :: !LedgerEnv
  }

data SyncOptions = SyncOptions
  { soptExtended :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
  , soptLedger :: !Bool
  , snapshotEveryFollowing :: !Word64
  , snapshotEveryLagging :: !Word64
  }

getTrace :: SyncEnv -> Trace IO Text
getTrace = leTrace . envLedger

getSlotHash :: SqlBackend -> SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash backend = DB.runDbIohkNoLogging backend . DB.querySlotHash

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
getDbTipBlockNo env = do
  maybeTip <- getDbLatestBlockInfo (envBackend env)
  case maybeTip of
    Just tip -> pure $ Point.At (bBlockNo tip)
    Nothing -> pure Point.Origin

logDbState :: SyncEnv -> IO ()
logDbState env = do
    mblk <- getDbLatestBlockInfo (envBackend env)
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
  maybeTip <- getDbLatestBlockInfo (envBackend env)
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin

mkSyncEnv
    :: Trace IO Text -> SqlBackend -> SyncOptions -> ProtocolInfo IO CardanoBlock -> Ledger.Network
    -> NetworkMagic -> SystemStart -> LedgerStateDir
    -> IO SyncEnv
mkSyncEnv trce backend syncOptions protoInfo nw nwMagic systemStart dir = do
  ledgerEnv <- mkLedgerEnv trce protoInfo dir nw systemStart (soptAbortOnInvalid syncOptions)
                 (snapshotEveryFollowing syncOptions) (snapshotEveryLagging syncOptions)
  cache <- if soptCache syncOptions then newEmptyCache 100000 else pure uninitiatedCache
  owq <- newTBQueueIO 100
  orq <- newTBQueueIO 100
  epochSyncTime <- newTVarIO =<< getCurrentTime
  noLegdState <- mkNoLedgerStateEnv trce systemStart
  pure $ SyncEnv
          { envProtocol = SyncProtocolCardano
          , envNetworkMagic = nwMagic
          , envSystemStart = systemStart
          , envBackend = backend
          , envOptions = syncOptions
          , envCache = cache
          , envOfflineWorkQueue = owq
          , envOfflineResultQueue = orq
          , envEpochSyncTime = epochSyncTime
          , envNoLedgerEnv = noLegdState
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: Trace IO Text -> SqlBackend -> SyncOptions -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce backend syncOptions dir genCfg =
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
          Right <$> mkSyncEnv trce backend syncOptions (mkProtocolInfoCardano genCfg []) (Shelley.sgNetworkId $ scConfig sCfg)
                      (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                      (SystemStart .Byron.gdStartTime $ Byron.configGenesisData bCfg)
                      dir


getLatestPoints :: SyncEnv -> IO [CardanoPoint]
getLatestPoints env = do
    if hasLedgerState env then do
      files <- listLedgerStateFilesOrdered $ leDir (envLedger env)
      verifyFilePoints env files
    else do
      -- Brings the 5 latest.
      lastPoints <- DB.runDbIohkNoLogging (envBackend env) DB.queryLatestPoints
      pure $ mapMaybe convert' lastPoints
  where
    convert' (Nothing, _) = Nothing
    convert' (Just slot, bs) = convert (SlotNo slot) bs

verifyFilePoints :: SyncEnv -> [LedgerStateFile] -> IO [CardanoPoint]
verifyFilePoints env files =
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe CardanoPoint)
    validLedgerFileToPoint lsf = do
        hashes <- getSlotHash (envBackend env) (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convert slot hash
          _ -> pure Nothing

convert :: SlotNo -> ByteString -> Maybe CardanoPoint
convert slot hashBlob =
    Point . Point.block slot <$> convertHashBlob hashBlob
  where
    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)
