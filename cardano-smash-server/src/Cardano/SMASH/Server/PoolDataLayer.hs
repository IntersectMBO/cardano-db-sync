{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Server.PoolDataLayer where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified Cardano.Db as Db

import           Cardano.SMASH.Server.Types

{- HLINT ignore "Reduce duplication" -}

data PoolDataLayer =
  PoolDataLayer
    { dlGetPoolMetadata         :: PoolId -> PoolMetadataHash -> IO (Either DBFail (TickerName, PoolMetadataRaw))
    , dlAddPoolMetadata         :: Maybe Db.PoolMetadataRefId -> PoolId -> PoolMetadataHash -> PoolMetadataRaw -> Db.ReservedPoolTicker -> IO (Either DBFail PoolMetadataRaw) -- testing

    , dlGetReservedTickers      :: IO [(TickerName, PoolId)]
    , dlAddReservedTicker       :: TickerName -> PoolId -> IO (Either DBFail TickerName)
    , dlCheckReservedTicker     :: TickerName -> IO (Maybe PoolId)

    , dlGetDelistedPools        :: IO [PoolId]
    , dlCheckDelistedPool       :: PoolId -> IO Bool
    , dlAddDelistedPool         :: PoolId -> IO (Either DBFail PoolId)
    , dlRemoveDelistedPool      :: PoolId -> IO (Either DBFail PoolId)

    , dlAddRetiredPool          :: PoolId -> Word64 -> IO (Either DBFail PoolId) -- testing mode
    , dlCheckRetiredPool        :: PoolId -> IO Bool
    , dlGetRetiredPools         :: IO (Either DBFail [PoolId])

    , dlGetFetchErrors          :: PoolId -> Maybe UTCTime -> IO (Either DBFail [PoolFetchError])

    , dlGetPool                 :: PoolId -> IO (Either DBFail PoolId)

    } deriving (Generic)

postgresqlPoolDataLayer :: Trace IO Text -> PoolDataLayer
postgresqlPoolDataLayer tracer = PoolDataLayer {
    dlGetPoolMetadata = \poolId poolMetadataHash -> do
      let poolHash = servantToDbPoolId poolId
      let metaHash = servantToDbPoolMetaHash poolMetadataHash
      mMeta <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ Db.queryPoolOfflineData poolHash metaHash
      case mMeta of
        Just (tickerName, metadata) -> pure $ Right (TickerName tickerName, PoolMetadataRaw metadata)
        Nothing -> pure $ Left $ DbLookupPoolMetadataHash poolId poolMetadataHash
  , dlAddPoolMetadata = panic "dlAddPoolMetadata not defined. Will be used only for testing."
  , dlGetReservedTickers = do
      tickers <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer Db.queryReservedTickers
      pure $ fmap (\ticker -> (TickerName $ Db.reservedPoolTickerName ticker, dbToServantPoolId $ Db.reservedPoolTickerPoolHash ticker)) tickers
  , dlAddReservedTicker = \ticker poolId -> do
      inserted <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $
        Db.insertReservedPoolTicker $
          Db.ReservedPoolTicker (getTickerName ticker) (servantToDbPoolId poolId)
      case inserted of
        Just _ -> pure $ Right ticker
        Nothing -> pure $ Left $ TickerAlreadyReserved ticker
  , dlCheckReservedTicker = \ticker -> do
      Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $
        fmap dbToServantPoolId <$> Db.queryReservedTicker (getTickerName ticker)
  , dlGetDelistedPools = do
      fmap dbToServantPoolId <$> Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer Db.queryDelistedPools
  , dlCheckDelistedPool = \poolHash -> do
      Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ Db.existsDelistedPool (servantToDbPoolId poolHash)
  , dlAddDelistedPool = \poolHash -> do
      Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ do
        let poolHashDb = servantToDbPoolId poolHash
        isAlready <- Db.existsDelistedPool poolHashDb
        if isAlready then return . Left . DbInsertError $ "Delisted pool already exists!"
        else do
          _ <- Db.insertDelistedPool (Db.DelistedPool poolHashDb)
          pure $ Right poolHash
  , dlRemoveDelistedPool = \poolHash -> do
      deleted <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $
        Db.deleteDelistedPool (servantToDbPoolId poolHash)
      if deleted
        then pure $ Right poolHash
        else pure $ Left RecordDoesNotExist
  , dlAddRetiredPool = \_ _ -> panic "dlAddRetiredPool not defined. Will be used only for testing"
  , dlCheckRetiredPool = \poolId -> do
      actions <- getCertActions tracer (Just poolId)
      pure $ not $ isRegistered (servantToDbPoolId poolId) actions
  , dlGetRetiredPools = do
      ls <- filterRetired <$> getCertActions tracer Nothing
      pure $ Right $ dbToServantPoolId <$> ls
  , dlGetFetchErrors = \poolId mTimeFrom -> do
      fetchErrors <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $
        Db.queryPoolOfflineFetchError (servantToDbPoolId poolId) mTimeFrom
      pure $ Right $ dbToServantFetchError poolId <$> fetchErrors
  , dlGetPool = \poolId -> do
      isActive <- isPoolActive tracer poolId
      if isActive
        then pure (Right poolId)
        else pure $ Left RecordDoesNotExist
  }

dbToServantFetchError :: PoolId -> (Db.PoolOfflineFetchError, ByteString) -> PoolFetchError
dbToServantFetchError poolId (fetchError, metaHash) =
  PoolFetchError (utcTimeToPOSIXSeconds $ Db.poolOfflineFetchErrorFetchTime fetchError)
                 poolId
                 (dbToServantMetaHash metaHash)
                 (Db.poolOfflineFetchErrorFetchError fetchError)
                 (Db.poolOfflineFetchErrorRetryCount fetchError)

-- For each pool return the latest certificate action. Also return the
-- current epoch.
getCertActions :: Trace IO Text -> Maybe PoolId -> IO (Maybe Word64, Map ByteString Db.PoolCertAction)
getCertActions tracer mPoolId = do
  (certs, epoch) <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ do
    poolRetired <- Db.queryRetiredPools (servantToDbPoolId <$> mPoolId)
    poolUpdate <- Db.queryPoolRegister (servantToDbPoolId <$> mPoolId)
    currentEpoch <- Db.queryCurrentEpochNo
    pure (poolRetired ++ poolUpdate, currentEpoch)
  let poolActions = findLatestPoolAction certs
  pure (epoch, poolActions)

getActivePools :: Trace IO Text -> Maybe PoolId -> IO (Map ByteString ByteString)
getActivePools tracer mPoolId = do
  (certs, epoch) <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ do
    poolRetired <- Db.queryRetiredPools (servantToDbPoolId <$> mPoolId)
    poolUpdate <- Db.queryPoolRegister (servantToDbPoolId <$> mPoolId)
    currentEpoch <- Db.queryCurrentEpochNo
    pure (poolRetired ++ poolUpdate, currentEpoch)
  pure $ groupByPoolMeta epoch certs

isPoolActive :: Trace IO Text -> PoolId -> IO Bool
isPoolActive tracer poolId = do
  isJust <$> getActiveMetaHash tracer poolId

-- If the pool is not retired, it will return the pool Hash and the latest metadata hash.
getActiveMetaHash :: Trace IO Text -> PoolId -> IO (Maybe (ByteString, ByteString))
getActiveMetaHash tracer poolId = do
  mp <- getActivePools tracer (Just poolId)
  case Map.toList mp of
    [(poolHash, metaHash)] -> pure $ Just (poolHash, metaHash)
    _ -> pure Nothing

filterRetired :: (Maybe Word64, Map ByteString Db.PoolCertAction) -> [ByteString]
filterRetired (mEpochNo, certs) =
    fst <$> filter predRetired (Map.toList certs)
  where
    predRetired (_, pca) = case pca of
      Db.Retirement retEpochNo -> Just retEpochNo <= mEpochNo
      _ -> False

filterRegistered :: (Maybe Word64, Map ByteString Db.PoolCertAction) -> [ByteString]
filterRegistered (mEpochNo, certs) =
    fst <$> filter predRegistered (Map.toList certs)
  where
    predRegistered (_bs, pca) = case pca of
      Db.Retirement retEpochNo -> Just retEpochNo > mEpochNo
      _ -> True

isRegistered :: ByteString -> (Maybe Word64, Map ByteString Db.PoolCertAction) -> Bool
isRegistered pid (mEpochNo, certs) = case Map.lookup pid certs of
  Nothing -> False
  Just (Db.Retirement retEpochNo) -> Just retEpochNo > mEpochNo
  Just (Db.Register _) -> True

servantToDbPoolId :: PoolId -> ByteString
servantToDbPoolId pid =
  case Base16.decode $ Text.encodeUtf8 $ getPoolId pid of
    Left err -> panic $ Text.pack err
    Right bs -> bs

dbToServantPoolId :: ByteString -> PoolId
dbToServantPoolId bs = PoolId $ Text.decodeUtf8 $ Base16.encode bs

servantToDbPoolMetaHash :: PoolMetadataHash -> ByteString
servantToDbPoolMetaHash pmh =
  case Base16.decode $ Text.encodeUtf8 $ getPoolMetadataHash pmh of
    Left err -> panic $ Text.pack err
    Right bs -> bs

dbToServantMetaHash :: ByteString -> PoolMetadataHash
dbToServantMetaHash bs = PoolMetadataHash $ Text.decodeUtf8 $ Base16.encode bs

createCachedPoolDataLayer :: Maybe () -> IO PoolDataLayer
createCachedPoolDataLayer _ = panic "createCachedPoolDataLayer not defined yet"

_getUsedTickers :: Trace IO Text ->  IO [(TickerName, PoolMetadataHash)]
_getUsedTickers tracer = do
  pools <- getActivePools tracer Nothing
  tickers <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ forM (Map.toList pools) $ \(ph, meta) -> do
    mticker <- Db.queryUsedTicker ph meta
    pure $ map (\ticker -> (TickerName ticker, dbToServantMetaHash meta)) mticker
  pure $ catMaybes tickers

_checkUsedTicker :: Trace IO Text -> TickerName -> IO (Maybe TickerName)
_checkUsedTicker tracer ticker = do
    pools <- getActivePools tracer Nothing
    tickers <- Db.runWithConnectionLogging Db.PGPassDefaultEnv tracer $ forM (Map.toList pools) $ \(ph, meta) -> do
      mticker <- Db.queryUsedTicker ph meta
      pure $ map (\tickerText -> (TickerName tickerText, dbToServantMetaHash meta)) mticker
    case Map.lookup ticker (Map.fromList $ catMaybes tickers) of
      Nothing -> pure Nothing
      Just _metaHash -> pure $ Just ticker

findLatestPoolAction :: [Db.PoolCert] -> Map ByteString Db.PoolCertAction
findLatestPoolAction pcerts =
    map Db.pcCertAction $ Map.fromListWith max pcs
  where
    pcs = map (\pc -> (Db.pcHash pc, pc)) pcerts

-- Returns active pools with their metadata hash
groupByPoolMeta :: Maybe Word64 -> [Db.PoolCert] -> Map ByteString ByteString
groupByPoolMeta mEpochNo certs =
    Map.mapMaybeWithKey lastValidRegister groupedByPool
  where
    groupedByPool :: Map ByteString [Db.PoolCert] = groupByKey Db.pcHash certs

    -- Returns the last metadata hash if the pool is not retired.
    lastValidRegister :: ByteString -> [Db.PoolCert] -> Maybe ByteString
    lastValidRegister _poolHash actions = case Db.pcCertAction (maximum actions) of
      Db.Register meta -> Just meta
      Db.Retirement retEpochNo | Just retEpochNo > mEpochNo -> getMaxRegister actions
      Db.Retirement _retEpochNo -> Nothing

    getMaxRegister :: [Db.PoolCert] -> Maybe ByteString
    getMaxRegister actions =
      case filter (isJust . getMetaHash . Db.pcCertAction) actions of
        [] -> Nothing
        ls -> getMetaHash $ Db.pcCertAction $ maximum ls

getMetaHash :: Db.PoolCertAction -> Maybe ByteString
getMetaHash (Db.Retirement _) = Nothing
getMetaHash (Db.Register bs) = Just bs

groupByKey :: Ord k => (v -> k) -> [v] -> Map k [v]
groupByKey getK = Map.fromListWith (++) . fmap (\val -> (getK val, [val]))
