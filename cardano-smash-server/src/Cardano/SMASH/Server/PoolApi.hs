{-# LANGUAGE DeriveGeneric #-}

module Cardano.SMASH.Server.PoolApi where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)

import           Cardano.Db (AdminUser, PoolMetadataRefId, PoolUrl, ReservedPoolTicker,
                   PoolOfflineData (..), runWithConnectionLogging, existsPoolHash,
                   queryPoolOfflineData, queryReservedTickers, queryRetiredPools)

import           Cardano.SMASH.Server.Types


data PoolApi =
  PoolApi
    { dlGetPoolMetadata         :: PoolId -> PoolMetadataHash -> IO (Either DBFail (TickerName, PoolMetadataRaw))
    , dlAddPoolMetadata         :: Maybe PoolMetadataRefId -> PoolId -> PoolMetadataHash -> PoolMetadataRaw -> ReservedPoolTicker -> IO (Either DBFail PoolMetadataRaw) -- testing

    , dlGetReservedTickers      :: IO [(TickerName, PoolMetadataHash)]
    , dlAddReservedTicker       :: TickerName -> PoolMetadataHash -> IO (Either DBFail TickerName)
    , dlCheckReservedTicker     :: TickerName -> PoolMetadataHash -> IO (Maybe TickerName)

    , dlGetDelistedPools        :: IO [PoolId]
    , dlCheckDelistedPool       :: PoolId -> IO Bool
    , dlAddDelistedPool         :: PoolId -> IO (Either DBFail PoolId)
    , dlRemoveDelistedPool      :: PoolId -> IO (Either DBFail PoolId)

    , dlAddRetiredPool          :: PoolId -> Word64 -> IO (Either DBFail PoolId) -- testing mode
    , dlCheckRetiredPool        :: PoolId -> IO (Either DBFail (PoolId, Word64))
    , dlGetRetiredPools         :: IO (Either DBFail [PoolId])
    , dlRemoveRetiredPool       :: PoolId -> IO (Either DBFail PoolId) -- Used only while inserting reg cert

    , dlGetAdminUsers           :: IO (Either DBFail [AdminUser])
    , dlAddAdminUser            :: ApplicationUser -> IO (Either DBFail AdminUser)
    , dlRemoveAdminUser         :: ApplicationUser -> IO (Either DBFail AdminUser)

    , dlGetFetchErrors          :: PoolId -> Maybe UTCTime -> IO (Either DBFail [PoolFetchError])

    , dlGetPool                 :: PoolId -> IO (Either DBFail PoolId)

    } deriving (Generic)

postgresqlPoolApi :: Trace IO Text -> PoolApi
postgresqlPoolApi tracer = PoolApi {
    dlGetPoolMetadata = \poolId poolMetadataHash -> do
    emd <- runWithConnectionLogging tracer $
      queryPoolOfflineData (servantToDbPoolId poolId) (servantToDbPoolMetaHash poolMetadataHash)
    case emd of
      Nothing -> pure $ Left $ DbLookupPoolMetadataHash poolId poolMetadataHash
      Just md -> pure $ Right (TickerName $ poolOfflineDataTickerName md,
                              PoolMetadataRaw $ poolOfflineDataJson md)
  , dlAddPoolMetadata = undefined
  , dlGetReservedTickers = do
       ls <- runWithConnectionLogging tracer $ queryReservedTickers
       pure $ fmap (\(tickerName, mdHash) -> (TickerName tickerName, dbToServantMetaHash mdHash)) ls
  , dlAddReservedTicker = undefined
  , dlCheckReservedTicker = undefined
  , dlGetDelistedPools = undefined
  , dlCheckDelistedPool = undefined
  , dlAddDelistedPool = undefined
  , dlRemoveDelistedPool = undefined
  , dlAddRetiredPool = undefined
  , dlCheckRetiredPool = undefined
  , dlGetRetiredPools = do
      ls <- runWithConnectionLogging tracer $ queryRetiredPools
      pure $ Right $ dbToServantPoolId <$> ls
  , dlRemoveRetiredPool = undefined
  , dlGetAdminUsers = undefined
  , dlAddAdminUser = undefined
  , dlRemoveAdminUser = undefined
  , dlGetFetchErrors = undefined
  , dlGetPool = \poolId -> do
      exst <- runWithConnectionLogging tracer $ existsPoolHash (servantToDbPoolId poolId)
      if exst then pure (Right poolId) else pure (Left RecordDoesNotExist)
  }

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

createCachedPoolApi :: Maybe () -> IO PoolApi
createCachedPoolApi = undefined
