{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.DB
    ( module X
    , DataLayer (..)
    , cachedDataLayer
    , createCachedDataLayer
    , postgresqlDataLayer
    -- * Examples
    , InMemoryCacheIORef (..)
    , InMemoryCache (..)
    ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.SMASH.DBSync.Db.Delete (deleteAdminUser, deleteDelistedPool,
                   deleteRetiredPool)
import           Cardano.SMASH.DBSync.Db.Insert (insertAdminUser, insertDelistedPool, insertPool,
                   insertPoolMetadata, insertPoolMetadataFetchError, insertPoolMetadataRef,
                   insertReservedTicker, insertRetiredPool)
import           Cardano.SMASH.DBSync.Db.Query
import           Cardano.SMASH.Types

import           Cardano.Db as X
import           Cardano.SMASH.Db.Error as X

-- | This is the data layer for the DB.
-- The resulting operation has to be @IO@, it can be made more granular,
-- but currently there is no complexity involved for that to be a sane choice.
data DataLayer = DataLayer
    { dlGetPoolMetadata         :: PoolIdentifier -> PoolMetaHash -> IO (Either DBFail (TickerName, PoolMetadataRaw))
    , dlGetAllPoolMetadata      :: IO [PoolMetadata]
    , dlAddPoolMetadata         :: Maybe PoolMetadataRefId -> PoolIdentifier -> PoolMetaHash -> PoolMetadataRaw -> PoolTicker -> IO (Either DBFail PoolMetadataRaw)

    , dlAddMetaDataReference    :: PoolIdentifier -> PoolUrl -> PoolMetaHash -> IO (Either DBFail PoolMetadataRefId)

    , dlGetReservedTickers      :: IO [(TickerName, PoolMetaHash)]
    , dlAddReservedTicker       :: TickerName -> PoolMetaHash -> IO (Either DBFail TickerName)
    , dlCheckReservedTicker     :: TickerName -> PoolMetaHash -> IO (Maybe TickerName)

    , dlGetDelistedPools        :: IO [PoolIdentifier]
    , dlCheckDelistedPool       :: PoolIdentifier -> IO Bool
    , dlAddDelistedPool         :: PoolIdentifier -> IO (Either DBFail PoolIdentifier)
    , dlRemoveDelistedPool      :: PoolIdentifier -> IO (Either DBFail PoolIdentifier)

    , dlAddRetiredPool          :: PoolIdentifier -> Word64 -> IO (Either DBFail PoolIdentifier)
    , dlCheckRetiredPool        :: PoolIdentifier -> IO (Either DBFail (PoolIdentifier, Word64))
    , dlGetRetiredPools         :: IO (Either DBFail [(PoolIdentifier, Word64)])
    , dlRemoveRetiredPool       :: PoolIdentifier -> IO (Either DBFail PoolIdentifier)

    , dlGetAdminUsers           :: IO (Either DBFail [AdminUser])
    , dlAddAdminUser            :: ApplicationUser -> IO (Either DBFail AdminUser)
    , dlRemoveAdminUser         :: ApplicationUser -> IO (Either DBFail AdminUser)

    -- TODO(KS): Switch to PoolFetchError!
    , dlAddFetchError           :: PoolMetadataFetchError -> IO (Either DBFail PoolMetadataFetchErrorId)
    , dlGetFetchErrors          :: PoolIdentifier -> Maybe UTCTime -> IO (Either DBFail [PoolFetchError])

    , dlGetPool                 :: PoolIdentifier -> IO (Either DBFail PoolIdentifier)
    , dlAddPool                 :: PoolIdentifier -> IO (Either DBFail PoolIdentifier)

    } deriving (Generic)

-- | The in-memory cache that server as a front-end to the DB calls.
data InMemoryCache = InMemoryCache
    { imcDelistedPools :: [PoolIdentifier]
    , imcRetiredPools :: [(PoolIdentifier, Word64)]
    , imcReservedTickers :: [(TickerName, PoolMetaHash)]
    , imcMetadata :: Map (PoolIdentifier, PoolMetaHash) (TickerName, PoolMetadataRaw)
    } deriving (Eq, Show, Generic)

newtype InMemoryCacheIORef = InMemoryCacheIORef (IORef InMemoryCache)
    deriving (Eq)

-- | Caching @DataLayer@.
-- We do need state here.
-- _This is thread safe._
--
-- Why we use a single structure (atomicity and space leak):
--
-- Docs: Extending the atomicity to multiple IORefs is problematic,
-- so it is recommended that if you need to do anything more complicated then
-- using MVar instead is a good idea.
--
-- Docs: Be warned that modifyIORef does not apply the function strictly.
-- This means if the program calls modifyIORef many times,
-- but seldomly uses the value, thunks will pile up in memory resulting
-- in a space leak. This is a common mistake made when using an IORef as a counter.
--
-- Before we even start serving this, we need to populate it with the DB data!
--
-- Maps make sure we do O(log n) when possible.
-- We shouldn't use @HashMap@ because of collision (attack vectors).
--
-- Reads will be REALLY fast and writes will be a little slower.
cachedDataLayer
    :: DataLayer
    -> InMemoryCacheIORef
    -> DataLayer
cachedDataLayer dbDataLayer (InMemoryCacheIORef inMemoryCacheIORef) =
    DataLayer
        -- Cache hit
        { dlGetPoolMetadata     = \poolId poolMetadataHash' -> do
            inMemoryCache <- readIORef inMemoryCacheIORef

            let metadataMap :: Map (PoolIdentifier, PoolMetaHash) (TickerName, PoolMetadataRaw)
                metadataMap = imcMetadata inMemoryCache

            let maybeMetadata = Map.lookup (poolId, poolMetadataHash') metadataMap

            case maybeMetadata of
                Nothing -> return $ Left (DbLookupPoolMetadataHash poolId poolMetadataHash')
                Just tickerNamePoolMeta -> return $ Right tickerNamePoolMeta

        , dlGetAllPoolMetadata = dlGetAllPoolMetadata dbDataLayer
        , dlAddPoolMetadata     = \mPoolMetadataRefId poolId poolMetadataHash' poolMetadata poolTicker -> runExceptT $ do
            -- Modify database
            let addPoolMetadata = dlAddPoolMetadata dbDataLayer
            poolMetadataRaw <-  ExceptT $ addPoolMetadata
                                    mPoolMetadataRefId
                                    poolId
                                    poolMetadataHash'
                                    poolMetadata
                                    poolTicker

            -- TODO(KS): Horrible, I know. Will fix.
            let tickerName = TickerName $ getPoolTicker poolTicker

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let metadataMap :: Map (PoolIdentifier, PoolMetaHash) (TickerName, PoolMetadataRaw)
                    metadataMap = imcMetadata inMemoryCache

                    newMetadataMap = Map.insert (poolId, poolMetadataHash') (tickerName, poolMetadata) metadataMap

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcMetadata = newMetadataMap }

                in  (newInMemoryCache, ())

            return poolMetadataRaw

        , dlAddMetaDataReference = dlAddMetaDataReference dbDataLayer

        -- TODO(KS): Cache hit?
        , dlGetReservedTickers = do
            inMemoryCache <- readIORef inMemoryCacheIORef
            return $ imcReservedTickers inMemoryCache
        , dlAddReservedTicker = \tickerName poolMetadataHash' -> runExceptT $ do
            -- Modify database
            let addReservedTicker = dlAddReservedTicker dbDataLayer
            tickerName' <- ExceptT $ addReservedTicker tickerName poolMetadataHash'

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let reservedTickers = imcReservedTickers inMemoryCache

                    newReservedTickers = (tickerName', poolMetadataHash') : reservedTickers

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcReservedTickers = newReservedTickers }

                in  (newInMemoryCache, ())

            return tickerName'

        -- Cache hit
        , dlCheckReservedTicker = \tickerName poolMetadataHash' -> do
            inMemoryCache <- readIORef inMemoryCacheIORef
            let reservedTickers = imcReservedTickers inMemoryCache
            let isTickerReserved = (tickerName, poolMetadataHash') `elem` reservedTickers
            if isTickerReserved
                then return $ Just tickerName
                else return Nothing

        , dlGetDelistedPools = do
            inMemoryCache <- readIORef inMemoryCacheIORef
            let delistedPools = imcDelistedPools inMemoryCache
            return delistedPools

        -- Cache hit.
        , dlCheckDelistedPool = \poolId -> do
            inMemoryCache <- readIORef inMemoryCacheIORef
            let delistedPools = imcDelistedPools inMemoryCache
            return $ poolId `elem` delistedPools

        , dlAddDelistedPool  = \poolId -> runExceptT $ do
            -- Modify database
            let addDelistedPool = dlAddDelistedPool dbDataLayer
            poolId' <- ExceptT $ addDelistedPool poolId

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let delistedPools = imcDelistedPools inMemoryCache

                    newDelistedPools = poolId' : delistedPools

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcDelistedPools = newDelistedPools }

                in  (newInMemoryCache, ())

            return poolId'

        , dlRemoveDelistedPool = \poolId -> runExceptT $ do
            -- Modify database
            let removeDelistedPool = dlRemoveDelistedPool dbDataLayer
            poolId' <- ExceptT $ removeDelistedPool poolId

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let delistedPools = imcDelistedPools inMemoryCache

                    newDelistedPools = filter (/= poolId') delistedPools

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcDelistedPools = newDelistedPools }

                in  (newInMemoryCache, ())


            return poolId'

        , dlAddRetiredPool      = \poolId blockNo -> runExceptT $ do
             -- Modify database
            let addRetiredPool = dlAddRetiredPool dbDataLayer
            poolId' <- ExceptT $ addRetiredPool poolId blockNo

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let retiredPools = imcRetiredPools inMemoryCache

                --let filteredRetiredPools = filter ((/= poolId') . fst) retiredPools
                    newRetiredPools = (poolId, blockNo) : retiredPools

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcRetiredPools = newRetiredPools }

                in  (newInMemoryCache, ())

            return poolId'

        -- Cache hit
        , dlCheckRetiredPool    = \poolId -> do
            inMemoryCache <- readIORef inMemoryCacheIORef
            let retiredPools = imcRetiredPools inMemoryCache
            let foundRetiredPools = filter ((== poolId) . fst) retiredPools
            case foundRetiredPools of
                []               -> return $ Left RecordDoesNotExist
                (retiredPool':_) -> return $ Right retiredPool'

        , dlGetRetiredPools     = do
            inMemoryCache <- readIORef inMemoryCacheIORef
            let retiredPools = imcRetiredPools inMemoryCache
            -- Just get @PoolIdentifier@
            return $ Right retiredPools

        , dlRemoveRetiredPool   = \poolId -> runExceptT $ do
            -- Modify database
            let removeRetiredPool = dlRemoveRetiredPool dbDataLayer
            poolId' <- ExceptT $ removeRetiredPool poolId

            -- Modify in-memory cache (thread-safe), if the DB operation is a success.
            _ <- liftIO $ atomicModifyIORef' inMemoryCacheIORef $ \inMemoryCache ->

                let retiredPools = imcRetiredPools inMemoryCache

                    newRetiredPools = filter ((/= poolId') . fst) retiredPools

                    newInMemoryCache :: InMemoryCache
                    newInMemoryCache =
                        inMemoryCache
                            { imcRetiredPools = newRetiredPools }

                in  (newInMemoryCache, ())

            return poolId'

        , dlGetAdminUsers       = dlGetAdminUsers dbDataLayer
        , dlAddAdminUser        = dlAddAdminUser dbDataLayer
        , dlRemoveAdminUser     = dlRemoveAdminUser dbDataLayer

        , dlAddFetchError       = dlAddFetchError dbDataLayer
        , dlGetFetchErrors      = dlGetFetchErrors dbDataLayer

        , dlGetPool             = dlGetPool dbDataLayer
        , dlAddPool             = dlAddPool dbDataLayer

        }

-- Init the data layer with the in-memory cache.
createCachedDataLayer :: SqlBackend -> Trace IO Text -> IO DataLayer
createCachedDataLayer sqlBackend tracer = do

    let dbDataLayer = postgresqlDataLayer sqlBackend tracer

    let getDelistedPools = dlGetDelistedPools dbDataLayer
    delistedPools <- getDelistedPools

    let getRetiredPools = dlGetRetiredPools dbDataLayer
    retiredPoolsE <- getRetiredPools

    let retiredPools =
            case retiredPoolsE of
                Left _err -> panic "Cannot fetch retired pools. Cannot populate cache!"
                Right retiredPools' -> retiredPools'

    let getReservedTickers = dlGetReservedTickers dbDataLayer
    reservedTickers <- getReservedTickers

    let getAllPoolMetadata = dlGetAllPoolMetadata dbDataLayer
    allPoolMetadata <- getAllPoolMetadata

    -- Just re-order the structure.
    let tupleMapMeta =
            map (\(PoolMetadata poolId tickerName hash metadata _pmrId) ->
                ((poolId, hash), (tickerName, metadata))) allPoolMetadata

    let allMetadata = Map.fromList tupleMapMeta

    -- The initial cache we need.
    let inMemoryCache =
            InMemoryCache
                { imcDelistedPools = delistedPools
                , imcRetiredPools = retiredPools
                , imcReservedTickers = reservedTickers
                , imcMetadata = allMetadata
                }

    inMemoryCacheIORef <- InMemoryCacheIORef <$> newIORef inMemoryCache

    let dataLayer :: DataLayer
        dataLayer = cachedDataLayer dbDataLayer inMemoryCacheIORef

    return dataLayer

-- TODO(KS): Passing the optional tracer.
postgresqlDataLayer :: SqlBackend -> Trace IO Text -> DataLayer
postgresqlDataLayer sqlBackend tracer = DataLayer
    { dlGetPoolMetadata = \poolId poolMetadataHash' -> do
        poolMetadata <- runDbIohkLogging sqlBackend tracer $ queryPoolMetadata poolId poolMetadataHash'
        let poolTickerName = poolMetadataTickerName <$> poolMetadata
        let poolMetadata' = poolMetadataMetadata <$> poolMetadata
        return $ (,) <$> poolTickerName <*> poolMetadata'
    , dlGetAllPoolMetadata = runDbIohkLogging sqlBackend tracer queryAllPoolMetadata
    , dlAddPoolMetadata     = \mRefId poolId poolHash poolMetadata poolTicker -> do
        let poolTickerName = TickerName $ getPoolTicker poolTicker
        poolMetadataId <- runDbIohkLogging sqlBackend tracer $ insertPoolMetadata $ PoolMetadata poolId poolTickerName poolHash poolMetadata mRefId

        case poolMetadataId of
            Left err  -> return $ Left err
            Right _id -> return $ Right poolMetadata

    , dlAddMetaDataReference = \poolId poolUrl poolMetadataHash' ->
        runDbIohkLogging sqlBackend tracer $ insertPoolMetadataRef $
            PoolMetadataRef
                { poolMetadataRefUrl = poolUrl
                , poolMetadataRefHash = poolMetadataHash'
                , poolMetadataRefPoolId = poolId
                }

    , dlGetReservedTickers = do
        reservedTickers <- runDbIohkLogging sqlBackend tracer queryAllReservedTickers
        return $ map (\reservedTicker -> (reservedTickerName reservedTicker, reservedTickerPoolHash reservedTicker)) reservedTickers

    , dlAddReservedTicker = \tickerName poolMetadataHash' -> do
        reservedTickerId <- runDbIohkLogging sqlBackend tracer $ insertReservedTicker $ ReservedTicker tickerName poolMetadataHash'

        case reservedTickerId of
            Left err  -> return $ Left err
            Right _id -> return $ Right tickerName
    , dlCheckReservedTicker = \tickerName poolMetadataHash' -> do
        mReservedTicker <- runDbIohkLogging sqlBackend tracer $ queryReservedTicker tickerName poolMetadataHash'

        case mReservedTicker of
            Nothing              -> return Nothing
            Just _reservedTicker -> return $ Just tickerName

    , dlGetDelistedPools = do
        delistedPoolsDB <- runDbIohkLogging sqlBackend tracer queryAllDelistedPools
        -- Convert from DB-specific type to the "general" type
        return $ map (PoolIdentifier . getPoolIdentifier . delistedPoolPoolId) delistedPoolsDB
    , dlCheckDelistedPool = \poolId -> do
        runDbIohkLogging sqlBackend tracer $ queryDelistedPool poolId
    , dlAddDelistedPool  = \poolId -> do
        delistedPoolIdentifier <- runDbIohkLogging sqlBackend tracer $ insertDelistedPool $ DelistedPool poolId

        case delistedPoolIdentifier of
            Left err  -> return $ Left err
            Right _id -> return $ Right poolId
    , dlRemoveDelistedPool = \poolId -> do
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteDelistedPool poolId
        -- Up for a discussion, but this might be more sensible in the lower DB layer.
        if isDeleted
            then return $ Right poolId
            else return $ Left RecordDoesNotExist

    , dlAddRetiredPool  = \poolId blockNo -> do
        retiredPoolId <- runDbIohkLogging sqlBackend tracer $ insertRetiredPool $ RetiredPool poolId blockNo

        case retiredPoolId of
            Left err  -> return $ Left err
            Right _id -> return $ Right poolId
    , dlCheckRetiredPool = \poolId -> do
        retiredPool <- runDbIohkLogging sqlBackend tracer $ queryRetiredPool poolId
        case retiredPool of
            Left err -> return $ Left err
            Right retiredPool' -> return $ Right (retiredPoolPoolId retiredPool', retiredPoolBlockNo retiredPool')
    , dlGetRetiredPools = do
        retiredPools <- runDbIohkLogging sqlBackend tracer queryAllRetiredPools
        return $ Right $ map (\retiredPool' -> (retiredPoolPoolId retiredPool', retiredPoolBlockNo retiredPool')) retiredPools
    , dlRemoveRetiredPool = \poolId -> do
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteRetiredPool poolId
        if isDeleted
            then return $ Right poolId
            else return $ Left $ UnknownError "Retired pool not deleted!"

    , dlGetAdminUsers       = do
        adminUsers <- runDbIohkLogging sqlBackend tracer queryAdminUsers
        return $ Right adminUsers
    , dlAddAdminUser        = \(ApplicationUser user pass') -> do
        let adminUser = AdminUser user pass'
        adminUserId <- runDbIohkLogging sqlBackend tracer $ insertAdminUser adminUser
        case adminUserId of
            Left err  -> return $ Left err
            Right _id -> return $ Right adminUser
    , dlRemoveAdminUser     = \(ApplicationUser user pass') -> do
        let adminUser = AdminUser user pass'
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteAdminUser adminUser
        if isDeleted
            then return $ Right adminUser
            else return $ Left $ UnknownError "Admin user not deleted. Both username and password must match."

    , dlAddFetchError       = runDbIohkLogging sqlBackend tracer . insertPoolMetadataFetchError
    , dlGetFetchErrors      = \poolId mTimeFrom -> do
        poolMetadataFetchErrors <- runDbIohkLogging sqlBackend tracer (queryPoolMetadataFetchErrorByTime poolId mTimeFrom)
        pure $ sequence $ Right <$> map convertPoolMetadataFetchError poolMetadataFetchErrors

    , dlGetPool             = \poolId -> do
        pool <- runDbIohkLogging sqlBackend tracer $ queryPoolByPoolId poolId
        case pool of
            Left err   -> return $ Left err
            Right _val -> return $ Right poolId
    , dlAddPool             = \poolId -> do
        poolId' <- runDbIohkLogging sqlBackend tracer $ insertPool (Pool poolId)
        case poolId' of
            Left err   -> return $ Left err
            Right _val -> return $ Right poolId

    }

convertPoolMetadataFetchError :: PoolMetadataFetchError -> PoolFetchError
convertPoolMetadataFetchError (PoolMetadataFetchError timeUTC poolId poolHash _pMRId fetchError retryCount) =
    PoolFetchError (utcTimeToPOSIXSeconds timeUTC) poolId poolHash fetchError retryCount

