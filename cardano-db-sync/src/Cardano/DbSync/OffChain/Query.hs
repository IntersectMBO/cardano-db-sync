{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.DbSync.OffChain.Query (
  getOffChainVoteData,
  getOffChainPoolData,
) where

import Cardano.Db (
  AnchorType (..),
  EntityField (..),
  OffChainPoolData,
  OffChainPoolFetchError,
  OffChainPoolFetchErrorId,
  OffChainVoteData,
  OffChainVoteFetchError,
  OffChainVoteFetchErrorId,
  PoolHash,
  PoolHashId,
  PoolMetaHash (PoolMetaHash),
  PoolMetadataRef,
  PoolMetadataRefId,
  PoolUrl,
  VoteMetaHash (..),
  VoteUrl,
  VotingAnchor,
  VotingAnchorId,
 )
import Cardano.DbSync.OffChain.FetchQueue (newRetry, retryAgain)
import Cardano.DbSync.Types (OffChainPoolWorkQueue (..), OffChainVoteWorkQueue (..))
import Cardano.Prelude hiding (from, groupBy, on, retry)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import Database.Esqueleto.Experimental (
  SqlBackend,
  SqlExpr,
  Value (..),
  ValueList,
  asc,
  from,
  groupBy,
  in_,
  innerJoin,
  just,
  limit,
  max_,
  notExists,
  on,
  orderBy,
  select,
  subList_select,
  table,
  val,
  where_,
  (!=.),
  (:&) ((:&)),
  (==.),
  (^.),
 )
import System.Random.Shuffle (shuffleM)

{- HLINT ignore "Fuse on/on" -}

---------------------------------------------------------------------------------------------------------------------------------
-- Query OffChain VoteData
---------------------------------------------------------------------------------------------------------------------------------
getOffChainVoteData :: MonadIO m => POSIXTime -> Int -> ReaderT SqlBackend m [OffChainVoteWorkQueue]
getOffChainVoteData now maxCount = do
  xs <- queryNewVoteWorkQueue now maxCount
  if length xs >= maxCount
    then take maxCount <$> liftIO (shuffleM xs)
    else do
      ys <- queryOffChainVoteWorkQueue (Time.posixSecondsToUTCTime now) maxCount
      take maxCount . (xs ++) <$> liftIO (shuffleM ys)

-- get all the voting anchors that don't already exist in OffChainVoteData or OffChainVoteFetchError
queryNewVoteWorkQueue :: MonadIO m => POSIXTime -> Int -> ReaderT SqlBackend m [OffChainVoteWorkQueue]
queryNewVoteWorkQueue now maxCount = do
  res <- select $ do
    va <- from $ table @VotingAnchor
    where_
      ( notExists $
          from (table @OffChainVoteData) >>= \ocvd ->
            where_ (ocvd ^. OffChainVoteDataVotingAnchorId ==. va ^. VotingAnchorId)
      )
    where_ (va ^. VotingAnchorType !=. val ConstitutionAnchor)
    where_
      ( notExists $
          from (table @OffChainVoteFetchError) >>= \ocvfe ->
            where_ (ocvfe ^. OffChainVoteFetchErrorVotingAnchorId ==. va ^. VotingAnchorId)
      )
    limit $ fromIntegral maxCount
    pure
      ( va ^. VotingAnchorId
      , va ^. VotingAnchorDataHash
      , va ^. VotingAnchorUrl
      , va ^. VotingAnchorType
      )
  pure $ map convert res
  where
    convert :: (Value VotingAnchorId, Value ByteString, Value VoteUrl, Value AnchorType) -> OffChainVoteWorkQueue
    convert (Value vaId, Value vaHash, Value url, Value tp) =
      OffChainVoteWorkQueue
        { oVoteWqMetaHash = VoteMetaHash vaHash
        , oVoteWqReferenceId = vaId
        , oVoteWqType = tp
        , oVoteWqRetry = newRetry now
        , oVoteWqUrl = url
        }

queryOffChainVoteWorkQueue :: MonadIO m => UTCTime -> Int -> ReaderT SqlBackend m [OffChainVoteWorkQueue]
queryOffChainVoteWorkQueue _now maxCount = do
  res <- select $ do
    (va :& ocpfe) <-
      from
        $ table @VotingAnchor
          `innerJoin` table @OffChainVoteFetchError
        `on` (\(va :& ocpfe) -> ocpfe ^. OffChainVoteFetchErrorVotingAnchorId ==. va ^. VotingAnchorId)
    orderBy [asc (ocpfe ^. OffChainVoteFetchErrorId)]
    where_ (just (ocpfe ^. OffChainVoteFetchErrorId) `in_` latestRefs)
    where_ (va ^. VotingAnchorType !=. val ConstitutionAnchor)
    limit $ fromIntegral maxCount
    pure
      ( ocpfe ^. OffChainVoteFetchErrorFetchTime
      , va ^. VotingAnchorId
      , va ^. VotingAnchorDataHash
      , va ^. VotingAnchorUrl
      , va ^. VotingAnchorType
      , ocpfe ^. OffChainVoteFetchErrorRetryCount
      )
  pure $ map convert res
  where
    convert :: (Value UTCTime, Value VotingAnchorId, Value ByteString, Value VoteUrl, Value AnchorType, Value Word) -> OffChainVoteWorkQueue
    convert (Value time, Value vaId, Value vaHash, Value url, Value tp, Value rCount) =
      OffChainVoteWorkQueue
        { oVoteWqMetaHash = VoteMetaHash vaHash
        , oVoteWqReferenceId = vaId
        , oVoteWqType = tp
        , oVoteWqRetry = retryAgain (Time.utcTimeToPOSIXSeconds time) rCount
        , oVoteWqUrl = url
        }

    latestRefs :: SqlExpr (ValueList (Maybe OffChainVoteFetchErrorId))
    latestRefs =
      subList_select $ do
        ocvfe <- from (table @OffChainVoteFetchError)
        groupBy (ocvfe ^. OffChainVoteFetchErrorVotingAnchorId)
        where_
          ( notExists $
              from (table @OffChainVoteData) >>= \ocvd ->
                where_ (ocvd ^. OffChainVoteDataVotingAnchorId ==. ocvfe ^. OffChainVoteFetchErrorVotingAnchorId)
          )
        pure $ max_ (ocvfe ^. OffChainVoteFetchErrorId)

---------------------------------------------------------------------------------------------------------------------------------
-- Query OffChain PoolData
---------------------------------------------------------------------------------------------------------------------------------
getOffChainPoolData :: MonadIO m => POSIXTime -> Int -> ReaderT SqlBackend m [OffChainPoolWorkQueue]
getOffChainPoolData now maxCount = do
  -- Results from the query are shuffles so we don't continuously get the same entries.
  xs <- queryNewPoolWorkQueue now maxCount
  if length xs >= maxCount
    then take maxCount <$> liftIO (shuffleM xs)
    else do
      ys <- queryOffChainPoolWorkQueue (Time.posixSecondsToUTCTime now) maxCount
      take maxCount . (xs ++) <$> liftIO (shuffleM ys)

-- Get pool work queue data for new pools (ie pools that had OffChainPoolData entry and no
-- OffChainPoolFetchError).
queryNewPoolWorkQueue :: MonadIO m => POSIXTime -> Int -> ReaderT SqlBackend m [OffChainPoolWorkQueue]
queryNewPoolWorkQueue now maxCount = do
  res <- select $ do
    (ph :& pmr) <-
      from
        $ table @PoolHash
          `innerJoin` table @PoolMetadataRef
        `on` (\(ph :& pmr) -> ph ^. PoolHashId ==. pmr ^. PoolMetadataRefPoolId)
    where_ (just (pmr ^. PoolMetadataRefId) `in_` latestRefs)
    where_
      ( notExists $
          from (table @OffChainPoolData) >>= \pod ->
            where_ (pod ^. OffChainPoolDataPmrId ==. pmr ^. PoolMetadataRefId)
      )
    where_
      ( notExists $
          from (table @OffChainPoolFetchError) >>= \pofe ->
            where_ (pofe ^. OffChainPoolFetchErrorPmrId ==. pmr ^. PoolMetadataRefId)
      )
    limit $ fromIntegral maxCount
    pure
      ( ph ^. PoolHashId
      , pmr ^. PoolMetadataRefId
      , pmr ^. PoolMetadataRefUrl
      , pmr ^. PoolMetadataRefHash
      )
  pure $ map convert res
  where
    -- This assumes that the autogenerated `id` field is a reliable proxy for time, ie, higher
    -- `id` was added later. This is a valid assumption because the primary keys are
    -- monotonically increasing and never reused.
    latestRefs :: SqlExpr (ValueList (Maybe PoolMetadataRefId))
    latestRefs =
      subList_select $ do
        pmr <- from $ table @PoolMetadataRef
        groupBy (pmr ^. PoolMetadataRefPoolId)
        pure $ max_ (pmr ^. PoolMetadataRefId)

    convert ::
      (Value PoolHashId, Value PoolMetadataRefId, Value PoolUrl, Value ByteString) ->
      OffChainPoolWorkQueue
    convert (Value phId, Value pmrId, Value url, Value pmh) =
      OffChainPoolWorkQueue
        { oPoolWqHashId = phId
        , oPoolWqReferenceId = pmrId
        , oPoolWqUrl = url
        , oPoolWqMetaHash = PoolMetaHash pmh
        , oPoolWqRetry = newRetry now
        }

-- Get pool fetch data for pools that have previously errored.
queryOffChainPoolWorkQueue :: MonadIO m => UTCTime -> Int -> ReaderT SqlBackend m [OffChainPoolWorkQueue]
queryOffChainPoolWorkQueue _now maxCount = do
  res <- select $ do
    (ph :& pmr :& pofe) <-
      from
        $ table @PoolHash
          `innerJoin` table @PoolMetadataRef
        `on` (\(ph :& pmr) -> ph ^. PoolHashId ==. pmr ^. PoolMetadataRefPoolId)
          `innerJoin` table @OffChainPoolFetchError
        `on` (\(_ph :& pmr :& pofe) -> pofe ^. OffChainPoolFetchErrorPmrId ==. pmr ^. PoolMetadataRefId)
    where_ (just (pofe ^. OffChainPoolFetchErrorId) `in_` latestRefs)
    orderBy [asc (pofe ^. OffChainPoolFetchErrorId)]
    limit $ fromIntegral maxCount
    pure
      ( pofe ^. OffChainPoolFetchErrorFetchTime
      , pofe ^. OffChainPoolFetchErrorPmrId
      , pmr ^. PoolMetadataRefUrl
      , pmr ^. PoolMetadataRefHash
      , ph ^. PoolHashId
      , pofe ^. OffChainPoolFetchErrorRetryCount
      )
  pure $ map convert res
  where
    -- This assumes that the autogenerated `id` fiels is a reliable proxy for time, ie, higher
    -- `id` was added later. This is a valid assumption because the primary keys are
    -- monotonically increasing and never reused.
    latestRefs :: SqlExpr (ValueList (Maybe OffChainPoolFetchErrorId))
    latestRefs =
      subList_select $ do
        pofe <- from (table @OffChainPoolFetchError)
        where_ (notExists $ from (table @OffChainPoolData) >>= \pod -> where_ (pod ^. OffChainPoolDataPmrId ==. pofe ^. OffChainPoolFetchErrorPmrId))
        groupBy (pofe ^. OffChainPoolFetchErrorPoolId)
        pure $ max_ (pofe ^. OffChainPoolFetchErrorId)

    convert ::
      (Value UTCTime, Value PoolMetadataRefId, Value PoolUrl, Value ByteString, Value PoolHashId, Value Word) ->
      OffChainPoolWorkQueue
    convert (Value time, Value pmrId, Value url, Value pmh, Value phId, Value rCount) =
      OffChainPoolWorkQueue
        { oPoolWqHashId = phId
        , oPoolWqReferenceId = pmrId
        , oPoolWqUrl = url
        , oPoolWqMetaHash = PoolMetaHash pmh
        , oPoolWqRetry = retryAgain (Time.utcTimeToPOSIXSeconds time) rCount
        }
