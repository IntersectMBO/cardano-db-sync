{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.DbSync.OffChain.Query (
  getOffChainVoteData,
  getOffChainPoolData,
) where

import Cardano.Db (
  AnchorType (..),
  PoolMetaHash (PoolMetaHash),
  PoolUrl,
  VoteMetaHash (..),
  VoteUrl,
 )
import qualified Cardano.Db as DB
import Cardano.DbSync.OffChain.FetchQueue (newRetry, retryAgain)
import Cardano.DbSync.Types (OffChainPoolWorkQueue (..), OffChainVoteWorkQueue (..))
import Cardano.Prelude hiding (from, groupBy, on, retry)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import System.Random.Shuffle (shuffleM)

---------------------------------------------------------------------------------------------------------------------------------
-- Query OffChain VoteData
---------------------------------------------------------------------------------------------------------------------------------
getOffChainVoteData :: MonadIO m => POSIXTime -> Int -> DB.DbAction m [OffChainVoteWorkQueue]
getOffChainVoteData now maxCount = do
  xs <- queryNewVoteWorkQueue now maxCount
  if length xs >= maxCount
    then take maxCount <$> liftIO (shuffleM xs)
    else do
      ys <- queryOffChainVoteWorkQueue (Time.posixSecondsToUTCTime now) maxCount
      take maxCount . (xs ++) <$> liftIO (shuffleM ys)

-- get all the voting anchors that don't already exist in OffChainVoteData or OffChainVoteFetchError
queryNewVoteWorkQueue :: MonadIO m => POSIXTime -> Int -> DB.DbAction m [OffChainVoteWorkQueue]
queryNewVoteWorkQueue now maxCount = do
  results <- DB.queryNewVoteWorkQueueData maxCount
  pure $ map (makeOffChainVoteWorkQueue now) results

makeOffChainVoteWorkQueue ::
  POSIXTime ->
  (DB.VotingAnchorId, ByteString, VoteUrl, AnchorType) ->
  OffChainVoteWorkQueue
makeOffChainVoteWorkQueue now (vaId, vaHash, url, tp) =
  OffChainVoteWorkQueue
    { oVoteWqMetaHash = VoteMetaHash vaHash
    , oVoteWqReferenceId = vaId
    , oVoteWqType = tp
    , oVoteWqRetry = newRetry now
    , oVoteWqUrl = url
    }

queryOffChainVoteWorkQueue :: MonadIO m => UTCTime -> Int -> DB.DbAction m [OffChainVoteWorkQueue]
queryOffChainVoteWorkQueue _now maxCount = do
  results <- DB.queryOffChainVoteWorkQueueData maxCount
  pure $ map convertToWorkQueue results

convertToWorkQueue :: (UTCTime, DB.VotingAnchorId, ByteString, VoteUrl, AnchorType, Word) -> OffChainVoteWorkQueue
convertToWorkQueue (time, vaId, vaHash, url, tp, rCount) =
  OffChainVoteWorkQueue
    { oVoteWqMetaHash = VoteMetaHash vaHash
    , oVoteWqReferenceId = vaId
    , oVoteWqType = tp
    , oVoteWqRetry = retryAgain (Time.utcTimeToPOSIXSeconds time) rCount
    , oVoteWqUrl = url
    }

---------------------------------------------------------------------------------------------------------------------------------
-- Query OffChain PoolData
---------------------------------------------------------------------------------------------------------------------------------
getOffChainPoolData :: MonadIO m => POSIXTime -> Int -> DB.DbAction m [OffChainPoolWorkQueue]
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
queryNewPoolWorkQueue :: MonadIO m => POSIXTime -> Int -> DB.DbAction m [OffChainPoolWorkQueue]
queryNewPoolWorkQueue now maxCount = do
  results <- DB.queryNewPoolWorkQueueData maxCount
  pure $ map (makeOffChainPoolWorkQueue now) results

makeOffChainPoolWorkQueue :: POSIXTime -> (DB.PoolHashId, DB.PoolMetadataRefId, PoolUrl, ByteString) -> OffChainPoolWorkQueue
makeOffChainPoolWorkQueue now (phId, pmrId, url, pmh) =
  OffChainPoolWorkQueue
    { oPoolWqHashId = phId
    , oPoolWqReferenceId = pmrId
    , oPoolWqUrl = url
    , oPoolWqMetaHash = PoolMetaHash pmh
    , oPoolWqRetry = newRetry now
    }

queryOffChainPoolWorkQueue :: MonadIO m => UTCTime -> Int -> DB.DbAction m [OffChainPoolWorkQueue]
queryOffChainPoolWorkQueue _now maxCount = do
  results <- DB.queryOffChainPoolWorkQueueData maxCount
  pure $ map convertToOffChainPoolWorkQueue results

convertToOffChainPoolWorkQueue :: (UTCTime, DB.PoolMetadataRefId, PoolUrl, ByteString, DB.PoolHashId, Word) -> OffChainPoolWorkQueue
convertToOffChainPoolWorkQueue (time, pmrId, url, pmh, phId, rCount) =
  OffChainPoolWorkQueue
    { oPoolWqHashId = phId
    , oPoolWqReferenceId = pmrId
    , oPoolWqUrl = url
    , oPoolWqMetaHash = PoolMetaHash pmh
    , oPoolWqRetry = retryAgain (Time.utcTimeToPOSIXSeconds time) rCount
    }
