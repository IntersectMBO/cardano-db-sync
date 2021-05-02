{-# LANGUAGE DeriveGeneric #-}

module Cardano.SMASH.FetchQueue
  ( PoolFetchRetry (..)
  , Retry (..)
  , newRetry
  , retryAgain
  , showRetryTimes
  ) where


import           Cardano.Prelude

import           Data.Time.Clock.POSIX (POSIXTime)

import           Cardano.Db

import           Cardano.SMASH.Types (formatTimeToNormal)

data PoolFetchRetry = PoolFetchRetry
  { pfrReferenceId :: !PoolMetadataRefId
  , pfrPoolIdWtf   :: !PoolIdentifier
  , pfrPoolUrl     :: !PoolUrl
  , pfrPoolMDHash  :: !PoolMetaHash
  , pfrRetry       :: !Retry
  } deriving (Show)

data Retry = Retry
  { fetchTime  :: !POSIXTime
  , retryTime  :: !POSIXTime
  , retryCount :: !Word
  } deriving (Eq, Show, Generic)

newRetry :: POSIXTime -> Retry
newRetry now =
  Retry
    { fetchTime = now
    , retryTime = now + 60 -- 60 seconds from now
    , retryCount = 0
    }

retryAgain :: POSIXTime -> Word -> Retry
retryAgain fetchTimePOSIX existingRetryCount =
    -- When to retry. Maximum of a day for a retry.
    -- We are basically using a series to predict the next retry time.
    let calculateNewDiff currRetryCount = min (24 * 60 * 60) ((3 ^ currRetryCount) * 60)
        newRetryDiff = sum $ map calculateNewDiff [0..existingRetryCount]
    in
        Retry
            { fetchTime = fetchTimePOSIX
            , retryTime = fetchTimePOSIX + newRetryDiff
            , retryCount = existingRetryCount
            }

-- A nice pretty printer for the retry.
showRetryTimes :: Retry -> Text
showRetryTimes retry' =
    mconcat
        [ "Fetch time: '"
        , formatTimeToNormal $ fetchTime retry'
        , "', retry time: '"
        , formatTimeToNormal $ retryTime retry'
        , "', retry count: '"
        , show $ retryCount retry'
        , "'."
        ]

