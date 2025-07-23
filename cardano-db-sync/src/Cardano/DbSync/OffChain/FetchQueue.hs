{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.FetchQueue (
  newRetry,
  retryAgain,
) where

import Cardano.DbSync.Types
import Cardano.Prelude hiding (retry)
import Data.Time.Clock.POSIX (POSIXTime)

newRetry :: POSIXTime -> Retry
newRetry now =
  Retry
    { retryFetchTime = now
    , retryRetryTime = now -- For a new one, retry time is now.
    , retryCount = 0
    }

retryAgain :: POSIXTime -> Word -> Retry
retryAgain fetchTime existingRetryCount =
  -- When to retry. Maximum of a day for a retry.
  -- POSIXTime is in seconds.
  Retry
    { retryFetchTime = fetchTime
    , retryRetryTime = fetchTime + retryDiff
    , retryCount = nextRetryCount
    }
  where
    nextRetryCount :: Word
    nextRetryCount = 1 + existingRetryCount

    retryDiff =
      if nextRetryCount >= 5
        then 24 * 60 * 60
        else min (24 * 60 * 60) (30 + (2 ^ nextRetryCount) * 60)
