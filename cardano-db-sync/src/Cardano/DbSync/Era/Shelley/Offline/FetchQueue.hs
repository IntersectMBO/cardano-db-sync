{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Offline.FetchQueue
  ( newRetry
  , retryAgain
  , showRetryTimes
  ) where


import           Cardano.Prelude hiding (retry)
import           Cardano.Sync.Types

import qualified Data.Text as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format as Time


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
    -- We are basically using a series to predict the next retry time.
    Retry
      { retryFetchTime = fetchTime
      , retryRetryTime = fetchTime + sum (map calculateNewDiff [0 .. existingRetryCount])
      , retryCount = 1 + existingRetryCount
      }
  where
    calculateNewDiff :: Word -> POSIXTime
    calculateNewDiff currRetryCount = min (24 * 60 * 60) ((3 ^ currRetryCount) * 60)

-- A nice pretty printer for the retry.
showRetryTimes :: Retry -> Text
showRetryTimes retry =
  mconcat
    [ "Fetch time: ", formatTimeToNormal (retryFetchTime retry), ", retry time: "
    , formatTimeToNormal (retryRetryTime retry), ", retry count: ", show $ retryCount retry, "."
    ]

formatTimeToNormal :: Time.POSIXTime -> Text
formatTimeToNormal =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%d.%m.%Y. %T" . Time.posixSecondsToUTCTime
