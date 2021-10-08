{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Offline.FetchQueue
  ( newRetry
  , retryAgain
  , showRetryTimes
  ) where


import           Cardano.Prelude hiding (retry)
import           Cardano.DbSync.Types

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
    -- POSIXTime is in seconds.
    Retry
      { retryFetchTime = fetchTime
      , retryRetryTime = fetchTime + min (24 * 60 * 60) (30 + (5 ^ nextRetryCount) * 60)
      , retryCount = nextRetryCount
      }
  where
    nextRetryCount :: Word
    nextRetryCount = 1 + existingRetryCount

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
