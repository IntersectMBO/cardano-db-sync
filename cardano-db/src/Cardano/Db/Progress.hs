{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Progress (
  -- * Types
  Progress (..),
  ProgressRef,

  -- * Progress creation and management
  initProgress,
  updateProgress,

  -- * Rendering
  renderProgressBar,
  withProgress,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Generic progress tracking data type
data Progress = Progress
  { pCurrentStep :: !Int
  , pTotalSteps :: !Int
  , pCurrentPhase :: !Text
  , pStartTime :: !UTCTime
  }
  deriving (Show)

type ProgressRef = IORef Progress

-- | Initialize a new progress tracker
initProgress :: MonadIO m => Int -> Text -> m ProgressRef
initProgress totalSteps initialPhase = liftIO $ do
  startTime <- getCurrentTime
  newIORef $ Progress 0 totalSteps initialPhase startTime

-- | Update progress with new step and phase
updateProgress :: MonadIO m => ProgressRef -> Int -> Text -> m ()
updateProgress progressRef step phase = liftIO $ do
  modifyIORef' progressRef $ \p ->
    p
      { pCurrentStep = step
      , pCurrentPhase = phase
      }
  renderProgressBar =<< readIORef progressRef

-- | Render the progress bar to stdout
renderProgressBar :: Progress -> IO ()
renderProgressBar progress = do
  let percentage :: Double
      percentage =
        if pTotalSteps progress == 0
          then 0
          else fromIntegral (pCurrentStep progress) / fromIntegral (pTotalSteps progress) * 100
      barWidth = 50
      filled = round (fromIntegral barWidth * percentage / 100)
      bar = replicate filled '█' ++ replicate (barWidth - filled) '░'

  -- Calculate elapsed time
  currentTime <- getCurrentTime
  let elapsed = diffUTCTime currentTime (pStartTime progress)
      elapsedStr = formatDuration elapsed

  putStr $
    "\r\ESC[K" -- Clear entire line
      ++ show (pCurrentStep progress)
      ++ "/"
      ++ show (pTotalSteps progress)
      ++ " ["
      ++ bar
      ++ "] "
      ++ printf "%.1f%% - " percentage
      ++ Text.unpack (pCurrentPhase progress)
      ++ " ("
      ++ elapsedStr
      ++ ")"
  hFlush stdout

-- | Format duration as MM:SS or HH:MM:SS
formatDuration :: NominalDiffTime -> String
formatDuration duration
  | totalSeconds < 3600 = printf "%02d:%02d" minutes seconds
  | otherwise = printf "%02d:%02d:%02d" hours minutes seconds
  where
    totalSeconds = round duration :: Int
    hours = totalSeconds `div` 3600
    minutes = (totalSeconds `mod` 3600) `div` 60
    seconds = totalSeconds `mod` 60

-- | Run an action with progress tracking, cleaning up the display afterward
withProgress :: MonadIO m => Int -> Text -> (ProgressRef -> m a) -> m a
withProgress totalSteps initialPhase action = do
  -- liftIO $ putStrLn "" -- Start with a new line
  progressRef <- initProgress totalSteps initialPhase
  liftIO $ renderProgressBar =<< readIORef progressRef
  result <- action progressRef
  liftIO $ threadDelay 100000 -- Small delay to make progress visible
  liftIO $ do
    putStrLn "✅ Operation completed!"
  pure result
