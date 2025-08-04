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

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (printf)

-- | Generic progress tracking data type
data Progress = Progress
  { pCurrentStep :: !Int
  , pTotalSteps :: !Int
  , pCurrentPhase :: !Text
  }
  deriving (Show)

type ProgressRef = IORef Progress

-- | Initialize a new progress tracker
initProgress :: MonadIO m => Int -> Text -> m ProgressRef
initProgress totalSteps initialPhase = liftIO $ do
  newIORef $ Progress 0 totalSteps initialPhase

-- | Update progress with new step and phase
updateProgress :: MonadIO m => Maybe (Trace IO Text) -> ProgressRef -> Int -> Text -> m ()
updateProgress mTrace progressRef step phase = liftIO $ do
  modifyIORef' progressRef $ \p ->
    p
      { pCurrentStep = step
      , pCurrentPhase = phase
      }
  case mTrace of
    Nothing -> pure () -- Don't log anything
    Just trce -> renderProgressBar trce =<< readIORef progressRef

-- | Render the progress bar to stdout
renderProgressBar :: Trace IO Text -> Progress -> IO ()
renderProgressBar trce progress = do
  let percentage :: Double
      percentage =
        if pTotalSteps progress == 0
          then 0
          else fromIntegral (pCurrentStep progress) / fromIntegral (pTotalSteps progress) * 100

  let progressMsg =
        Text.pack $
          Text.unpack (pCurrentPhase progress)
            ++ " "
            ++ show (pCurrentStep progress)
            ++ "/"
            ++ show (pTotalSteps progress)
            ++ " ("
            ++ printf "%.1f%%" percentage
            ++ ")"

  logInfo trce progressMsg

-- | Run an action with progress tracking, cleaning up the display afterward
withProgress :: MonadIO m => Maybe (Trace IO Text) -> Int -> Text -> (ProgressRef -> m a) -> m a
withProgress mTrace totalSteps initialPhase action =
  case mTrace of
    Nothing -> do
      -- Create a dummy progress ref but don't log anything
      progressRef <- initProgress totalSteps initialPhase
      action progressRef
    Just trce -> do
      progressRef <- initProgress totalSteps initialPhase
      liftIO $ renderProgressBar trce =<< readIORef progressRef
      result <- action progressRef
      liftIO $ threadDelay 100000 -- Small delay to make progress visible
      pure result
