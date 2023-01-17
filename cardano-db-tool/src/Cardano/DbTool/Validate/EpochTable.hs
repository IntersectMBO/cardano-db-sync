{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Validate.EpochTable (
  validateEpochTable,
) where

import Cardano.Db
import Cardano.DbTool.Validate.Util
import Control.Monad (when)
import Data.Word (Word64)

-- | Validate that the total supply is decreasing.
-- This is only true for the Byron error where transaction fees are burnt.
validateEpochTable :: IO ()
validateEpochTable =
  maybe (putStrLn "Epoch table is empty") validate =<< getStableEpochCount

validate :: Word64 -> IO ()
validate lastEpoch = do
  putStrF $
    "Epoch table entries for epochs [0.."
      ++ show lastEpoch
      ++ "] are correct: "
  recurse 0
  where
    recurse :: Word64 -> IO ()
    recurse current
      | current > lastEpoch = putStrLn $ greenText "ok"
      | otherwise = do
          -- Recalculate the epoch entry
          recalc <- Right <$> runDbNoLoggingEnv (queryCalcEpochEntry current)
          -- Get the table entry
          value <- runDbNoLoggingEnv $ queryEpochEntry current

          when (recalc /= value)
            . error
            $ redText (show recalc ++ " /= " ++ show value)
          recurse (current + 1)

-- -----------------------------------------------------------------------------

getStableEpochCount :: IO (Maybe Word64)
getStableEpochCount = do
  -- May return Nothing if the EPoch table is empty.
  mLatest <- runDbNoLoggingEnv queryLatestCachedEpochNo
  case mLatest of
    Nothing -> pure Nothing
    Just 0 -> pure Nothing
    Just latest -> pure $ Just (latest - 1)
