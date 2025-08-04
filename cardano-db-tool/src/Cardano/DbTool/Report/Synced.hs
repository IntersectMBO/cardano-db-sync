{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Synced (
  assertFullySynced,
) where

import qualified Cardano.Db as DB
import Control.Monad (when)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Time
import System.Exit (exitFailure)

assertFullySynced :: IO ()
assertFullySynced = do
  latestBlock <- maybe (assertFail Nothing) pure =<< DB.runDbStandaloneSilent DB.queryLatestBlock
  currentTime <- Time.getCurrentTime
  let diff = Time.diffUTCTime currentTime (DB.blockTime latestBlock)
  when (diff > 300.0) $
    assertFail (Just $ renderDifftime diff)

assertFail :: Maybe String -> IO a
assertFail mdiff = do
  case mdiff of
    Nothing -> putStrLn "Error: Database is not fully synced."
    Just diff -> putStrLn $ "Error: Database is not fully synced. Currently " ++ diff ++ " behind the tip."
  exitFailure

renderDifftime :: NominalDiffTime -> String
renderDifftime ndt
  | ndt > 3.0 * 24.0 * 3600.0 = show (ceiling (ndt / (24.0 * 3600.0)) :: Word) ++ " days"
  | ndt > 3.0 * 3600.0 = show (ceiling (ndt / 3600.0) :: Word) ++ " hours"
  | ndt > 3.0 * 60.0 = show (ceiling (ndt / 60.0) :: Word) ++ " minutes"
  | otherwise = show (ceiling ndt :: Word) ++ " seconds"
