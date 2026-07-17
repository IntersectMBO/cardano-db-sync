{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Cardano.DbTool.Validate.BlockProperties (
  validateBlockProperties,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Validate.Util
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time
import Data.Word (Word64)
import qualified System.Random as Random

{- HLINT ignore "Reduce duplication" -}

validateBlockProperties :: IO ()
validateBlockProperties = do
  blkCount <- fromIntegral <$> DB.runDbStandaloneSilent DB.queryBlockCount
  validateBlockTimesInPast
  validataBlockNosContiguous blkCount
  validateTimestampsOrdered blkCount

-- -------------------------------------------------------------------------------------------------

validateBlockTimesInPast :: IO ()
validateBlockTimesInPast = do
  putStrF "All block times are in the past: "
  now <- Time.getCurrentTime
  xs <- DB.runDbStandaloneSilent $ DB.queryBlocksTimeAfters now
  if List.null xs
    then putStrLn $ greenText "ok"
    else error $ redText (reportFailures xs)
  where
    reportFailures :: [(Maybe Word64, Maybe Word64, UTCTime)] -> String
    reportFailures xs =
      mconcat
        [ "\nThere are "
        , show (length xs)
        , " blocks with time stamps in the future.\n"
        , "First future block is: "
        , showFirst (head xs)
        ]

    showFirst :: (Maybe Word64, Maybe Word64, UTCTime) -> String
    showFirst (mEpoch, mBlockNo, time) =
      mconcat ["epoch ", show mEpoch, " block ", show mBlockNo, " time ", show time]

-- The random start block and how many blocks to sample, clamped so the range
-- never underflows on a database with fewer than testBlocks blocks.
sampleWindow :: Word64 -> Word64 -> (Word64, Word64)
sampleWindow blkCount testBlocks =
  let count = min testBlocks blkCount
   in (blkCount - count, count)

validataBlockNosContiguous :: Word64 -> IO ()
validataBlockNosContiguous blkCount = do
  let (maxStart, count) = sampleWindow blkCount testBlocks
  startBlock <- Random.randomRIO (0, maxStart)
  putStrF $
    "Block numbers ["
      ++ show startBlock
      ++ " .. "
      ++ show (startBlock + count)
      ++ "] are contiguous: "
  blockNos <- DB.runDbStandaloneSilent $ DB.queryBlockNoList startBlock count
  case checkContinguous blockNos of
    Nothing -> putStrLn $ greenText "ok"
    Just xs -> error $ redText "failed: " ++ show xs
  where
    testBlocks :: Word64
    testBlocks = 100000

    checkContinguous :: [Word64] -> Maybe [Word64]
    checkContinguous xs =
      case xs of
        (a : b : ys) ->
          if a + 1 == b
            then checkContinguous (b : ys)
            else Just [a, b]
        _otherwise -> Nothing

validateTimestampsOrdered :: Word64 -> IO ()
validateTimestampsOrdered blkCount = do
  let (maxStart, count) = sampleWindow blkCount testBlocks
  startBlock <- Random.randomRIO (0, maxStart)
  putStrF $
    "Block time stamps for blocks ["
      ++ show startBlock
      ++ " .. "
      ++ show (startBlock + count)
      ++ "] are ordered: "
  ts <- DB.runDbStandaloneSilent $ DB.queryBlockTimestamps startBlock count
  if List.nubOrd ts == ts
    then putStrLn $ greenText "ok"
    else error $ redText "failed: " ++ show ts
  where
    testBlocks :: Word64
    testBlocks = 100000
