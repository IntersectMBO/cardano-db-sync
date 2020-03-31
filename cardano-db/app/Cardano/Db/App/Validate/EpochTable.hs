module Cardano.Db.App.Validate.EpochTable
  ( validateEpochTable
  ) where

import           Cardano.Db.App.Validate.Util

import           Control.Monad (replicateM_)

import           Data.Word (Word64)

import           Cardano.Db

import           System.Random (randomRIO)


-- | Validate that the total supply is decreasing.
-- This is only true for the Byron error where transaction fees are burnt.
validateEpochTable :: Word -> IO ()
validateEpochTable count =
  replicateM_ (fromIntegral count) $ do
    mTestEpoch <- genRandomEpochNo
    case mTestEpoch of
      Nothing -> putStrLn "Epoch table is empty"
      Just testEpoch -> do
        putStrF $ "Epoch table entry for epoch " ++ show testEpoch
                ++ " is correct: "
        -- Recalculate the epoch entry
        recalc <- runDbNoLogging $ queryCalcEpochEntry testEpoch
        -- Get the table entry
        value <- runDbNoLogging $ queryEpochEntry testEpoch

        if recalc == value
          then putStrLn $ greenText "ok"
          else error $ redText (show recalc ++ " /= " ++ show value)

-- -----------------------------------------------------------------------------

genRandomEpochNo :: IO (Maybe Word64)
genRandomEpochNo = do
  -- May return Nothing if the EPoch table is empty.
  latest <- runDbNoLogging queryLatestEpochNo
  if latest == 0
    then pure Nothing
    else Just <$> randomRIO (0, latest - 1)
