{-# LANGUAGE ExplicitNamespaces #-}

module Cardano.DbTool.Validate.BlockTxs (
  validateEpochBlockTxs,
) where

-- import Cardano.Db hiding (queryBlockTxCount)

import qualified Cardano.Db as DB
import Cardano.DbTool.Validate.Util
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (lefts)
import Data.Word (Word64)
import qualified System.Random as Random

validateEpochBlockTxs :: IO ()
validateEpochBlockTxs = do
  mLatestEpoch <- DB.runDbNoLoggingEnv DB.queryLatestCachedEpochNo
  case mLatestEpoch of
    Nothing -> putStrLn "Epoch table is empty"
    Just latest -> validateLatestBlockTxs latest

-- -----------------------------------------------------------------------------

data ValidateError = ValidateError
  { veBlockNo :: !Word64
  , veTxCountActual :: !Word64
  , veTxCountExpected :: !Word64
  }

validateLatestBlockTxs :: Word64 -> IO ()
validateLatestBlockTxs latestEpoch = do
  validateBlockTxs latestEpoch
  validateBlockTxs =<< Random.randomRIO (0, latestEpoch - 1)

validateBlockTxs :: Word64 -> IO ()
validateBlockTxs epoch = do
  putStrF $ "All transactions for blocks in epoch " ++ show epoch ++ " are present: "
  blks <- DB.runDbNoLoggingEnv $ DB.queryEpochBlockNumbers epoch
  results <- DB.runDbNoLoggingEnv $ mapM validateBlockCount blks
  case lefts results of
    [] -> putStrLn $ greenText "ok"
    xs -> do
      when (length xs > 1) $ putStrLn ""
      forM_ xs $ \ve ->
        putStrLn $
          redText
            ( "Failed on block no "
                ++ show (veBlockNo ve)
                ++ ": expected tx count of "
                ++ show (veTxCountExpected ve)
                ++ " but got "
                ++ show (veTxCountActual ve)
            )

validateBlockCount :: MonadIO m => (Word64, Word64) -> DB.DbAction m (Either ValidateError ())
validateBlockCount (blockNo, txCountExpected) = do
  txCountActual <- DB.queryBlockTxCount $ DB.BlockId $ fromIntegral blockNo
  pure $
    if txCountActual == txCountExpected
      then Right ()
      else Left $ ValidateError blockNo txCountActual txCountExpected
