module Cardano.DbTool.Validate.BlockTxs
  ( validateEpochBlockTxs
  ) where

import           Cardano.DbTool.Validate.Util

import           Cardano.Db hiding (queryBlockTxCount)

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Either (lefts)
import           Data.Word (Word64)

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), countRows, from, just, on,
                   select, unValue, val, where_, (==.), (^.))

import           Database.Persist.Sql (SqlBackend)

import qualified System.Random as Random


validateEpochBlockTxs :: IO ()
validateEpochBlockTxs = do
  mLatestEpoch <- runDbNoLoggingEnv queryLatestCachedEpochNo
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
  blks <- runDbNoLoggingEnv $ queryEpochBlockNumbers epoch
  results <- runDbNoLoggingEnv $ mapM validateBlockCount blks
  case lefts results of
    [] -> putStrLn $ greenText "ok"
    xs -> do
      when (length xs > 1) $ putStrLn ""
      forM_ xs $ \ ve ->
        putStrLn $ redText ("Failed on block no " ++ show (veBlockNo ve)
                            ++ ": expected tx count of " ++ show (veTxCountExpected ve)
                            ++ " but got " ++ show (veTxCountActual ve)
                            )

validateBlockCount :: MonadIO m => (Word64, Word64) -> ReaderT SqlBackend m (Either ValidateError ())
validateBlockCount (blockNo, txCountExpected) = do
  txCountActual <- queryBlockTxCount blockNo
  pure $ if txCountActual == txCountExpected
          then Right ()
          else Left $ ValidateError blockNo txCountActual txCountExpected

-- This queries by BlockNo, the one in Cardano.Db.Query queries by BlockId.
queryBlockTxCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryBlockTxCount blockNo = do
  res <- select . from $ \ (blk `InnerJoin` tx) -> do
            on (blk ^. BlockId ==. tx ^. TxBlockId)
            where_ (blk ^. BlockBlockNo ==. just (val blockNo))
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryEpochBlockNumbers :: MonadIO m => Word64 -> ReaderT SqlBackend m [(Word64, Word64)]
queryEpochBlockNumbers epoch = do
    res <- select . from $ \ blk -> do
              where_ (blk ^. BlockEpochNo ==. just (val epoch))
              pure (blk ^. BlockBlockNo, blk ^. BlockTxCount)
    pure $ map convert res
  where
    convert :: (Value (Maybe Word64), Value Word64) -> (Word64, Word64)
    convert (Value ma, Value b) =
      case ma of
        Nothing -> (0, b) -- The block does not have transactions.
        Just a -> (a, b)
