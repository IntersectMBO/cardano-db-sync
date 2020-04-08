module Cardano.Db.App.Validate.BlockTxs
  ( validateEpochBlockTxs
  ) where

import           Cardano.Db.App.Validate.Util

import           Cardano.Db hiding (queryBlockTxCount)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Either (lefts)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.),
                    countRows, from, just, on, select, unValue, val, where_)

import           Database.Persist.Sql (SqlBackend)

import qualified System.Random as Random


validateEpochBlockTxs :: IO ()
validateEpochBlockTxs = do
  mLatestEpoch <- runDbNoLogging queryLatestCachedEpochNo
  case mLatestEpoch of
    Nothing -> putStrLn "Epoch table is empty"
    Just latest -> validateLatestBlockTxs latest

-- -----------------------------------------------------------------------------

validateLatestBlockTxs :: Word64 -> IO ()
validateLatestBlockTxs latestEpoch = do
  validateBlockTxs latestEpoch
  validateBlockTxs =<< Random.randomRIO (0, latestEpoch - 1)

validateBlockTxs :: Word64 -> IO ()
validateBlockTxs epoch = do
  putStrF $ "All txs for blocks in epoch " ++ show epoch
                ++ " are present: "
  blks <- runDbNoLogging $ queryEpochBlockNumbers epoch
  results <- mapM validateBlockCount blks
  case listToMaybe (lefts results) of
    Nothing ->
      putStrLn $ greenText "ok"
    Just (txCountExpected, txCountActual) ->
      putStrLn $ redText ("Failed: expected tx count of " ++ show txCountExpected
                            ++ " but got " ++ show txCountActual)

validateBlockCount :: (Word64, Word64) -> IO (Either (Word64, Word64) ())
validateBlockCount (blockNo, txCountExpected) = do
  txCountActual <- runDbNoLogging $ queryBlockTxCount blockNo
  pure $ if txCountActual == txCountExpected
          then Right ()
          else Left (txCountExpected, txCountActual)

-- This queries by BlockNo, the one in Cardano.Db.Query queries by BlockId.
queryBlockTxCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryBlockTxCount blockNo = do
  res <- select . from $ \ (blk `InnerJoin` tx) -> do
            on (blk ^. BlockId ==. tx ^. TxBlock)
            where_ (blk ^. BlockBlockNo ==. just (val blockNo))
            pure countRows
  pure $ fromMaybe 0 (unValue <$> listToMaybe res)

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
