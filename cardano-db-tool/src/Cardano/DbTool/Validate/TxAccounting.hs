{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Validate.TxAccounting (
  validateTxAccounting,
) where

import Cardano.Db
import Cardano.DbTool.Validate.Util
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left, runExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (
  Entity (entityVal),
  SqlBackend,
  Value (Value, unValue),
  countRows,
  from,
  innerJoin,
  on,
  select,
  table,
  toSqlKey,
  val,
  where_,
  (==.),
  (>.),
  (^.),
  type (:&) ((:&)),
 )
import qualified System.Random as Random

{- HLINT ignore "Fuse on/on" -}

validateTxAccounting :: IO ()
validateTxAccounting = do
  txIdRange <- runDbNoLoggingEnv queryTestTxIds
  putStrF $
    "For "
      ++ show testCount
      ++ " transactions out of "
      ++ show (snd txIdRange)
      ++ " accounting is: "
  ids <- randomTxIds testCount txIdRange
  res <- runExceptT $ traverse validateAccounting ids
  case res of
    Left err -> error $ redText (reportError err)
    Right _ -> putStrLn $ greenText "ok"
  where
    testCount :: Int
    testCount = 100

-- -----------------------------------------------------------------------------

data ValidateError = ValidateError
  { veTxId :: !Word64
  , veFee :: !Ada
  , veDeposit :: !Int64
  , veWithdrawal :: !Ada
  , inputs :: ![TxOut]
  , outputs :: ![TxOut]
  }

randomTxIds :: Int -> (Word64, Word64) -> IO [Word64]
randomTxIds c (minTxId', maxIxId) =
  List.sort <$> replicateM c (Random.randomRIO (minTxId', maxIxId))

reportError :: ValidateError -> String
reportError ve =
  mconcat
    [ "\nTxId: "
    , show (veTxId ve)
    , "\n  Fee: "
    , show (veFee ve)
    , case compare (veDeposit ve) 0 of
        LT -> "\n  Deposit: " ++ show (veDeposit ve)
        EQ -> mempty
        GT -> "\n  Refund: " ++ show (negate $ veDeposit ve)
    , if veWithdrawal ve == 0
        then mempty
        else "\n  Withdrawal: " ++ show (veWithdrawal ve)
    , "\n  TxIn: ["
    , showTxOuts (inputs ve)
    , "]"
    , "\n  TxOut: ["
    , showTxOuts (outputs ve)
    , "]"
    ]
  where
    showTxOuts :: [TxOut] -> String
    showTxOuts = List.intercalate "," . map showTxOut

    showTxOut :: TxOut -> String
    showTxOut txo =
      mconcat
        [ "TxId "
        , show (unTxId $ txOutTxId txo)
        , " Value "
        , show (word64ToAda . unDbLovelace $ txOutValue txo)
        ]

-- For a given TxId, validate the input/output accounting.
validateAccounting :: Word64 -> ExceptT ValidateError IO ()
validateAccounting txId = do
  (fee, deposit) <- liftIO $ runDbNoLoggingEnv (queryTxFeeDeposit txId)
  withdrawal <- liftIO $ runDbNoLoggingEnv (queryTxWithdrawal txId)
  ins <- liftIO $ runDbNoLoggingEnv (queryTxInputs txId)
  outs <- liftIO $ runDbNoLoggingEnv (queryTxOutputs txId)
  -- A refund is a negative deposit.
  when (deposit >= 0 && sumValues ins + withdrawal /= fee + adaDeposit deposit + sumValues outs) $
    left (ValidateError txId fee deposit withdrawal ins outs)
  when (deposit < 0 && sumValues ins + adaRefund deposit + withdrawal /= fee + sumValues outs) $
    left (ValidateError txId fee deposit withdrawal ins outs)
  where
    sumValues :: [TxOut] -> Ada
    sumValues txs = word64ToAda $ sum (map (unDbLovelace . txOutValue) txs)

    adaDeposit :: Int64 -> Ada
    adaDeposit = word64ToAda . fromIntegral

    adaRefund :: Int64 -> Ada
    adaRefund = word64ToAda . fromIntegral . negate

-- -------------------------------------------------------------------------------------------------

queryTestTxIds :: MonadIO m => ReaderT SqlBackend m (Word64, Word64)
queryTestTxIds = do
  -- Exclude all 'faked' generated TxId values from the genesis block (block_id == 1).
  lower <-
    select $
      from (table @Tx) >>= \tx -> do
        where_ (tx ^. TxBlockId >. val (toSqlKey 1))
        pure (tx ^. TxId)
  upper <- select $ from (table @Tx) >> pure countRows
  pure (maybe 0 (unTxId . unValue) (listToMaybe lower), maybe 0 unValue (listToMaybe upper))

queryTxFeeDeposit :: MonadIO m => Word64 -> ReaderT SqlBackend m (Ada, Int64)
queryTxFeeDeposit txId = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxId ==. val (toSqlKey $ fromIntegral txId))
    pure (tx ^. TxFee, tx ^. TxDeposit)
  pure $ maybe (0, 0) convert (listToMaybe res)
  where
    convert :: (Value DbLovelace, Value (Maybe Int64)) -> (Ada, Int64)
    convert (Value (DbLovelace w64), d) = (word64ToAda w64, fromMaybe 0 (unValue d))

queryTxInputs :: MonadIO m => Word64 -> ReaderT SqlBackend m [TxOut]
queryTxInputs txId = do
  res <- select $ do
    (tx :& txin :& txout) <-
      from
        $ table @Tx
          `innerJoin` table @TxIn
        `on` (\(tx :& txin) -> tx ^. TxId ==. txin ^. TxInTxInId)
          `innerJoin` table @TxOut
        `on` (\(_tx :& txin :& txout) -> txin ^. TxInTxOutId ==. txout ^. TxOutTxId)
    where_ (tx ^. TxId ==. val (toSqlKey $ fromIntegral txId))
    where_ (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
    pure txout
  pure $ entityVal <$> res

queryTxOutputs :: MonadIO m => Word64 -> ReaderT SqlBackend m [TxOut]
queryTxOutputs txId = do
  res <- select $ do
    (tx :& txout) <-
      from
        $ table @Tx
          `innerJoin` table @TxOut
        `on` (\(tx :& txout) -> tx ^. TxId ==. txout ^. TxOutTxId)
    where_ (tx ^. TxId ==. val (toSqlKey $ fromIntegral txId))
    pure txout
  pure $ entityVal <$> res

queryTxWithdrawal :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryTxWithdrawal txId = do
  res <- select $ do
    withdraw <- from $ table @Withdrawal
    where_ (withdraw ^. WithdrawalTxId ==. val (toSqlKey $ fromIntegral txId))
    pure (withdraw ^. WithdrawalAmount)
  -- It is probably not possible to have two withdrawals in a single Tx.
  -- If it is possible then there will be an accounting error.
  pure $ maybe 0 (word64ToAda . unDbLovelace . unValue) (listToMaybe res)
