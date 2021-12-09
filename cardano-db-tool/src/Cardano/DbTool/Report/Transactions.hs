{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbTool.Report.Transactions
  ( reportTransactions
  ) where

import           Cardano.Db
import           Cardano.DbTool.Report.Display

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import qualified Data.List.Extra as List
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (UTCTime)

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), from, just, on, select, val,
                   where_, (&&.), (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Redundant ^." -}

reportTransactions :: [Text] -> IO ()
reportTransactions addrs =
  forM_ addrs $ \ saddr -> do
    Text.putStrLn $ "\nTransactions for: " <> saddr <> "\n"
    xs <- runDbNoLoggingEnv (queryStakeAddressTransactions saddr)
    renderTransactions $ coaleseTxs xs

-- -------------------------------------------------------------------------------------------------
-- This command is designed to emulate the output of the script:
-- https://forum.cardano.org/t/dump-wallet-transactions-with-cardano-cli/40651/6
-- -------------------------------------------------------------------------------------------------

data Direction
  = Outgoing
  | Incoming
  deriving (Eq, Ord, Show)

data Transaction = Transaction
  { trHash :: !Text
  , trTime :: !UTCTime
  , trDirection :: !Direction
  , trAmount :: !Ada
  } deriving Eq

instance Ord Transaction where
  compare tra trb =
    case compare (trTime tra) (trTime trb) of
      LT -> LT
      GT -> GT
      EQ -> compare (trDirection tra) (trDirection trb)

queryStakeAddressTransactions :: MonadIO m => Text -> ReaderT SqlBackend m [Transaction]
queryStakeAddressTransactions address = do
    mSaId <- queryStakeAddressId
    case mSaId of
      Nothing -> pure []
      Just saId -> queryTransactions saId
  where
    queryStakeAddressId :: MonadIO m => ReaderT SqlBackend m (Maybe StakeAddressId)
    queryStakeAddressId = do
      res <- select . from $ \ saddr -> do
                where_ (saddr ^. StakeAddressView ==. val address)
                pure (saddr ^. StakeAddressId)
      pure $ fmap unValue (listToMaybe res)

    queryTransactions :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m [Transaction]
    queryTransactions saId = do
      inputs <- queryInputs saId
      outputs <- queryOutputs saId
      pure $ List.sort (inputs ++ outputs)

queryInputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m [Transaction]
queryInputs saId = do
    -- Standard UTxO inputs.
    res1 <- select . from $ \ (txOut `InnerJoin` tx `InnerJoin` blk) -> do
              on (txOut ^. TxOutTxId ==. tx ^. TxId)
              on (tx ^. TxBlockId ==. blk ^. BlockId)
              where_ (txOut ^. TxOutStakeAddressId ==. just (val saId))
              pure (tx ^. TxHash, blk ^. BlockTime, txOut ^. TxOutValue)

    -- Reward withdrawals.
    res2 <- select . from $ \ (wdrl `InnerJoin` tx `InnerJoin` blk) -> do
              on (tx ^. TxBlockId ==. blk ^. BlockId)
              on (wdrl ^. WithdrawalTxId ==. tx ^. TxId)
              where_ (wdrl ^. WithdrawalAddrId ==. val saId)
              pure (tx ^. TxHash, blk ^. BlockTime, wdrl ^. WithdrawalAmount)

    pure $ groupByTxHash (map (convertTx Incoming) res1 ++ map (convertTx Outgoing) res2)
  where
    groupByTxHash :: [Transaction] -> [Transaction]
    groupByTxHash = mapMaybe coaleseInputs . List.groupOn trHash . List.sortOn trHash

    coaleseInputs :: [Transaction] -> Maybe Transaction
    coaleseInputs xs =
      case xs of
        [] -> Nothing
        (x:_) -> Just $
                  Transaction
                    { trHash = trHash x
                    , trTime = trTime x
                    , trDirection = trDirection x
                    , trAmount = sumAmounts xs
                    }

sumAmounts :: [Transaction] -> Ada
sumAmounts =
    List.foldl' func 0
  where
    func :: Ada -> Transaction -> Ada
    func acc tr =
      case trDirection tr of
        Incoming -> acc + trAmount tr
        Outgoing -> acc - trAmount tr


queryOutputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m [Transaction]
queryOutputs saId = do
    res <- select . from $ \ (txOut `InnerJoin` txInTx `InnerJoin` txIn `InnerJoin` txOutTx `InnerJoin` blk) -> do
              on (txOutTx ^. TxBlockId ==. blk ^. BlockId)
              on (txOutTx ^. TxId ==. txIn ^. TxInTxInId)
              on (txOut ^. TxOutTxId ==. txInTx ^. TxId)
              on (txIn ^. TxInTxOutId ==. txInTx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
              where_ (txOut ^. TxOutStakeAddressId ==. just (val saId))
              pure (txOutTx ^. TxHash, blk ^. BlockTime, txOut ^. TxOutValue)
    pure . groupOutputs $ map (convertTx Outgoing) res
  where
    groupOutputs :: [Transaction] -> [Transaction]
    groupOutputs = mapMaybe coaleseInputs . List.groupOn trHash . List.sortOn trHash

    coaleseInputs :: [Transaction] -> Maybe Transaction
    coaleseInputs xs =
      case xs of
        [] -> Nothing
        (x:_) -> Just $
                  Transaction
                    { trHash = trHash x
                    , trTime = trTime x
                    , trDirection = trDirection x
                    , trAmount = sum $ map trAmount xs
                    }

coaleseTxs :: [Transaction] -> [Transaction]
coaleseTxs =
    mapMaybe coalese . List.groupOn trHash
  where
    coalese :: [Transaction] -> Maybe Transaction
    coalese xs =
      case xs of
        [] -> Nothing
        [a] -> Just a
        [a, b] -> Just $ if trAmount a > trAmount b
                            then Transaction (trHash a) (trTime a) Outgoing (trAmount a - trAmount b)
                            else Transaction (trHash a) (trTime a) Incoming (trAmount b - trAmount a)
        _otherwise -> error $ "coaleseTxs: " ++ show (length xs)

convertTx :: Direction -> (Value ByteString, Value UTCTime, Value DbLovelace) -> Transaction
convertTx dir (Value hash, Value time, Value ll) =
  Transaction
    { trHash = Text.decodeUtf8 (Base16.encode hash)
    , trTime = time
    , trDirection = dir
    , trAmount = word64ToAda (unDbLovelace ll)
    }

renderTransactions :: [Transaction] -> IO ()
renderTransactions xs = do
    putStrLn "                             tx_hash                              |        date/time        | direction |      amount"
    putStrLn "------------------------------------------------------------------+-------------------------+-----------+----------------"
    mapM_ renderTx xs
    putStrLn ""
  where
    renderTx :: Transaction -> IO ()
    renderTx tr =
      Text.putStrLn $ mconcat
        [ " "
        , trHash tr
        , separator
        , textShow (trTime tr)
        , separator
        , " ", textShow (trDirection tr)
        , separator
        , leftPad 14 (renderAda $ trAmount tr)
        ]
