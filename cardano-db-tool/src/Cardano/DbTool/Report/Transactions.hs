{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbTool.Report.Transactions (
  reportTransactions,
) where

import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (textShow)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)

{- HLINT ignore "Redundant ^." -}

reportTransactions :: TxOutVariantType -> [Text] -> IO ()
reportTransactions txOutVariantType addrs =
  forM_ addrs $ \saddr -> do
    Text.putStrLn $ "\nTransactions for: " <> saddr <> "\n"
    xs <- runDbNoLoggingEnv (queryStakeAddressTransactions txOutVariantType saddr)
    renderTransactions $ coaleseTxs xs

-- -------------------------------------------------------------------------------------------------
-- This command is designed to emulate the output of the script:
-- https://forum.cardano.org/t/dump-wallet-transactions-with-cardano-cli/40651/6
-- -------------------------------------------------------------------------------------------------

data Direction = Outgoing | Incoming
  deriving (Eq, Ord, Show)

data Transaction = Transaction
  { trHash :: !Text
  , trTime :: !UTCTime
  , trDirection :: !Direction
  , trAmount :: !Ada
  }
  deriving (Eq)

instance Ord Transaction where
  compare tra trb =
    case compare (trTime tra) (trTime trb) of
      LT -> LT
      GT -> GT
      EQ -> compare (trDirection tra) (trDirection trb)

queryStakeAddressTransactions :: MonadIO m => TxOutVariantType -> Text -> DB.DbAction m [Transaction]
queryStakeAddressTransactions txOutVariantType address = do
  mSaId <- DB.queryStakeAddressId address
  case mSaId of
    Nothing -> pure []
    Just saId -> queryTransactions saId
  where
    queryTransactions :: MonadIO m => DB.StakeAddressId -> DB.DbAction m [Transaction]
    queryTransactions saId = do
      inputs <- queryInputs txOutVariantType saId
      outputs <- queryOutputs txOutVariantType saId
      pure $ List.sort (inputs ++ outputs)

queryInputs :: MonadIO m => TxOutVariantType -> DB.StakeAddressId -> DB.DbAction m [Transaction]
queryInputs txOutVariantType saId = do
  -- Standard UTxO inputs
  res1 <- case txOutVariantType of
    TxOutVariantCore -> DB.queryInputTransactionsCore saId
    TxOutVariantAddress -> DB.queryInputTransactionsAddress saId

  -- Reward withdrawals
  res2 <- DB.queryWithdrawalTransactions saId
  pure $ groupByTxHash (map (convertTx Incoming) res1 ++ map (convertTx Outgoing) res2)
  where
    groupByTxHash :: [Transaction] -> [Transaction]
    groupByTxHash = mapMaybe coaleseInputs . List.groupOn trHash . List.sortOn trHash

    coaleseInputs :: [Transaction] -> Maybe Transaction
    coaleseInputs xs =
      case xs of
        [] -> Nothing
        (x : _) ->
          Just $
            Transaction
              { trHash = trHash x
              , trTime = trTime x
              , trDirection = trDirection x
              , trAmount = sumAmounts xs
              }

queryOutputs :: MonadIO m => TxOutVariantType -> DB.StakeAddressId -> DB.DbAction m [Transaction]
queryOutputs txOutVariantType saId = do
  res <- case txOutVariantType of
    TxOutVariantCore -> DB.queryOutputTransactionsCore saId
    TxOutVariantAddress -> DB.queryOutputTransactionsAddress saId

  pure . groupOutputs $ map (convertTx Outgoing) res
  where
    groupOutputs :: [Transaction] -> [Transaction]
    groupOutputs = mapMaybe coaleseInputs . List.groupOn trHash . List.sortOn trHash

    coaleseInputs :: [Transaction] -> Maybe Transaction
    coaleseInputs xs =
      case xs of
        [] -> Nothing
        (x : _) ->
          Just $
            Transaction
              { trHash = trHash x
              , trTime = trTime x
              , trDirection = trDirection x
              , trAmount = sum $ map trAmount xs
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

coaleseTxs :: [Transaction] -> [Transaction]
coaleseTxs =
  mapMaybe coalese . List.groupOn trHash
  where
    coalese :: [Transaction] -> Maybe Transaction
    coalese xs =
      case xs of
        [] -> Nothing
        [a] -> Just a
        [a, b] ->
          Just $
            if trAmount a > trAmount b
              then Transaction (trHash a) (trTime a) Outgoing (trAmount a - trAmount b)
              else Transaction (trHash a) (trTime a) Incoming (trAmount b - trAmount a)
        _otherwise -> error $ "coaleseTxs: " ++ show (length xs)

convertTx :: Direction -> (ByteString, UTCTime, DbLovelace) -> Transaction
convertTx dir (hash, time, ll) =
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
      Text.putStrLn $
        mconcat
          [ " "
          , trHash tr
          , separator
          , textShow (trTime tr)
          , separator
          , " "
          , textShow (trDirection tr)
          , separator
          , leftPad 14 (renderAda $ trAmount tr)
          ]
