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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbTool.Report.Transactions (
  reportTransactions,
) where

import Cardano.Db
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbTool.Report.Display
import Cardano.Prelude (textShow)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Database.Esqueleto.Experimental (
  SqlBackend,
  Value (Value, unValue),
  from,
  innerJoin,
  just,
  on,
  select,
  table,
  val,
  where_,
  (&&.),
  (==.),
  (^.),
  type (:&) ((:&)),
 )

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

data Direction
  = Outgoing
  | Incoming
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
  mSaId <- queryStakeAddressId
  case mSaId of
    Nothing -> pure []
    Just saId -> queryTransactions saId
  where
    queryStakeAddressId :: MonadIO m => DB.DbAction m (Maybe StakeAddressId)
    queryStakeAddressId = do
      res <- select $ do
        saddr <- from (table @StakeAddress)
        where_ (saddr ^. StakeAddressView ==. val address)
        pure (saddr ^. StakeAddressId)
      pure $ fmap unValue (listToMaybe res)

    queryTransactions :: MonadIO m => StakeAddressId -> DB.DbAction m [Transaction]
    queryTransactions saId = do
      inputs <- queryInputs txOutVariantType saId
      outputs <- queryOutputs txOutVariantType saId
      pure $ List.sort (inputs ++ outputs)

queryInputs ::
  MonadIO m =>
  TxOutVariantType ->
  StakeAddressId ->
  DB.DbAction m [Transaction]
queryInputs txOutVariantType saId = do
  -- Standard UTxO inputs.
  res1 <- case txOutVariantType of
    -- get the StakeAddressId from the Core TxOut table
    TxOutVariantCore -> select $ do
      (tx :& txOut :& blk) <-
        from $
          table @Tx
            `innerJoin` table @VC.TxOut
              `on` (\(tx :& txOut) -> txOut ^. VC.TxOutTxId ==. tx ^. TxId)
            `innerJoin` table @Block
              `on` (\(tx :& _txOut :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      where_ (txOut ^. VC.TxOutStakeAddressId ==. just (val saId))
      pure (tx ^. TxHash, blk ^. BlockTime, txOut ^. VC.TxOutValue)
    -- get the StakeAddressId from the Variant TxOut table
    TxOutVariantAddress -> select $ do
      (tx :& txOut :& addr :& blk) <-
        from $
          table @Tx
            `innerJoin` table @VA.TxOut
              `on` (\(tx :& txOut) -> txOut ^. VA.TxOutTxId ==. tx ^. TxId)
            `innerJoin` table @VA.Address
              `on` (\(_tx :& txOut :& addr) -> txOut ^. VA.TxOutAddressId ==. addr ^. VA.AddressId)
            `innerJoin` table @Block
              `on` (\(tx :& _txOut :& _addr :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      where_ (addr ^. VA.AddressStakeAddressId ==. just (val saId))
      pure (tx ^. TxHash, blk ^. BlockTime, txOut ^. VA.TxOutValue)
  -- Reward withdrawals.
  res2 <- select $ do
    (tx :& blk :& wdrl) <-
      from $
        table @Tx
          `innerJoin` table @Block
            `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
          `innerJoin` table @Withdrawal
            `on` (\(tx :& _blk :& wdrl) -> wdrl ^. WithdrawalTxId ==. tx ^. TxId)
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
        (x : _) ->
          Just $
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

queryOutputs :: MonadIO m => TxOutVariantType -> StakeAddressId -> DB.DbAction m [Transaction]
queryOutputs txOutVariantType saId = do
  res <- case txOutVariantType of
    TxOutVariantCore -> select $ do
      (txOut :& _txInTx :& _txIn :& txOutTx :& blk) <-
        from $
          table @VC.TxOut
            `innerJoin` table @Tx
              `on` (\(txOut :& txInTx) -> txOut ^. VC.TxOutTxId ==. txInTx ^. TxId)
            `innerJoin` table @TxIn
              `on` (\(txOut :& txInTx :& txIn) -> txIn ^. TxInTxOutId ==. txInTx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. VC.TxOutIndex)
            `innerJoin` table @Tx
              `on` (\(_txOut :& _txInTx :& txIn :& txOutTx) -> txOutTx ^. TxId ==. txIn ^. TxInTxInId)
            `innerJoin` table @Block
              `on` (\(_txOut :& _txInTx :& _txIn :& txOutTx :& blk) -> txOutTx ^. TxBlockId ==. blk ^. BlockId)

      where_ (txOut ^. VC.TxOutStakeAddressId ==. just (val saId))
      pure (txOutTx ^. TxHash, blk ^. BlockTime, txOut ^. VC.TxOutValue)
    TxOutVariantAddress -> select $ do
      (txOut :& addr :& _txInTx :& _txIn :& txOutTx :& blk) <-
        from $
          table @VA.TxOut
            `innerJoin` table @VA.Address
              `on` (\(txOut :& addr) -> txOut ^. VA.TxOutAddressId ==. addr ^. VA.AddressId)
            `innerJoin` table @Tx
              `on` (\(txOut :& _addr :& txInTx) -> txOut ^. VA.TxOutTxId ==. txInTx ^. TxId)
            `innerJoin` table @TxIn
              `on` (\(txOut :& _addr :& txInTx :& txIn) -> txIn ^. TxInTxOutId ==. txInTx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. VA.TxOutIndex)
            `innerJoin` table @Tx
              `on` (\(_txOut :& _addr :& _txInTx :& txIn :& txOutTx) -> txOutTx ^. TxId ==. txIn ^. TxInTxInId)
            `innerJoin` table @Block
              `on` (\(_txOut :& _addr :& _txInTx :& _txIn :& txOutTx :& blk) -> txOutTx ^. TxBlockId ==. blk ^. BlockId)

      where_ (addr ^. VA.AddressStakeAddressId ==. just (val saId))
      pure (txOutTx ^. TxHash, blk ^. BlockTime, txOut ^. VA.TxOutValue)

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
