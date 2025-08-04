{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbTool.Validate.TxAccounting (
  validateTxAccounting,
) where

import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbTool.Validate.Util
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left, runExceptT)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Word (Word64)
import qualified System.Random as Random

{- HLINT ignore "Fuse on/on" -}

validateTxAccounting :: DB.TxOutVariantType -> IO ()
validateTxAccounting getTxOutVariantType = do
  txIdRange <- DB.runDbStandaloneSilent DB.queryTestTxIds
  putStrF $
    "For "
      ++ show testCount
      ++ " transactions out of "
      ++ show (snd txIdRange)
      ++ " accounting is: "
  ids <- randomTxIds testCount txIdRange
  res <- runExceptT $ traverse (validateAccounting getTxOutVariantType) ids
  case res of
    Left err -> error $ redText (reportError err)
    Right _ -> putStrLn $ greenText "ok"
  where
    testCount :: Int
    testCount = 100

-------------------------------------------------------------------------------

data ValidateError = ValidateError
  { veTxId :: !Word64
  , veFee :: !DB.Ada
  , veDeposit :: !Int64
  , veWithdrawal :: !DB.Ada
  , inputs :: ![DB.TxOutW]
  , outputs :: ![DB.TxOutW]
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
    showTxOuts :: [DB.TxOutW] -> String
    showTxOuts = List.intercalate "," . map showTxOut

showTxOut :: DB.TxOutW -> String
showTxOut txo =
  mconcat
    [ "TxId "
    , show (DB.getTxId txId)
    , " Value "
    , show (DB.word64ToAda . DB.unDbLovelace $ value)
    ]
  where
    (txId, value) = case txo of
      DB.VCTxOutW cTxOut -> (VC.txOutCoreTxId cTxOut, VC.txOutCoreValue cTxOut)
      DB.VATxOutW vTxOut _ -> (VA.txOutAddressTxId vTxOut, VA.txOutAddressValue vTxOut)

-- For a given TxId, validate the input/output accounting.
validateAccounting :: DB.TxOutVariantType -> Word64 -> ExceptT ValidateError IO ()
validateAccounting txOutVariantType txId = do
  (fee, deposit) <- liftIO $ DB.runDbStandaloneSilent (DB.queryTxFeeDeposit txId)
  withdrawal <- liftIO $ DB.runDbStandaloneSilent (DB.queryTxWithdrawal txId)
  ins <- liftIO $ DB.runDbStandaloneSilent (DB.queryTxInputs txOutVariantType txId)
  outs <- liftIO $ DB.runDbStandaloneSilent (DB.queryTxOutputs txOutVariantType txId)
  -- A refund is a negative deposit.
  when (deposit >= 0 && sumValues ins + withdrawal /= fee + adaDeposit deposit + sumValues outs) $
    left (ValidateError txId fee deposit withdrawal ins outs)
  when (deposit < 0 && sumValues ins + adaRefund deposit + withdrawal /= fee + sumValues outs) $
    left (ValidateError txId fee deposit withdrawal ins outs)
  where
    adaDeposit :: Int64 -> DB.Ada
    adaDeposit = DB.word64ToAda . fromIntegral

    adaRefund :: Int64 -> DB.Ada
    adaRefund = DB.word64ToAda . fromIntegral . negate

sumValues :: [DB.TxOutW] -> DB.Ada
sumValues = DB.word64ToAda . sum . map txOutValue
  where
    txOutValue =
      DB.unDbLovelace . \case
        DB.VCTxOutW cTxOut -> VC.txOutCoreValue cTxOut
        DB.VATxOutW vTxOut _ -> VA.txOutAddressValue vTxOut
