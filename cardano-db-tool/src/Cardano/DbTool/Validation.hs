{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbTool.Validation (
  LedgerValidationParams (..),
  runDbValidation,
  runLedgerValidation,
) where

import Cardano.Db (TxOutVariantType)
import Cardano.DbTool.Validate.AdaPots (validateSumAdaPots)
import Cardano.DbTool.Validate.BlockProperties (validateBlockProperties)
import Cardano.DbTool.Validate.BlockTxs (validateEpochBlockTxs)
import Cardano.DbTool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import Cardano.DbTool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import Cardano.DbTool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import Cardano.DbTool.Validate.TxAccounting (validateTxAccounting)
import Cardano.DbTool.Validate.Withdrawal (validateWithdrawals)
import Control.Exception (ErrorCall (..), SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (unless)
import System.Exit (exitFailure)

runDbValidation :: TxOutVariantType -> IO ()
runDbValidation txOutVariantType = do
  fast <- fastValidations
  slow <- slowValidations txOutVariantType
  unless (and (fast <> slow)) exitFailure

runLedgerValidation :: LedgerValidationParams -> TxOutVariantType -> IO ()
runLedgerValidation =
  validateLedger

-- -------------------------------------------------------------------------------------------------

fastValidations :: IO [Bool]
fastValidations =
  sequence
    [ runCheck validateAllPoolsHaveOwners
    , runCheck validateBlockProperties
    , runCheck validateSumAdaPots
    ]

slowValidations :: TxOutVariantType -> IO [Bool]
slowValidations txOutVariantType =
  sequence
    [ runCheck (validateTxAccounting txOutVariantType)
    , runCheck validateWithdrawals
    , runCheck validateEpochBlockTxs
    , runCheck (validateTotalSupplyDecreasing txOutVariantType)
    ]

-- Run a single check, reporting a failure instead of aborting the remaining checks.
runCheck :: IO () -> IO Bool
runCheck action = do
  result <- try action
  case result of
    Right () -> pure True
    Left (e :: SomeException)
      -- Let async exceptions (e.g. Ctrl-C) propagate instead of counting them as a failed check.
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          putStrLn (renderCheckError e)
          pure False

renderCheckError :: SomeException -> String
renderCheckError e =
  case fromException e of
    Just (ErrorCall msg) -> msg
    Nothing -> displayException e
