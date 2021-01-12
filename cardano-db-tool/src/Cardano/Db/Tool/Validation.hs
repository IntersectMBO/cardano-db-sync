module Cardano.Db.Tool.Validation
  ( LedgerValidationParams (..)
  , runValidation
  , runLedgerValidation
  ) where

import           Cardano.Db.Tool.Validate.BlockProperties (validateBlockProperties)
import           Cardano.Db.Tool.Validate.BlockTxs (validateEpochBlockTxs)
import           Cardano.Db.Tool.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.Tool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import           Cardano.Db.Tool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Cardano.Db.Tool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Cardano.Db.Tool.Validate.TxAccounting (validateTxAccounting)

runValidation :: IO ()
runValidation = do
  slowValidations
  fastValidations

runLedgerValidation :: LedgerValidationParams -> IO ()
runLedgerValidation =
  validateLedger

fastValidations :: IO ()
fastValidations = do
  validateAllPoolsHaveOwners
  validateTxAccounting
  validateBlockProperties

slowValidations :: IO ()
slowValidations = do
  validateTotalSupplyDecreasing
  validateEpochTable
  validateEpochBlockTxs
