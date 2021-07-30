module Cardano.Db.Tool.Validation
  ( LedgerValidationParams (..)
  , runDbValidation
  , runLedgerValidation
  ) where

import           Cardano.Db.Tool.Validate.AdaPots (validateSumAdaPots)
import           Cardano.Db.Tool.Validate.BlockProperties (validateBlockProperties)
import           Cardano.Db.Tool.Validate.BlockTxs (validateEpochBlockTxs)
import           Cardano.Db.Tool.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.Tool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import           Cardano.Db.Tool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Cardano.Db.Tool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Cardano.Db.Tool.Validate.TxAccounting (validateTxAccounting)

runDbValidation :: IO ()
runDbValidation = do
  fastValidations
  slowValidations

runLedgerValidation :: LedgerValidationParams -> IO ()
runLedgerValidation =
  validateLedger

-- -------------------------------------------------------------------------------------------------

fastValidations :: IO ()
fastValidations = do
  validateAllPoolsHaveOwners
  validateTxAccounting
  validateBlockProperties
  validateSumAdaPots

slowValidations :: IO ()
slowValidations = do
  validateEpochTable
  validateEpochBlockTxs
  validateTotalSupplyDecreasing
