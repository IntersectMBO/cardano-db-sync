module Cardano.Db.App.Validation
  ( runValidation
  ) where

import           Cardano.Db.App.Validate.BlockTxs (validateEpochBlockTxs)
import           Cardano.Db.App.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.App.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Cardano.Db.App.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Cardano.Db.App.Validate.TxAccounting (validateTxAccounting)

runValidation :: IO ()
runValidation = do
  slowValidations
  fastValidations

fastValidations :: IO ()
fastValidations = do
  validateAllPoolsHaveOwners
  validateTxAccounting

slowValidations :: IO ()
slowValidations = do
  validateTotalSupplyDecreasing
  validateEpochTable
  validateEpochBlockTxs
