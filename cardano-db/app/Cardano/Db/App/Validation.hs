module Cardano.Db.App.Validation
  ( runValidation
  ) where

import           Cardano.Db.App.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.App.Validate.TotalSupply (validateTotalSupplyDecreasing)
import           Cardano.Db.App.Validate.BlockTxs (validateEpochBlockTxs)

runValidation :: IO ()
runValidation = do
  validateTotalSupplyDecreasing
  validateEpochTable
  validateEpochBlockTxs
