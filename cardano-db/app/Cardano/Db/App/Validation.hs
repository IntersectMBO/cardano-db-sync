module Cardano.Db.App.Validation
  ( runValidation
  ) where

import           Cardano.Db.App.Validate.BlockTxs (validateEpochBlockTxs)
import           Cardano.Db.App.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.App.Validate.PoolOwner (validateAllPoolsHaveOwners)
import           Cardano.Db.App.Validate.TotalSupply (validateTotalSupplyDecreasing)

runValidation :: IO ()
runValidation = do
  validateTotalSupplyDecreasing
  validateEpochTable
  validateEpochBlockTxs
  validateAllPoolsHaveOwners
