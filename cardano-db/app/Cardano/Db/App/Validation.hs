module Cardano.Db.App.Validation
  ( runValidation
  ) where

import           Cardano.Db.App.Validate.EpochTable (validateEpochTable)
import           Cardano.Db.App.Validate.TotalSupply (validateTotalSupplyDecreasing)

runValidation :: Word -> IO ()
runValidation count = do
  validateTotalSupplyDecreasing count
  validateEpochTable (10 * count)
