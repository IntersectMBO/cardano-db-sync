module Cardano.DbTool.Validate.PoolOwner (
  validateAllPoolsHaveOwners,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Validate.Util

validateAllPoolsHaveOwners :: IO ()
validateAllPoolsHaveOwners = do
  putStrF "All pools have owners : "
  count <- DB.runDbStandaloneSilent DB.queryPoolsWithoutOwners
  if count == 0
    then putStrLn $ greenText "ok"
    else putStrLn $ redText ("Failed, " ++ show count ++ " pools are without owners.")
