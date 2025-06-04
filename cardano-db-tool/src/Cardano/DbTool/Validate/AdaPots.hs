{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Validate.AdaPots (
  validateSumAdaPots,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Validate.Util
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Word (Word64)

-- | Validate that for all epochs, the sum of the AdaPots values are always the
-- same.
validateSumAdaPots :: IO ()
validateSumAdaPots = do
  putStrF "Sum of AdaPots amounts is constant across epochs: "

  xs <- DB.runDbNoLoggingEnv queryAdaPotsAccounting
  let uniqueCount = List.length $ List.nubOrd (map accSumAdaPots xs)

  if
    | uniqueCount == 0 -> error $ redText "No AdaPots entries found"
    | length xs == 1 -> putStrLn $ greenText "ok (but only one AdaPots entry found)"
    | uniqueCount == 1 -> putStrLn $ greenText "ok"
    | otherwise -> error $ redText (show uniqueCount ++ " unique AdaPots sums (should be 1)")

-- -----------------------------------------------------------------------------

data Accounting = Accounting
  { accEpochNo :: Word64
  , accSumAdaPots :: DB.Ada
  }

queryAdaPotsAccounting :: MonadIO m => DB.DbAction m [Accounting]
queryAdaPotsAccounting = do
  adaPotsSums <- DB.queryAdaPotsSum
  pure $ map convertToAccounting adaPotsSums
  where
    convertToAccounting :: DB.AdaPotsSum -> Accounting
    convertToAccounting aps =
      Accounting
        { accEpochNo = DB.apsEpochNo aps
        , accSumAdaPots = DB.word64ToAda $ DB.apsSum aps
        }
