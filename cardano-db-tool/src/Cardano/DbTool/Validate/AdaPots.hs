{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
module Cardano.DbTool.Validate.AdaPots
  ( validateSumAdaPots
  ) where

import           Cardano.Db
import           Cardano.DbTool.Validate.Util

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.List as List
import qualified Data.List.Extra as List
import           Data.Word (Word64)

import           Database.Esqueleto.Legacy (Entity (..), Value (..), from, select, (^.))

import           Database.Persist.Sql (SqlBackend)


-- | Validate that for all epochs, the sum of the AdaPots values are always the
-- same.
validateSumAdaPots :: IO ()
validateSumAdaPots = do
  putStrF "Sum of AdaPots amounts is constant across epochs: "

  xs <- runDbNoLoggingEnv queryAdaPotsAccounting
  let uniqueCount = List.length $ List.nubOrd (map accSumAdaPots xs)

  if
    | uniqueCount == 0 -> error $ redText "No AdaPots entries found"
    | length xs == 1 -> putStrLn $ greenText "ok (but only one AdaPots entry found)"
    | uniqueCount == 1 -> putStrLn $ greenText "ok"
    | otherwise -> error $ redText (show uniqueCount ++ " unique AdaPots sums (should be 1)" )

-- -----------------------------------------------------------------------------

data Accounting = Accounting
  { accEpochNo :: Word64
  , accSumAdaPots :: Ada
  }

queryAdaPotsAccounting :: MonadIO m => ReaderT SqlBackend m [Accounting]
queryAdaPotsAccounting = do
    res <- select . from $ \ ap ->
              pure (ap ^. AdaPotsEpochNo, ap)
    pure $ map convert res
  where
    convert :: (Value Word64, Entity AdaPots) -> Accounting
    convert (Value epochNum, Entity _ ap) =
      Accounting
        { accEpochNo = epochNum
        , accSumAdaPots = word64ToAda
                            $ unDbLovelace (adaPotsTreasury ap)
                                + unDbLovelace (adaPotsReserves ap)
                                + unDbLovelace (adaPotsRewards ap)
                                + unDbLovelace (adaPotsUtxo ap)
                                + unDbLovelace (adaPotsDeposits ap)
                                + unDbLovelace (adaPotsFees ap)
        }
