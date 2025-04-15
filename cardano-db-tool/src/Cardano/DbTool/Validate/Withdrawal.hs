{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Validate.Withdrawal (
  validateWithdrawals,
) where

import Cardano.Db
import Cardano.DbTool.Validate.Util
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (partitionEithers)
import Data.Fixed (Micro)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Esqueleto.Experimental (
  SqlBackend,
  Value (..),
  distinct,
  from,
  select,
  sum_,
  table,
  unValue,
  val,
  where_,
  (==.),
  (^.),
 )
import System.Random.Shuffle (shuffleM)

-- For any stake address which has seen a withdrawal, the sum of the withdrawals for that address
-- should be less than or equal to the sum of the rewards for that address.

validateWithdrawals :: IO ()
validateWithdrawals = do
  res <- runDbNoLoggingEnv $ mapM validateAccounting . take 1000 =<< queryWithdrawalAddresses
  putStrF $ "For " ++ show (length res) ++ " withdrawal addresses, sum withdrawals <= sum rewards: "
  case partitionEithers res of
    ([], _) -> putStrLn $ greenText "ok"
    (xs, _) -> error $ redText (show (length xs) ++ " errors:\n" ++ unlines (map reportError xs))

-- -----------------------------------------------------------------------------

data AddressInfo = AddressInfo
  { aiStakeAddressId :: !StakeAddressId
  , aiStakeAddress :: !Text
  , aiSumRewards :: !Ada
  , aiSumWithdrawals :: !Ada
  }
  deriving (Show)

reportError :: AddressInfo -> String
reportError ai =
  mconcat
    [ "  "
    , Text.unpack (aiStakeAddress ai)
    , " rewards are "
    , show (aiSumRewards ai)
    , " ADA and withdrawals are "
    , show (aiSumWithdrawals ai)
    , " ADA"
    ]

-- For a given StakeAddressId, validate that sum rewards >= sum withdrawals.
validateAccounting :: MonadIO m => StakeAddressId -> DB.DbAction m (Either AddressInfo ())
validateAccounting addrId = do
  ai <- queryAddressInfo addrId
  pure $
    if aiSumRewards ai < aiSumWithdrawals ai
      then Left ai
      else Right ()

queryAddressInfo :: MonadIO m => StakeAddressId -> DbAction m AddressInfo
queryAddressInfo addrId = do
  result <- queryAddressInfoData addrId
  pure $ makeAddressInfo addrId result

makeAddressInfo :: StakeAddressId -> (Ada, Ada, Maybe Text) -> AddressInfo
makeAddressInfo addrId (rewards, withdrawals, view) =
  AddressInfo
    { aiStakeAddressId = addrId
    , aiStakeAddress = fromMaybe "unknown" view
    , aiSumRewards = rewards
    , aiSumWithdrawals = withdrawals
    }
