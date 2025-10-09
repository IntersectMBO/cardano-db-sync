{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Validate.Withdrawal (
  validateWithdrawals,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Validate.Util
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Random.Shuffle (shuffleM)

-- For any stake address which has seen a withdrawal, the sum of the withdrawals for that address
-- should be less than or equal to the sum of the rewards for that address.

validateWithdrawals :: IO ()
validateWithdrawals = do
  res <- DB.runDbStandaloneSilent $ do
    addresses <- DB.queryWithdrawalAddresses
    shuffledAddresses <- liftIO $ shuffleM addresses
    mapM validateAccounting (take 1000 shuffledAddresses)
  putStrF $ "For " ++ show (length res) ++ " withdrawal addresses, sum withdrawals <= sum rewards: "
  case partitionEithers res of
    ([], _) -> putStrLn $ greenText "ok"
    (xs, _) -> error $ redText (show (length xs) ++ " errors:\n" ++ unlines (map reportError xs))

--------------------------------------------------------------------------------

data AddressInfo = AddressInfo
  { aiStakeAddressId :: !DB.StakeAddressId
  , aiStakeAddress :: !Text
  , aiSumRewards :: !DB.Ada
  , aiSumWithdrawals :: !DB.Ada
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
validateAccounting :: DB.StakeAddressId -> DB.DbM (Either AddressInfo ())
validateAccounting addrId = do
  ai <- queryAddressInfo addrId
  pure $
    if aiSumRewards ai < aiSumWithdrawals ai
      then Left ai
      else Right ()

queryAddressInfo :: DB.StakeAddressId -> DB.DbM AddressInfo
queryAddressInfo addrId = do
  result <- DB.queryAddressInfoData addrId
  pure $ makeAddressInfo addrId result

makeAddressInfo :: DB.StakeAddressId -> (DB.Ada, DB.Ada, Maybe Text) -> AddressInfo
makeAddressInfo addrId (rewards, withdrawals, view) =
  AddressInfo
    { aiStakeAddressId = addrId
    , aiStakeAddress = fromMaybe "unknown" view
    , aiSumRewards = rewards
    , aiSumWithdrawals = withdrawals
    }
