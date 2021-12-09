{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbTool.Validate.Withdrawal
  ( validateWithdrawals
  ) where

import           Cardano.DbTool.Validate.Util

import           Cardano.Db

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Either (partitionEithers)
import           Data.Fixed (Micro)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Esqueleto.Legacy (Value (..), distinct, from, select, sum_, unValue, val,
                   where_, (==.), (^.))

import           Database.Persist.Sql (SqlBackend)

import           System.Random.Shuffle (shuffleM)

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
  } deriving Show

reportError :: AddressInfo -> String
reportError ai =
  mconcat
    [ "  ", Text.unpack (aiStakeAddress ai), " rewards are ", show (aiSumRewards ai)
    , " ADA and withdrawals are ", show (aiSumWithdrawals ai), " ADA"
    ]

-- For a given StakeAddressId, validate that sum rewards >= sum withdrawals.
validateAccounting :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m  (Either AddressInfo ())
validateAccounting addrId = do
   ai <- queryAddressInfo addrId
   pure $ if aiSumRewards ai < aiSumWithdrawals ai
            then Left ai
            else Right ()

-- -------------------------------------------------------------------------------------------------

-- Get all stake addresses with have seen a withdrawal, and return them in shuffled order.
queryWithdrawalAddresses :: MonadIO m => ReaderT SqlBackend m [StakeAddressId]
queryWithdrawalAddresses = do
  res <- select . distinct . from $ \ wd ->
            pure (wd ^. WithdrawalAddrId)
  liftIO $ shuffleM (map unValue res)

queryAddressInfo :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m AddressInfo
queryAddressInfo addrId = do
    rwds <- select . from $ \ rwd -> do
              where_ (rwd ^. RewardAddrId ==. val addrId)
              pure (sum_ $ rwd ^. RewardAmount)
    wdls <- select . from $ \ wdl -> do
              where_ (wdl ^. WithdrawalAddrId ==. val addrId)
              pure (sum_ (wdl ^. WithdrawalAmount))
    view <- select . from $ \ saddr -> do
              where_ (saddr ^. StakeAddressId ==. val addrId)
              pure (saddr ^. StakeAddressView)
    pure $ convert (listToMaybe rwds) (listToMaybe wdls) (listToMaybe view)
  where
    convert :: Maybe (Value (Maybe Micro)) -> Maybe (Value (Maybe Micro)) -> Maybe (Value Text) -> AddressInfo
    convert rAmount wAmount mview =
        AddressInfo
          { aiStakeAddressId = addrId
          , aiStakeAddress  = maybe "unknown" unValue mview
          , aiSumRewards = unValueSumAda rAmount
          , aiSumWithdrawals = unValueSumAda wAmount
          }
