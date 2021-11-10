{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Era.Shelley.ValidateWithdrawal
  ( validateRewardWithdrawals
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Cardano.BM.Trace (Trace, logError)

import           Cardano.Db (Ada (..))
import qualified Cardano.Db as Db
import           Cardano.Slotting.Slot (EpochNo (..))
import           Cardano.Sync.Util

import           Data.Either (partitionEithers)
import           Data.Fixed (Micro)
import qualified Data.List as List
import           Data.Text (Text)

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), asc, distinct, from,
                   groupBy, having, on, orderBy, select, sum_, unValue, val, where_, (<.), (==.),
                   (^.))

import           Database.Persist.Sql (SqlBackend)


-- For any stake address which has seen a withdrawal, the sum of the withdrawals for that address
-- should be less than or equal to the sum of the rewards for that address.

validateRewardWithdrawals
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> ReaderT SqlBackend m ()
validateRewardWithdrawals trce (EpochNo epochNo) = do
  res <- mapM validateAccounting =<< queryWithdrawalAddresses
  _bad <- queryBadWithdrawals
  liftIO $
    case partitionEithers res of
      ([], _) -> pure ()
      (xs, _) -> do
        logError trce . mconcat $
            [ textShow epochNo, ": ", textShow (length xs), " errors, eg\n"]
            ++ List.intersperse "\n" (map reportError xs)
        panicAbort "Validation failure"

-- -----------------------------------------------------------------------------

data AddressInfo = AddressInfo
  { aiStakeAddress :: !Text
  , aiSumRewards :: !Ada
  , aiSumWithdrawals :: !Ada
  } deriving (Eq, Ord, Show)

reportError :: AddressInfo -> Text
reportError ai =
  mconcat
    [ "  ", aiStakeAddress ai, " rewards are ", textShow (aiSumRewards ai)
    , " ADA and withdrawals are ", textShow (aiSumWithdrawals ai), " ADA"
    ]

-- For a given TxId, validate the input/output accounting.
validateAccounting
    :: (MonadBaseControl IO m, MonadIO m)
    => Db.StakeAddressId -> ReaderT SqlBackend m  (Either AddressInfo ())
validateAccounting addrId = do
   ai <- queryAddressInfo addrId
   pure $ if aiSumRewards ai < aiSumWithdrawals ai
            then Left ai
            else Right ()

-- -------------------------------------------------------------------------------------------------

-- Get all stake addresses with have seen a withdrawal, and return them in shuffled order.
queryWithdrawalAddresses :: MonadIO m => ReaderT SqlBackend m [Db.StakeAddressId]
queryWithdrawalAddresses = do
  res <- select . distinct . from $ \ wd -> do
            orderBy [asc (wd ^. Db.WithdrawalAddrId)]
            pure (wd ^. Db.WithdrawalAddrId)
  pure $ map unValue res

queryAddressInfo :: MonadIO m => Db.StakeAddressId -> ReaderT SqlBackend m AddressInfo
queryAddressInfo addrId = do
    rwds <- select . from $ \ rwd -> do
              where_ (rwd ^. Db.RewardAddrId ==. val addrId)
              pure (sum_ $ rwd ^. Db.RewardAmount)
    wdls <- select . from $ \ wdl -> do
              where_ (wdl ^. Db.WithdrawalAddrId ==. val addrId)
              pure (sum_ (wdl ^. Db.WithdrawalAmount))
    view <- select . from $ \ saddr -> do
              where_ (saddr ^. Db.StakeAddressId ==. val addrId)
              pure (saddr ^. Db.StakeAddressView)
    pure $ convert (Db.listToMaybe rwds) (Db.listToMaybe wdls) (Db.listToMaybe view)
  where
    convert :: Maybe (Value (Maybe Micro)) -> Maybe (Value (Maybe Micro)) -> Maybe (Value Text) -> AddressInfo
    convert rAmount wAmount mview =
        AddressInfo
          { aiStakeAddress  = maybe "unknown" unValue mview
          , aiSumRewards = Db.unValueSumAda rAmount
          , aiSumWithdrawals = Db.unValueSumAda wAmount
          }

-- A stake address state is bad if sum rewards < sum withdrawals
queryBadWithdrawals :: MonadIO m => ReaderT SqlBackend m [AddressInfo]
queryBadWithdrawals = do
    res <- select . from $ \ (rwd `InnerJoin` wdrl `InnerJoin` sa) -> do
              on (rwd ^. Db.RewardAddrId ==. sa ^. Db.StakeAddressId)
              on (rwd ^. Db.RewardAddrId ==. wdrl ^. Db.WithdrawalAddrId)
              groupBy (sa ^. Db.StakeAddressId)
              let sumReward = sum_ (rwd ^. Db.RewardAmount)
                  sumWithdraw = sum_ (wdrl ^. Db.WithdrawalAmount)
              having (sumReward <. sumWithdraw)
              pure (sa ^. Db.StakeAddressView, sumReward, sumWithdraw)
    pure $ List.sort (map convert res)
  where
    convert :: (Value Text, Value (Maybe Micro), Value (Maybe Micro)) -> AddressInfo
    convert (Value saView, rwdTotal, wdrlTotal) =
      AddressInfo
        { aiStakeAddress  = saView
        , aiSumRewards  = Db.unValueSumAda (Just rwdTotal)
        , aiSumWithdrawals = Db.unValueSumAda (Just wdrlTotal)
        }

