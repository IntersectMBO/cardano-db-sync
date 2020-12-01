{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.DbSync.Era.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Cardano.DbSync.Config.Types (DbSyncEnv (..))

import           Cardano.Prelude

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley


newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

toStakeCred :: DbSyncEnv -> Shelley.Credential 'Shelley.Staking era -> StakeCred
toStakeCred env cred =
  StakeCred $ Shelley.serialiseRewardAcnt (Shelley.RewardAcnt (envNetwork env) cred)
