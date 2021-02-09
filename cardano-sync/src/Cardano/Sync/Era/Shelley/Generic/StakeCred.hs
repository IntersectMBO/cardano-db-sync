{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Cardano.Prelude

import           Cardano.Sync.Config.Types (DbSyncEnv (..))

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley


newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

toStakeCred :: DbSyncEnv -> Shelley.Credential 'Shelley.Staking era -> StakeCred
toStakeCred env cred =
  StakeCred $ Shelley.serialiseRewardAcnt (Shelley.RewardAcnt (envNetwork env) cred)
