{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Cardano.Prelude

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Keys as Ledger

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley

newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

toStakeCred :: Ledger.Network -> Shelley.Credential 'Ledger.Staking era -> StakeCred
toStakeCred network cred =
  StakeCred $ Shelley.serialiseRewardAcnt (Shelley.RewardAcnt network cred)
