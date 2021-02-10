{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Cardano.Prelude

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley

newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

toStakeCred :: Shelley.Network -> Shelley.Credential 'Shelley.Staking era -> StakeCred
toStakeCred network cred =
  StakeCred $ Shelley.serialiseRewardAcnt (Shelley.RewardAcnt network cred)
