{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.StakeCred
  ( StakeCred (..)
  , toStakeCred
  ) where

import           Cardano.Prelude

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger

newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

toStakeCred :: Ledger.Network -> Ledger.Credential 'Ledger.Staking era -> StakeCred
toStakeCred network cred =
  StakeCred $ Ledger.serialiseRewardAcnt (Ledger.RewardAcnt network cred)
