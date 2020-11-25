{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Era.Allegra.Types
  ( CardanoProtocol
  , ShelleyAddress
  , ShelleyBlock
  , AllegraDCert
  , AllegraDelegCert
  , ShelleyHash
  , AllegraMIRCert
  , AllegraPoolCert
  , AllegraPoolParams
  , AllegraRewardAccount
  , AllegraStakeCreds
  , AllegraStakePoolKeyHash
  , AllegraStakingCred
  , AllegraStakingKeyHash
  , AllegraTx
  , AllegraTxBody
  , AllegraTxId
  , AllegraTxIn
  , AllegraTxOut
  , AllegraTxSeq
  ) where

import           Cardano.DbSync.Config.Types (CardanoProtocol)

import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardCrypto)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

type ShelleyAddress = Shelley.Addr StandardAllegra
type ShelleyBlock = Shelley.ShelleyBlock StandardAllegra
type AllegraDCert = Shelley.DCert StandardAllegra
type AllegraDelegCert = Shelley.DelegCert StandardAllegra
type ShelleyHash = Shelley.ShelleyHash StandardAllegra
type AllegraMIRCert = Shelley.MIRCert StandardAllegra
type AllegraPoolCert = Shelley.PoolCert StandardAllegra
type AllegraPoolParams = Shelley.PoolParams StandardAllegra
type AllegraRewardAccount = Shelley.RewardAcnt StandardAllegra
type AllegraStakeCreds = Shelley.StakeCreds StandardAllegra
type AllegraStakingCred = Shelley.StakeCredential StandardAllegra
type AllegraStakingKeyHash = Shelley.KeyHash 'Shelley.Staking StandardCrypto
type AllegraStakePoolKeyHash = Shelley.KeyHash 'Shelley.StakePool StandardCrypto
type AllegraTx = Shelley.Tx StandardAllegra
type AllegraTxBody = Shelley.TxBody StandardAllegra
type AllegraTxId = Shelley.TxId StandardAllegra
type AllegraTxIn = Shelley.TxIn StandardAllegra
type AllegraTxOut = Shelley.TxOut StandardAllegra
type AllegraTxSeq = Shelley.TxSeq StandardAllegra
