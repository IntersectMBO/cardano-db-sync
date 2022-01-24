{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Generic
  ( allPoolStakeCert
  , resolveAddress
  , resolveUTxOIndex
  , resolveStakeCreds
  , resolvePool
  ) where

import           Cardano.Prelude hiding ((.), length)

import           Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import           Control.SetAlgebra

import           Cardano.Ledger.Address
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys
import           Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import           Cardano.Ledger.Shelley.TxBody
import           Cardano.Ledger.Shelley.UTxO
import           Cardano.Ledger.TxIn (TxIn (..))

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Types

resolveAddress :: forall era. (Crypto era ~ StandardCrypto, HasField "address" (Core.TxOut era) (Addr (Crypto era)))
               => UTxOIndex era -> LedgerState (ShelleyBlock era)
               -> Either ForgingError (Addr (Crypto era))
resolveAddress index st = case index of
    UTxOAddressNew n -> Right $ Addr Testnet (unregisteredAddresses !! n) StakeRefNull
    UTxOAddressNewWithStake n stakeIndex -> do
      stakeCred <- resolveStakeCreds stakeIndex st
      Right $ Addr Testnet (unregisteredAddresses !! n) (StakeRefBase stakeCred)
    UTxOAddress addr -> Right addr
    UTxOAddressNewWithPtr n ptr ->
      Right $ Addr Testnet (unregisteredAddresses !! n) (StakeRefPtr ptr)
    _ -> getField @"address" . snd . fst <$> resolveUTxOIndex index st

resolveUTxOIndex :: forall era. (Crypto era ~ StandardCrypto, HasField "address" (Core.TxOut era) (Addr (Crypto era)))
                 => UTxOIndex era -> LedgerState (ShelleyBlock era)
                 -> Either ForgingError ((TxIn (Crypto era), Core.TxOut era), UTxOIndex era)
resolveUTxOIndex index st = toLeft $ case index of
    UTxOIndex n -> safeIndex n utxoPairs
    UTxOAddress addr -> find (hasAddr addr) utxoPairs
    UTxOInput input -> find (hasInput input) utxoPairs
    UTxOPair pair -> Just pair
    UTxOAddressNew _ -> do
      addr <- rightToMaybe $ resolveAddress index st
      find (hasAddr addr) utxoPairs
    UTxOAddressNewWithStake _ _ -> do
      addr <- rightToMaybe $ resolveAddress index st
      find (hasAddr addr) utxoPairs
    UTxOAddressNewWithPtr _ _ -> do
      addr <- rightToMaybe $ resolveAddress index st
      find (hasAddr addr) utxoPairs
  where
    utxoPairs :: [(TxIn (Crypto era), Core.TxOut era)]
    utxoPairs = Map.toList $ unUTxO $ _utxo $ _utxoState $ esLState $
        nesEs $ Consensus.shelleyLedgerState st

    safeIndex :: Int -> [a] -> Maybe a
    safeIndex n ls
      | n < length ls = Just $ ls !! n
      | True = Nothing

    hasAddr addr (_, txOut) = addr == getField @"address" txOut
    hasInput inp (inp', _) = inp == inp'

    toLeft :: Maybe (TxIn (Crypto era), Core.TxOut era) -> Either ForgingError ((TxIn (Crypto era), Core.TxOut era), UTxOIndex era)
    toLeft Nothing = Left $ CantFindUTxO
    toLeft (Just  (txIn, txOut)) = Right ((txIn, txOut), UTxOInput txIn)

resolveStakeCreds :: (Crypto era ~ StandardCrypto)
                  => StakeIndex -> LedgerState (ShelleyBlock era)
                  -> Either ForgingError (StakeCredential StandardCrypto)
resolveStakeCreds indx st = case indx of
    StakeIndex n -> Right $ rewardAccs !! n
    StakeAddress addr -> Right addr
    StakeIndexNew n -> Right $ unregisteredStakeCredentials !! n
    StakeIndexScript bl -> Right $ if bl then alwaysSucceedsScriptStake else alwaysFailsScriptStake
    StakeIndexPoolLeader poolIndex -> Right $ getRwdCred $  _poolRAcnt $ findPoolParams poolIndex
    StakeIndexPoolMember n poolIndex -> Right $ resolvePoolMember n poolIndex
  where
    rewardAccs = Map.keys $ _rewards $ _dstate $ _delegationState $ esLState $
        nesEs $ Consensus.shelleyLedgerState st

    poolParams = _pParams $ _pstate $ _delegationState $ esLState $
        nesEs $ Consensus.shelleyLedgerState st

    delegations = _delegations dstate
    pointers = _ptrs dstate

    dstate = _dstate $ _delegationState $ esLState $
        nesEs $ Consensus.shelleyLedgerState st

    resolvePoolMember n poolIndex =
      let poolId = _poolId (findPoolParams poolIndex)
          poolMembers = Map.keys $ Map.filter (== poolId) delegations
      in poolMembers !! n

    findPoolParams :: PoolIndex -> PoolParams StandardCrypto
    findPoolParams (PoolIndex n) = (Map.elems poolParams) !! n
    findPoolParams (PoolIndexId pid) = poolParams Map.! pid

resolvePool :: (Crypto era ~ StandardCrypto)
            => PoolIndex -> LedgerState (ShelleyBlock era)
            -> KeyHash 'StakePool StandardCrypto
resolvePool pix st = case pix of
    PoolIndexId key -> key
    PoolIndex n -> _poolId $ poolParams !! n
  where
    poolParams = Map.elems $ _pParams $ _pstate $ _delegationState $ esLState $
        nesEs $ Consensus.shelleyLedgerState st

allPoolStakeCert :: LedgerState (ShelleyBlock era) -> [DCert (Crypto era)]
allPoolStakeCert st =
    DCertDeleg . RegKey <$> nub creds
  where
    poolParms = Map.elems $ _pParams $ _pstate $ _delegationState $
      esLState $ nesEs $ Consensus.shelleyLedgerState st
    creds = concatMap getPoolStakeCreds poolParms

getPoolStakeCreds :: PoolParams c -> [StakeCredential c]
getPoolStakeCreds pparams =
     (getRwdCred (_poolRAcnt pparams)) :
      (KeyHashObj <$> Set.toList (_poolOwners pparams))

unregisteredStakeCredentials :: [StakeCredential StandardCrypto]
unregisteredStakeCredentials =
  [ KeyHashObj $ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHashObj $ KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHashObj $ KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredAddresses :: [PaymentCredential StandardCrypto]
unregisteredAddresses =
  [ KeyHashObj $ KeyHash "11121865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  ]
