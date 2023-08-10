{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Mock.Forging.Tx.Generic (
  allPoolStakeCert,
  resolveAddress,
  resolveUTxOIndex,
  resolveStakeCreds,
  resolvePool,
  createStakeCredentials,
  createPaymentCredentials,
  mkDummyScriptHash,
  unregisteredGenesisKeys,
  mkDummyHash,
  unregisteredKeyHash,
  unregisteredWitnessKey,
  unregisteredAddresses,
  unregisteredStakeCredentials,
  unregisteredPools,
) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Era (Era (..), EraCrypto)
import Cardano.Ledger.Hashes (ScriptHash (ScriptHash))
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.UTxO
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (length, (.))
import Data.Coerce (coerce)
import Data.List (nub)
import Data.List.Extra ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

resolveAddress ::
  forall era p.
  (EraCrypto era ~ StandardCrypto, Core.EraTxOut era) =>
  UTxOIndex era ->
  LedgerState (ShelleyBlock p era) ->
  Either ForgingError (Addr (EraCrypto era))
resolveAddress index st = case index of
  UTxOAddressNew n -> Right $ Addr Testnet (unregisteredAddresses !! n) StakeRefNull
  UTxOAddressNewWithStake n stakeIndex -> do
    stakeCred <- resolveStakeCreds stakeIndex st
    Right $ Addr Testnet (unregisteredAddresses !! n) (StakeRefBase stakeCred)
  UTxOAddress addr -> Right addr
  UTxOAddressNewWithPtr n ptr ->
    Right $ Addr Testnet (unregisteredAddresses !! n) (StakeRefPtr ptr)
  _ -> (^. Core.addrTxOutL) . snd . fst <$> resolveUTxOIndex index st

resolveUTxOIndex ::
  forall era p.
  (EraCrypto era ~ StandardCrypto, Core.EraTxOut era) =>
  UTxOIndex era ->
  LedgerState (ShelleyBlock p era) ->
  Either ForgingError ((TxIn (EraCrypto era), Core.TxOut era), UTxOIndex era)
resolveUTxOIndex index st = toLeft $ case index of
  UTxOIndex n -> utxoPairs !? n
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
    utxoPairs :: [(TxIn (EraCrypto era), Core.TxOut era)]
    utxoPairs =
      Map.toList $
        unUTxO $
          utxosUtxo $
            lsUTxOState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st

    hasAddr addr (_, txOut) = addr == txOut ^. Core.addrTxOutL
    hasInput inp (inp', _) = inp == inp'

    toLeft :: Maybe (TxIn (EraCrypto era), Core.TxOut era) -> Either ForgingError ((TxIn (EraCrypto era), Core.TxOut era), UTxOIndex era)
    toLeft Nothing = Left CantFindUTxO
    toLeft (Just (txIn, txOut)) = Right ((txIn, txOut), UTxOInput txIn)

resolveStakeCreds ::
  EraCrypto era ~ StandardCrypto =>
  StakeIndex ->
  LedgerState (ShelleyBlock p era) ->
  Either ForgingError (StakeCredential StandardCrypto)
resolveStakeCreds indx st = case indx of
  StakeIndex n -> toEither $ fst <$> (rewardAccs !? n)
  StakeAddress addr -> Right addr
  StakeIndexNew n -> toEither $ unregisteredStakeCredentials !? n
  StakeIndexScript bl -> Right $ if bl then alwaysSucceedsScriptStake else alwaysFailsScriptStake
  StakeIndexPoolLeader poolIndex -> Right $ getRwdCred $ ppRewardAcnt $ findPoolParams poolIndex
  StakeIndexPoolMember n poolIndex -> Right $ resolvePoolMember n poolIndex
  where
    rewardAccs =
      Map.toList $
        UMap.rewardMap $
          dsUnified $
            certDState $
              lsCertState $
                esLState $
                  nesEs $
                    Consensus.shelleyLedgerState st

    poolParams =
      psStakePoolParams $
        certPState $
          lsCertState $
            esLState $
              nesEs $
                Consensus.shelleyLedgerState st

    delegs = UMap.sPoolMap $ dsUnified dstate

    dstate =
      certDState $
        lsCertState $
          esLState $
            nesEs $
              Consensus.shelleyLedgerState st

    resolvePoolMember n poolIndex =
      let poolId = ppId (findPoolParams poolIndex)
          poolMembers = Map.keys $ Map.filter (== poolId) delegs
       in poolMembers !! n

    findPoolParams :: PoolIndex -> PoolParams StandardCrypto
    findPoolParams (PoolIndex n) = Map.elems poolParams !! n
    findPoolParams (PoolIndexId pid) = poolParams Map.! pid
    findPoolParams pix@(PoolIndexNew _) = poolParams Map.! resolvePool pix st

    toEither :: Maybe a -> Either ForgingError a
    toEither Nothing = Left CantFindStake
    toEither (Just a) = Right a

resolvePool ::
  EraCrypto era ~ StandardCrypto =>
  PoolIndex ->
  LedgerState (ShelleyBlock p era) ->
  KeyHash 'StakePool StandardCrypto
resolvePool pix st = case pix of
  PoolIndexId key -> key
  PoolIndex n -> ppId $ poolParams !! n
  PoolIndexNew n -> unregisteredPools !! n
  where
    poolParams =
      Map.elems $
        psStakePoolParams $
          certPState $
            lsCertState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st

allPoolStakeCert :: LedgerState (ShelleyBlock p era) -> [ShelleyTxCert era]
allPoolStakeCert st =
  ShelleyTxCertDelegCert . ShelleyRegCert <$> nub creds
  where
    poolParms =
      Map.elems $
        psStakePoolParams $
          certPState $
            lsCertState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st
    creds = concatMap getPoolStakeCreds poolParms

getPoolStakeCreds :: PoolParams c -> [StakeCredential c]
getPoolStakeCreds pparams =
  getRwdCred (ppRewardAcnt pparams)
    : (KeyHashObj <$> Set.toList (ppOwners pparams))

unregisteredStakeCredentials :: [StakeCredential StandardCrypto]
unregisteredStakeCredentials =
  [ KeyHashObj $ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHashObj $ KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHashObj $ KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredKeyHash :: [KeyHash 'Staking StandardCrypto]
unregisteredKeyHash =
  [ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredWitnessKey :: [KeyHash 'Witness StandardCrypto]
unregisteredWitnessKey =
  [ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredAddresses :: [PaymentCredential StandardCrypto]
unregisteredAddresses =
  [ KeyHashObj $ KeyHash "11121865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  ]

unregisteredPools :: [KeyHash 'StakePool StandardCrypto]
unregisteredPools =
  [ KeyHash "11138475621387465239786593240875634298756324987562352435"
  , KeyHash "22246254326479503298745680239746523897456238974563298348"
  , KeyHash "33323876542397465497834256329487563428975634827956348975"
  ]

unregisteredGenesisKeys :: [KeyHash 'Genesis StandardCrypto]
unregisteredGenesisKeys =
  [ KeyHash "11138475621387465239786593240875634298756324987562352435"
  , KeyHash "22246254326479503298745680239746523897456238974563298348"
  , KeyHash "33323876542397465497834256329487563428975634827956348975"
  ]

createStakeCredentials :: Int -> [StakeCredential StandardCrypto]
createStakeCredentials n =
  fmap (KeyHashObj . KeyHash . mkDummyHash (Proxy @(ADDRHASH StandardCrypto))) [1 .. n]

createPaymentCredentials :: Int -> [PaymentCredential StandardCrypto]
createPaymentCredentials n =
  fmap (KeyHashObj . KeyHash . mkDummyHash (Proxy @(ADDRHASH StandardCrypto))) [1 .. n]

mkDummyScriptHash :: Int -> ScriptHash StandardCrypto
mkDummyScriptHash n = ScriptHash $ mkDummyHash (Proxy @(ADDRHASH StandardCrypto)) n

{-# ANN module ("HLint: ignore Avoid restricted function" :: Text) #-}

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . hashWithSerialiser @h toCBOR
