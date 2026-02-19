{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  registeredByronGenesisKeys,
  registeredShelleyGenesisKeys,
  bootstrapCommitteeCreds,
  unregisteredCommitteeCreds,
  unregisteredDRepIds,
  consPoolParams,
  getPoolStakeCreds,
  resolveStakePoolVoters,
  drepVoters,
  committeeVoters,
) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Voter (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes (ADDRHASH, ScriptHash (ScriptHash))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), hashWithSerialiser)
import Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Mock.Forging.Crypto
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (length, map, (.))
import Cardano.Protocol.Crypto (hashVerKeyVRF)
import Data.Coerce (coerce)
import Data.List (nub)
import Data.List.Extra ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

resolveAddress ::
  forall era p mk.
  (Core.EraTxOut era, EraCertState era) =>
  UTxOIndex era ->
  LedgerState (ShelleyBlock p era) mk ->
  Either ForgingError Addr
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
  forall era p mk.
  (Core.EraTxOut era, EraCertState era) =>
  UTxOIndex era ->
  LedgerState (ShelleyBlock p era) mk ->
  Either ForgingError ((TxIn, Core.TxOut era), UTxOIndex era)
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
    utxoPairs :: [(TxIn, Core.TxOut era)]
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

    toLeft :: Maybe (TxIn, Core.TxOut era) -> Either ForgingError ((TxIn, Core.TxOut era), UTxOIndex era)
    toLeft Nothing = Left CantFindUTxO
    toLeft (Just (txIn, txOut)) = Right ((txIn, txOut), UTxOInput txIn)

resolveStakeCreds ::
  forall era p mk.
  EraCertState era =>
  StakeIndex ->
  LedgerState (ShelleyBlock p era) mk ->
  Either ForgingError StakeCredential
resolveStakeCreds indx st = case indx of
  StakeIndex n -> toEither $ fst <$> (Map.toList rewardAccs !? n)
  StakeAddress addr -> Right addr
  StakeIndexNew n -> toEither $ unregisteredStakeCredentials !? n
  StakeIndexScript bl -> Right $ if bl then alwaysSucceedsScriptStake else alwaysFailsScriptStake
  StakeIndexPoolLeader poolIndex -> Right $ raCredential $ ppRewardAccount $ findPoolParams poolIndex
  StakeIndexPoolMember n poolIndex -> Right $ resolvePoolMember n poolIndex
  where
    rewardAccs :: Map StakeCredential (AccountState era)
    rewardAccs = dsAccounts dstate ^. accountsMapL

    poolParams :: Map (KeyHash 'StakePool) PoolParams
    poolParams =
      let certState =
            lsCertState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st
       in Map.mapWithKey stakePoolStateToPoolParams $ psStakePools (certState ^. certPStateL)
    delegs :: Map StakeCredential (KeyHash 'StakePool)
    delegs = Map.mapMaybe (^. stakePoolDelegationAccountStateL) rewardAccs

    dstate =
      let certState =
            lsCertState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st
       in certState ^. certDStateL

    resolvePoolMember n poolIndex =
      let poolId = ppId (findPoolParams poolIndex)
          poolMembers = Map.keys $ Map.filter (== poolId) delegs
       in poolMembers !! n

    findPoolParams :: PoolIndex -> PoolParams
    findPoolParams (PoolIndex n) = Map.elems poolParams !! n
    findPoolParams (PoolIndexId pid) = poolParams Map.! pid
    findPoolParams pix@(PoolIndexNew _) = poolParams Map.! resolvePool pix st

    toEither :: Maybe a -> Either ForgingError a
    toEither Nothing = Left CantFindStake
    toEither (Just a) = Right a

resolvePool ::
  EraCertState era =>
  PoolIndex ->
  LedgerState (ShelleyBlock p era) mk ->
  KeyHash 'StakePool
resolvePool pix st = case pix of
  PoolIndexId key -> key
  PoolIndex n -> ppId $ poolParams !! n
  PoolIndexNew n -> unregisteredPools !! n
  where
    poolParams =
      let certState =
            lsCertState $
              esLState $
                nesEs $
                  Consensus.shelleyLedgerState st
       in Map.elems $ Map.mapWithKey stakePoolStateToPoolParams (certState ^. certPStateL . psStakePoolsL)

allPoolStakeCert :: EraCertState era => LedgerState (ShelleyBlock p era) mk -> [ShelleyTxCert era]
allPoolStakeCert st =
  ShelleyTxCertDelegCert . ShelleyRegCert <$> notRegisteredCreds
  where
    certState =
      Consensus.shelleyLedgerState st ^. nesEsL . esLStateL . lsCertStateL
    poolParms = Map.elems $ Map.mapWithKey stakePoolStateToPoolParams (certState ^. certPStateL . psStakePoolsL)
    creds = concatMap getPoolStakeCreds poolParms
    notRegisteredCreds =
      filter (`Map.notMember` (certState ^. certDStateL . accountsL . accountsMapL)) $ nub creds

getPoolStakeCreds :: PoolParams -> [StakeCredential]
getPoolStakeCreds pparams =
  raCredential (ppRewardAccount pparams)
    : (KeyHashObj <$> Set.toList (ppOwners pparams))

unregisteredStakeCredentials :: [StakeCredential]
unregisteredStakeCredentials =
  [ KeyHashObj $ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHashObj $ KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHashObj $ KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredKeyHash :: [KeyHash 'Staking]
unregisteredKeyHash =
  [ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredWitnessKey :: [KeyHash 'Witness]
unregisteredWitnessKey =
  [ KeyHash "000131350ac206583290486460934394208654903261221230945870"
  , KeyHash "11130293748658946834096854968435096854309685490386453861"
  , KeyHash "22236827154873624578632414768234573268457923654973246472"
  ]

unregisteredAddresses :: [PaymentCredential]
unregisteredAddresses =
  [ KeyHashObj $ KeyHash "11121865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  , KeyHashObj $ KeyHash "22221865734872361547862358673245672834567832456783245312"
  ]

unregisteredPools :: [KeyHash 'StakePool]
unregisteredPools =
  [ KeyHash "11138475621387465239786593240875634298756324987562352435"
  , KeyHash "22246254326479503298745680239746523897456238974563298348"
  , KeyHash "33323876542397465497834256329487563428975634827956348975"
  ]

unregisteredGenesisKeys :: [KeyHash 'Genesis]
unregisteredGenesisKeys =
  [ KeyHash "11138475621387465239786593240875634298756324987562352435"
  , KeyHash "22246254326479503298745680239746523897456238974563298348"
  , KeyHash "33323876542397465497834256329487563428975634827956348975"
  ]

registeredByronGenesisKeys :: [KeyHash 'Genesis]
registeredByronGenesisKeys =
  [ KeyHash "1a3e49767796fd99b057ad54db3310fd640806fcb0927399bbca7b43"
  ]

registeredShelleyGenesisKeys :: [KeyHash 'Genesis]
registeredShelleyGenesisKeys =
  [ KeyHash "30c3083efd794227fde2351a04500349d1b467556c30e35d6794a501"
  , KeyHash "471cc34983f6a2fd7b4018e3147532185d69a448d6570d46019e58e6"
  ]

bootstrapCommitteeCreds ::
  [ ( Credential 'ColdCommitteeRole
    , Credential 'HotCommitteeRole
    )
  ]
bootstrapCommitteeCreds =
  [
    ( ScriptHashObj $ ScriptHash "2c698e41831684b16477fb50082b0c0e396d436504e39037d5366582"
    , KeyHashObj $ KeyHash "583a4c8d5f9b3769f98135a2cc041a3118586bd5c74ca72e808af73b"
    )
  ,
    ( ScriptHashObj $ ScriptHash "8fc13431159fdda66347a38c55105d50d77d67abc1c368b876d52ad1"
    , KeyHashObj $ KeyHash "39c898a713b67e7e0ed2345753db246f0d812669445488943a9f851e"
    )
  ,
    ( ScriptHashObj $ ScriptHash "921e1ccb4812c4280510c9ccab81c561f3d413e7d744d48d61215d1f"
    , KeyHashObj $ KeyHash "7cad8428ef51c1eb1f916be74e43ad49fd022482484a770d43b003ea"
    )
  ,
    ( ScriptHashObj $ ScriptHash "d5d09d9380cf9dcde1f3c6cd88b08ca9e00a3d550022ca7ee4026342"
    , KeyHashObj $ KeyHash "ca283340187f1b87e871d903c0178ddfbf4aa896a398787ceba20f98"
    )
  ]

unregisteredCommitteeCreds :: [Credential 'ColdCommitteeRole]
unregisteredCommitteeCreds =
  [ KeyHashObj $ KeyHash "e0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b082541"
  , KeyHashObj $ KeyHash "f15d3cfda3ac52c86d2d98925419795588e74f4e270a3c17beabeaff"
  ]

unregisteredDRepIds :: [Credential 'DRepRole]
unregisteredDRepIds =
  [KeyHashObj $ KeyHash "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4"]

createStakeCredentials :: Int -> [StakeCredential]
createStakeCredentials n =
  fmap (KeyHashObj . KeyHash . mkDummyHash (Proxy @ADDRHASH)) [1 .. n]

createPaymentCredentials :: Int -> [PaymentCredential]
createPaymentCredentials n =
  fmap (KeyHashObj . KeyHash . mkDummyHash (Proxy @ADDRHASH)) [1 .. n]

mkDummyScriptHash :: Int -> ScriptHash
mkDummyScriptHash n = ScriptHash $ mkDummyHash (Proxy @ADDRHASH) n

{-# ANN module ("HLint: ignore Avoid restricted function" :: Text) #-}

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . hashWithSerialiser @h toCBOR

consPoolParams ::
  KeyHash 'StakePool ->
  StakeCredential ->
  [KeyHash 'Staking] ->
  PoolParams
consPoolParams poolId rwCred owners =
  PoolParams
    { ppId = poolId
    , ppVrf = hashVerKeyVRF @StandardCrypto . snd . mkVRFKeyPair $ RawSeed 0 0 0 0 0 -- undefined
    , ppPledge = Coin 1000
    , ppCost = Coin 10000
    , ppMargin = minBound
    , ppRewardAccount = RewardAccount Testnet rwCred
    , ppOwners = Set.fromList owners
    , ppRelays = StrictSeq.singleton $ SingleHostAddr SNothing SNothing SNothing
    , ppMetadata = SJust $ PoolMetadata (fromJust $ textToUrl 64 "best.pool") "89237365492387654983275634298756"
    }

resolveStakePoolVoters ::
  EraCertState era =>
  LedgerState (ShelleyBlock proto era) mk ->
  [Voter]
resolveStakePoolVoters ledger =
  [ StakePoolVoter (resolvePool (PoolIndex 0) ledger)
  , StakePoolVoter (resolvePool (PoolIndex 1) ledger)
  , StakePoolVoter (resolvePool (PoolIndex 2) ledger)
  ]

drepVoters :: [Voter]
drepVoters = map DRepVoter unregisteredDRepIds

committeeVoters :: [Voter]
committeeVoters = map (CommitteeVoter . snd) bootstrapCommitteeCreds
