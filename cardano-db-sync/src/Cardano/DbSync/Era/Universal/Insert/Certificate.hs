{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Certificate (
  insertCertificate,
  insertMirCert,
  insertDrepRegistration,
  insertDrepDeRegistration,
  insertCommitteeRegistration,
  insertCommitteeDeRegistration,
  insertDelegation,
  insertDelegationVote,
  insertStakeDeregistration,
  insertStakeRegistration,
  insertPots,
  mkAdaPots,
) where

import Cardano.BM.Trace (logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, InsertOptions (..), MonadAppDB (..), SyncEnv (..), askInsertOptions, askNetwork, askTrace)
import Cardano.DbSync.Cache (
  insertStakeAddress,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (UpdateCache (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCommitteeHash, insertCredDrepHash, insertDrep, insertVotingAnchor)
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember, insertPoolCert)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Ledger.Conway.TxCert
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Keys
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.AdaPots as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import Cardano.Prelude
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

insertCertificate ::
  IsPoolMember ->
  Maybe Generic.Deposits ->
  DB.BlockId ->
  DB.TxId ->
  EpochNo ->
  SlotNo ->
  Map Word64 DB.RedeemerId ->
  Generic.TxCertificate ->
  App ()
insertCertificate isMember mDeposits blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) = do
  InsertOptions {ioShelley = isShelley, ioGov = isGov} <- askInsertOptions
  tracer <- askTrace
  case cert of
    Left (ShelleyTxCertDelegCert deleg) ->
      when isShelley $ insertDelegCert mDeposits txId idx mRedeemerId epochNo slotNo deleg
    Left (ShelleyTxCertPool pool) ->
      when isShelley $ insertPoolCert isMember mDeposits epochNo blkId txId idx pool
    Left (ShelleyTxCertMir mir) ->
      when isShelley $ insertMirCert txId idx mir
    Left (ShelleyTxCertGenesisDeleg _gen) ->
      when isShelley $
        liftIO $
          logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
    Right (ConwayTxCertDeleg deleg) ->
      insertConwayDelegCert mDeposits txId idx mRedeemerId epochNo slotNo deleg
    Right (ConwayTxCertPool pool) ->
      when isShelley $ insertPoolCert isMember mDeposits epochNo blkId txId idx pool
    Right (ConwayTxCertGov c) ->
      when isGov $ case c of
        ConwayRegDRep cred coin anchor ->
          insertDrepRegistration txId idx cred (Just coin) (strictMaybeToMaybe anchor)
        ConwayUnRegDRep cred coin ->
          insertDrepDeRegistration txId idx cred coin
        ConwayAuthCommitteeHotKey khCold khHot ->
          insertCommitteeRegistration txId idx khCold khHot
        ConwayResignCommitteeColdKey khCold anchor ->
          insertCommitteeDeRegistration txId idx khCold (strictMaybeToMaybe anchor)
        ConwayUpdateDRep cred anchor ->
          insertDrepRegistration txId idx cred Nothing (strictMaybeToMaybe anchor)
  where
    mRedeemerId = mlookup ridx redeemers

insertDelegCert ::
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ShelleyDelegCert StandardCrypto ->
  App ()
insertDelegCert mDeposits txId idx mRedeemerId epochNo slotNo dCert = do
  network <- askNetwork
  case dCert of
    ShelleyRegCert cred -> insertStakeRegistration epochNo mDeposits txId idx $ Generic.annotateStakingCred network cred
    ShelleyUnRegCert cred -> insertStakeDeregistration epochNo txId idx mRedeemerId cred
    ShelleyDelegCert cred poolkh -> insertDelegation epochNo slotNo txId idx mRedeemerId cred poolkh

insertConwayDelegCert ::
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ConwayDelegCert StandardCrypto ->
  App ()
insertConwayDelegCert mDeposits txId idx mRedeemerId epochNo slotNo dCert = do
  InsertOptions {ioShelley = isShelley, ioGov = isGov} <- askInsertOptions
  network <- askNetwork
  let insertDeleg cred = \case
        DelegStake poolkh ->
          when isShelley $
            insertDelegation epochNo slotNo txId idx mRedeemerId cred poolkh
        DelegVote drep ->
          when isGov $
            insertDelegationVote txId idx cred drep
        DelegStakeVote poolkh drep -> do
          when isShelley $
            insertDelegation epochNo slotNo txId idx mRedeemerId cred poolkh
          when isGov $
            insertDelegationVote txId idx cred drep
  case dCert of
    ConwayRegCert cred _dep ->
      when isShelley $
        insertStakeRegistration epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
    ConwayUnRegCert cred _dep ->
      when isShelley $
        insertStakeDeregistration epochNo txId idx mRedeemerId cred
    ConwayDelegCert cred delegatee -> insertDeleg cred delegatee
    ConwayRegDelegCert cred delegatee _dep -> do
      when isShelley $
        insertStakeRegistration epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
      insertDeleg cred delegatee

insertMirCert ::
  DB.TxId ->
  Word16 ->
  MIRCert StandardCrypto ->
  App ()
insertMirCert txId idx mcert = do
  case mirPot mcert of
    ReservesMIR ->
      case mirRewards mcert of
        StakeAddressesMIR rwds -> mapM_ insertMirReserves $ Map.toList rwds
        SendToOppositePotMIR xfrs -> insertPotTransfer (Ledger.toDeltaCoin xfrs)
    TreasuryMIR -> do
      case mirRewards mcert of
        StakeAddressesMIR rwds -> mapM_ insertMirTreasury $ Map.toList rwds
        SendToOppositePotMIR xfrs -> insertPotTransfer (invert $ Ledger.toDeltaCoin xfrs)
  where
    insertMirReserves ::
      (StakeCred, Ledger.DeltaCoin) ->
      App ()
    insertMirReserves (cred, dcoin) = do
      network <- askNetwork
      cache <- asks envCache
      addrId <- queryOrInsertStakeAddress cache UpdateCache network cred
      void . dbQueryToApp . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertMirTreasury ::
      (StakeCred, Ledger.DeltaCoin) ->
      App ()
    insertMirTreasury (cred, dcoin) = do
      network <- askNetwork
      cache <- asks envCache
      addrId <- queryOrInsertStakeAddress cache UpdateCache network cred
      void . dbQueryToApp . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertPotTransfer ::
      Ledger.DeltaCoin ->
      App ()
    insertPotTransfer dcoinTreasury =
      void
        . dbQueryToApp
        . DB.insertPotTransfer
        $ DB.PotTransfer
          { DB.potTransferCertIndex = idx
          , DB.potTransferTreasury = DB.deltaCoinToDbInt65 dcoinTreasury
          , DB.potTransferReserves = DB.deltaCoinToDbInt65 (invert dcoinTreasury)
          , DB.potTransferTxId = txId
          }

--------------------------------------------------------------------------------------------
-- Insert Registration
--------------------------------------------------------------------------------------------
insertDrepRegistration ::
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole StandardCrypto ->
  Maybe Coin ->
  Maybe (Anchor StandardCrypto) ->
  App ()
insertDrepRegistration txId idx cred mcoin mAnchor = do
  drepId <- insertCredDrepHash cred
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor blkId DB.DrepAnchor
  void
    . dbQueryToApp
    . DB.insertDrepRegistration
    $ DB.DrepRegistration
      { DB.drepRegistrationTxId = txId
      , DB.drepRegistrationCertIndex = idx
      , DB.drepRegistrationDeposit = fromIntegral . unCoin <$> mcoin
      , DB.drepRegistrationVotingAnchorId = votingAnchorId
      , DB.drepRegistrationDrepHashId = drepId
      }

insertDrepDeRegistration ::
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole StandardCrypto ->
  Coin ->
  App ()
insertDrepDeRegistration txId idx cred coin = do
  drepId <- insertCredDrepHash cred
  void
    . dbQueryToApp
    . DB.insertDrepRegistration
    $ DB.DrepRegistration
      { DB.drepRegistrationTxId = txId
      , DB.drepRegistrationCertIndex = idx
      , DB.drepRegistrationDeposit = Just (-(fromIntegral $ unCoin coin))
      , DB.drepRegistrationVotingAnchorId = Nothing
      , DB.drepRegistrationDrepHashId = drepId
      }

insertCommitteeRegistration ::
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole StandardCrypto ->
  Ledger.Credential 'HotCommitteeRole StandardCrypto ->
  App ()
insertCommitteeRegistration txId idx khCold cred = do
  khHotId <- insertCommitteeHash cred
  khColdId <- insertCommitteeHash khCold
  void
    . dbQueryToApp
    . DB.insertCommitteeRegistration
    $ DB.CommitteeRegistration
      { DB.committeeRegistrationTxId = txId
      , DB.committeeRegistrationCertIndex = idx
      , DB.committeeRegistrationColdKeyId = khColdId
      , DB.committeeRegistrationHotKeyId = khHotId
      }

insertCommitteeDeRegistration ::
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole StandardCrypto ->
  Maybe (Anchor StandardCrypto) ->
  App ()
insertCommitteeDeRegistration txId idx khCold mAnchor = do
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor txId DB.OtherAnchor
  khColdId <- insertCommitteeHash khCold
  void
    . dbQueryToApp
    . DB.insertCommitteeDeRegistration
    $ DB.CommitteeDeRegistration
      { DB.committeeDeRegistrationTxId = txId
      , DB.committeeDeRegistrationCertIndex = idx
      , DB.committeeDeRegistrationColdKeyId = khColdId
      , DB.committeeDeRegistrationVotingAnchorId = votingAnchorId
      }

insertStakeDeregistration ::
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  App ()
insertStakeDeregistration epochNo txId idx mRedeemerId cred = do
  network <- askNetwork
  cache <- asks envCache
  scId <- queryOrInsertStakeAddress cache EvictAndReturn network cred
  void
    . dbQueryToApp
    . DB.insertStakeDeregistration
    $ DB.StakeDeregistration
      { DB.stakeDeregistrationAddrId = scId
      , DB.stakeDeregistrationCertIndex = idx
      , DB.stakeDeregistrationEpochNo = unEpochNo epochNo
      , DB.stakeDeregistrationTxId = txId
      , DB.stakeDeregistrationRedeemerId = mRedeemerId
      }

insertStakeRegistration ::
  EpochNo ->
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Shelley.RewardAccount StandardCrypto ->
  App ()
insertStakeRegistration epochNo mDeposits txId idx rewardAccount = do
  -- We by-pass the cache here It's likely it won't hit.
  -- We don't store to the cache yet, since there are many addrresses
  -- which are registered and never used.
  saId <- insertStakeAddress rewardAccount Nothing
  void . dbQueryToApp . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = saId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationEpochNo = unEpochNo epochNo
      , DB.stakeRegistrationDeposit = Generic.coinToDbLovelace . Generic.stakeKeyDeposit <$> mDeposits
      , DB.stakeRegistrationTxId = txId
      }

--------------------------------------------------------------------------------------------
-- Insert Pots
--------------------------------------------------------------------------------------------
insertPots ::
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Shelley.AdaPots ->
  App ()
insertPots blockId slotNo epochNo pots =
  void
    . dbQueryToApp
    $ DB.insertAdaPots
    $ mkAdaPots blockId slotNo epochNo pots

mkAdaPots ::
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Shelley.AdaPots ->
  DB.AdaPots
mkAdaPots blockId slotNo epochNo pots =
  DB.AdaPots
    { DB.adaPotsSlotNo = unSlotNo slotNo
    , DB.adaPotsEpochNo = unEpochNo epochNo
    , DB.adaPotsTreasury = Generic.coinToDbLovelace $ Shelley.treasuryAdaPot pots
    , DB.adaPotsReserves = Generic.coinToDbLovelace $ Shelley.reservesAdaPot pots
    , DB.adaPotsRewards = Generic.coinToDbLovelace $ Shelley.rewardsAdaPot pots
    , DB.adaPotsUtxo = Generic.coinToDbLovelace $ Shelley.utxoAdaPot pots
    , DB.adaPotsDepositsStake = DB.DbLovelace $ fromIntegral $ unCoin (oblStake oblgs) + unCoin (oblPool oblgs)
    , DB.adaPotsDepositsDrep = Generic.coinToDbLovelace $ oblDRep oblgs
    , DB.adaPotsDepositsProposal = Generic.coinToDbLovelace $ oblProposal oblgs
    , DB.adaPotsFees = Generic.coinToDbLovelace $ Shelley.feesAdaPot pots
    , DB.adaPotsBlockId = blockId
    }
  where
    oblgs = Shelley.obligationsPot pots

--------------------------------------------------------------------------------------------
-- Insert Delegation
--------------------------------------------------------------------------------------------
insertDelegation ::
  EpochNo ->
  SlotNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  Ledger.KeyHash 'Ledger.StakePool StandardCrypto ->
  App ()
insertDelegation (EpochNo epoch) slotNo txId idx mRedeemerId cred poolkh = do
  network <- askNetwork
  cache <- asks envCache
  addrId <- queryOrInsertStakeAddress cache UpdateCache network cred
  poolHashId <- queryPoolKeyOrInsert "insertDelegation" cache UpdateCache True poolkh
  void . dbQueryToApp . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationCertIndex = idx
      , DB.delegationPoolHashId = poolHashId
      , DB.delegationActiveEpochNo = epoch + 2 -- The first epoch where this delegation is valid.
      , DB.delegationTxId = txId
      , DB.delegationSlotNo = unSlotNo slotNo
      , DB.delegationRedeemerId = mRedeemerId
      }

insertDelegationVote ::
  DB.TxId ->
  Word16 ->
  StakeCred ->
  DRep StandardCrypto ->
  App ()
insertDelegationVote txId idx cred drep = do
  network <- askNetwork
  cache <- asks envCache
  addrId <- queryOrInsertStakeAddress cache UpdateCache network cred
  drepId <- insertDrep drep
  void
    . dbQueryToApp
    . DB.insertDelegationVote
    $ DB.DelegationVote
      { DB.delegationVoteAddrId = addrId
      , DB.delegationVoteCertIndex = idx
      , DB.delegationVoteDrepHashId = drepId
      , DB.delegationVoteTxId = txId
      , DB.delegationVoteRedeemerId = Nothing
      }
