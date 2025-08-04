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
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (
  queryOrInsertRewardAccount,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCommitteeHash, insertCredDrepHash, insertDrep, insertVotingAnchor)
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember, insertPoolCert)
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
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

insertCertificate ::
  SyncEnv ->
  IsPoolMember ->
  Maybe Generic.Deposits ->
  DB.BlockId ->
  DB.TxId ->
  EpochNo ->
  SlotNo ->
  Map Word64 DB.RedeemerId ->
  Generic.TxCertificate ->
  ExceptT SyncNodeError DB.DbM ()
insertCertificate syncEnv isMember mDeposits blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) =
  case cert of
    Left (ShelleyTxCertDelegCert deleg) ->
      when (ioShelley iopts) $ insertDelegCert syncEnv mDeposits network txId idx mRedeemerId epochNo slotNo deleg
    Left (ShelleyTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert syncEnv isMember mDeposits network epochNo blkId txId idx pool
    Left (ShelleyTxCertMir mir) ->
      when (ioShelley iopts) $ insertMirCert syncEnv network txId idx mir
    Left (ShelleyTxCertGenesisDeleg _gen) ->
      when (ioShelley iopts) $
        liftIO $
          logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
    Right (ConwayTxCertDeleg deleg) ->
      insertConwayDelegCert syncEnv mDeposits txId idx mRedeemerId epochNo slotNo deleg
    Right (ConwayTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert syncEnv isMember mDeposits network epochNo blkId txId idx pool
    Right (ConwayTxCertGov c) ->
      when (ioGov iopts) $ case c of
        ConwayRegDRep cred coin anchor ->
          insertDrepRegistration blkId txId idx cred (Just coin) (strictMaybeToMaybe anchor)
        ConwayUnRegDRep cred coin ->
          insertDrepDeRegistration txId idx cred coin
        ConwayAuthCommitteeHotKey khCold khHot ->
          insertCommitteeRegistration txId idx khCold khHot
        ConwayResignCommitteeColdKey khCold anchor ->
          insertCommitteeDeRegistration blkId txId idx khCold (strictMaybeToMaybe anchor)
        ConwayUpdateDRep cred anchor ->
          insertDrepRegistration blkId txId idx cred Nothing (strictMaybeToMaybe anchor)
  where
    tracer = getTrace syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv
    mRedeemerId = mlookup ridx redeemers

insertDelegCert ::
  SyncEnv ->
  Maybe Generic.Deposits ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ShelleyDelegCert ->
  ExceptT SyncNodeError DB.DbM ()
insertDelegCert syncEnv mDeposits network txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ShelleyRegCert cred -> insertStakeRegistration syncEnv epochNo mDeposits txId idx $ Generic.annotateStakingCred network cred
    ShelleyUnRegCert cred -> insertStakeDeregistration syncEnv network epochNo txId idx mRedeemerId cred
    ShelleyDelegCert cred poolkh -> insertDelegation syncEnv network epochNo slotNo txId idx mRedeemerId cred poolkh

insertConwayDelegCert ::
  SyncEnv ->
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ConwayDelegCert ->
  ExceptT SyncNodeError DB.DbM ()
insertConwayDelegCert syncEnv mDeposits txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ConwayRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeRegistration syncEnv epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
    ConwayUnRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeDeregistration syncEnv network epochNo txId idx mRedeemerId cred
    ConwayDelegCert cred delegatee -> insertDeleg cred delegatee
    ConwayRegDelegCert cred delegatee _dep -> do
      when (ioShelley iopts) $
        insertStakeRegistration syncEnv epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
      insertDeleg cred delegatee
  where
    insertDeleg cred = \case
      DelegStake poolkh ->
        when (ioShelley iopts) $
          insertDelegation syncEnv network epochNo slotNo txId idx mRedeemerId cred poolkh
      DelegVote drep ->
        when (ioGov iopts) $
          insertDelegationVote syncEnv network txId idx cred drep
      DelegStakeVote poolkh drep -> do
        when (ioShelley iopts) $
          insertDelegation syncEnv network epochNo slotNo txId idx mRedeemerId cred poolkh
        when (ioGov iopts) $
          insertDelegationVote syncEnv network txId idx cred drep

    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv

insertMirCert ::
  SyncEnv ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  MIRCert ->
  ExceptT SyncNodeError DB.DbM ()
insertMirCert syncEnv network txId idx mcert = do
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
      ExceptT SyncNodeError DB.DbM ()
    insertMirReserves (cred, dcoin) = do
      addrId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong network cred
      void . lift $
        DB.insertReserve $
          DB.Reserve
            { DB.reserveAddrId = addrId
            , DB.reserveCertIndex = idx
            , DB.reserveTxId = txId
            , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
            }

    insertMirTreasury ::
      (StakeCred, Ledger.DeltaCoin) ->
      ExceptT SyncNodeError DB.DbM ()
    insertMirTreasury (cred, dcoin) = do
      addrId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong network cred
      void . lift $
        DB.insertTreasury $
          DB.Treasury
            { DB.treasuryAddrId = addrId
            , DB.treasuryCertIndex = idx
            , DB.treasuryTxId = txId
            , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
            }

    insertPotTransfer ::
      Ledger.DeltaCoin ->
      ExceptT SyncNodeError DB.DbM ()
    insertPotTransfer dcoinTreasury =
      void
        . lift
        $ DB.insertPotTransfer
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
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole ->
  Maybe Coin ->
  Maybe Anchor ->
  ExceptT SyncNodeError DB.DbM ()
insertDrepRegistration blkId txId idx cred mcoin mAnchor = do
  drepId <- insertCredDrepHash cred
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor blkId DB.DrepAnchor
  void
    . lift
    $ DB.insertDrepRegistration
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
  Ledger.Credential 'DRepRole ->
  Coin ->
  ExceptT SyncNodeError DB.DbM ()
insertDrepDeRegistration txId idx cred coin = do
  drepId <- insertCredDrepHash cred
  void
    . lift
    $ DB.insertDrepRegistration
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
  Ledger.Credential 'ColdCommitteeRole ->
  Ledger.Credential 'HotCommitteeRole ->
  ExceptT SyncNodeError DB.DbM ()
insertCommitteeRegistration txId idx khCold cred = do
  khHotId <- insertCommitteeHash cred
  khColdId <- insertCommitteeHash khCold
  void
    . lift
    $ DB.insertCommitteeRegistration
    $ DB.CommitteeRegistration
      { DB.committeeRegistrationTxId = txId
      , DB.committeeRegistrationCertIndex = idx
      , DB.committeeRegistrationColdKeyId = khColdId
      , DB.committeeRegistrationHotKeyId = khHotId
      }

insertCommitteeDeRegistration ::
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole ->
  Maybe Anchor ->
  ExceptT SyncNodeError DB.DbM ()
insertCommitteeDeRegistration blockId txId idx khCold mAnchor = do
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor blockId DB.CommitteeDeRegAnchor
  khColdId <- insertCommitteeHash khCold
  void
    . lift
    $ DB.insertCommitteeDeRegistration
    $ DB.CommitteeDeRegistration
      { DB.committeeDeRegistrationTxId = txId
      , DB.committeeDeRegistrationCertIndex = idx
      , DB.committeeDeRegistrationColdKeyId = khColdId
      , DB.committeeDeRegistrationVotingAnchorId = votingAnchorId
      }

insertStakeDeregistration ::
  SyncEnv ->
  Ledger.Network ->
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  ExceptT SyncNodeError DB.DbM ()
insertStakeDeregistration syncEnv network epochNo txId idx mRedeemerId cred = do
  scId <- queryOrInsertStakeAddress syncEnv EvictAndUpdateCache network cred
  void . lift $
    DB.insertStakeDeregistration $
      DB.StakeDeregistration
        { DB.stakeDeregistrationAddrId = scId
        , DB.stakeDeregistrationCertIndex = idx
        , DB.stakeDeregistrationEpochNo = unEpochNo epochNo
        , DB.stakeDeregistrationTxId = txId
        , DB.stakeDeregistrationRedeemerId = mRedeemerId
        }

insertStakeRegistration ::
  SyncEnv ->
  EpochNo ->
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Shelley.RewardAccount ->
  ExceptT SyncNodeError DB.DbM ()
insertStakeRegistration syncEnv epochNo mDeposits txId idx rewardAccount = do
  saId <- queryOrInsertRewardAccount syncEnv UpdateCache rewardAccount
  void . lift $
    DB.insertStakeRegistration $
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
  ExceptT SyncNodeError DB.DbM ()
insertPots blockId slotNo epochNo pots =
  void $ lift $ DB.insertAdaPots $ mkAdaPots blockId slotNo epochNo pots

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
  SyncEnv ->
  Ledger.Network ->
  EpochNo ->
  SlotNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  Ledger.KeyHash 'Ledger.StakePool ->
  ExceptT SyncNodeError DB.DbM ()
insertDelegation syncEnv network (EpochNo epoch) slotNo txId idx mRedeemerId cred poolkh = do
  addrId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong network cred
  poolHashId <- queryPoolKeyOrInsert syncEnv "insertDelegation" UpdateCache True poolkh
  void . lift $
    DB.insertDelegation $
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
  SyncEnv ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  StakeCred ->
  DRep ->
  ExceptT SyncNodeError DB.DbM ()
insertDelegationVote syncEnv network txId idx cred drep = do
  addrId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong network cred
  drepId <- insertDrep drep
  void
    . lift
    $ DB.insertDelegationVote
    $ DB.DelegationVote
      { DB.delegationVoteAddrId = addrId
      , DB.delegationVoteCertIndex = idx
      , DB.delegationVoteDrepHashId = drepId
      , DB.delegationVoteTxId = txId
      , DB.delegationVoteRedeemerId = Nothing
      }
