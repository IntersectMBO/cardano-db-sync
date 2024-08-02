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

import Cardano.BM.Trace (Trace, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (
  queryOrInsertRewardAccount,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheStatus (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCommitteeHash, insertCredDrepHash, insertDrep, insertVotingAnchor)
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember, insertPoolCert)
import Cardano.DbSync.Error
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
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

insertCertificate ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  IsPoolMember ->
  Maybe Generic.Deposits ->
  DB.BlockId ->
  DB.TxId ->
  EpochNo ->
  SlotNo ->
  Map Word64 DB.RedeemerId ->
  Generic.TxCertificate ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCertificate syncEnv isMember mDeposits blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) =
  case cert of
    Left (ShelleyTxCertDelegCert deleg) ->
      when (ioShelley iopts) $ insertDelegCert tracer cache mDeposits network txId idx mRedeemerId epochNo slotNo deleg
    Left (ShelleyTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert tracer cache isMember mDeposits network epochNo blkId txId idx pool
    Left (ShelleyTxCertMir mir) ->
      when (ioShelley iopts) $ insertMirCert tracer cache network txId idx mir
    Left (ShelleyTxCertGenesisDeleg _gen) ->
      when (ioShelley iopts) $
        liftIO $
          logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
    Right (ConwayTxCertDeleg deleg) ->
      insertConwayDelegCert syncEnv mDeposits txId idx mRedeemerId epochNo slotNo deleg
    Right (ConwayTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert tracer cache isMember mDeposits network epochNo blkId txId idx pool
    Right (ConwayTxCertGov c) ->
      when (ioGov iopts) $ case c of
        ConwayRegDRep cred coin anchor ->
          lift $ insertDrepRegistration blkId txId idx cred (Just coin) (strictMaybeToMaybe anchor)
        ConwayUnRegDRep cred coin ->
          lift $ insertDrepDeRegistration txId idx cred coin
        ConwayAuthCommitteeHotKey khCold khHot ->
          lift $ insertCommitteeRegistration txId idx khCold khHot
        ConwayResignCommitteeColdKey khCold anchor ->
          lift $ insertCommitteeDeRegistration blkId txId idx khCold (strictMaybeToMaybe anchor)
        ConwayUpdateDRep cred anchor ->
          lift $ insertDrepRegistration blkId txId idx cred Nothing (strictMaybeToMaybe anchor)
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv
    mRedeemerId = mlookup ridx redeemers

insertDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Maybe Generic.Deposits ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ShelleyDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer cache mDeposits network txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ShelleyRegCert cred -> insertStakeRegistration tracer cache epochNo mDeposits txId idx $ Generic.annotateStakingCred network cred
    ShelleyUnRegCert cred -> insertStakeDeregistration tracer cache network epochNo txId idx mRedeemerId cred
    ShelleyDelegCert cred poolkh -> insertDelegation tracer cache network epochNo slotNo txId idx mRedeemerId cred poolkh

insertConwayDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ConwayDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertConwayDelegCert syncEnv mDeposits txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ConwayRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeRegistration trce cache epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
    ConwayUnRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeDeregistration trce cache network epochNo txId idx mRedeemerId cred
    ConwayDelegCert cred delegatee -> insertDeleg cred delegatee
    ConwayRegDelegCert cred delegatee _dep -> do
      when (ioShelley iopts) $
        insertStakeRegistration trce cache epochNo mDeposits txId idx $
          Generic.annotateStakingCred network cred
      insertDeleg cred delegatee
  where
    insertDeleg cred = \case
      DelegStake poolkh ->
        when (ioShelley iopts) $
          insertDelegation trce cache network epochNo slotNo txId idx mRedeemerId cred poolkh
      DelegVote drep ->
        when (ioGov iopts) $
          insertDelegationVote trce cache network txId idx cred drep
      DelegStakeVote poolkh drep -> do
        when (ioShelley iopts) $
          insertDelegation trce cache network epochNo slotNo txId idx mRedeemerId cred poolkh
        when (ioGov iopts) $
          insertDelegationVote trce cache network txId idx cred drep

    trce = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv

insertMirCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  MIRCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMirCert tracer cache network txId idx mcert = do
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
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Ledger.DeltaCoin) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirReserves (cred, dcoin) = do
      addrId <- lift $ queryOrInsertStakeAddress tracer cache UpdateCacheStrong network cred
      void . lift . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertMirTreasury ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Ledger.DeltaCoin) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury (cred, dcoin) = do
      addrId <- lift $ queryOrInsertStakeAddress tracer cache UpdateCacheStrong network cred
      void . lift . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertPotTransfer ::
      (MonadBaseControl IO m, MonadIO m) =>
      Ledger.DeltaCoin ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertPotTransfer dcoinTreasury =
      void
        . lift
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
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole StandardCrypto ->
  Maybe Coin ->
  Maybe (Anchor StandardCrypto) ->
  ReaderT SqlBackend m ()
insertDrepRegistration blkId txId idx cred mcoin mAnchor = do
  drepId <- insertCredDrepHash cred
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor blkId DB.DrepAnchor
  void
    . DB.insertDrepRegistration
    $ DB.DrepRegistration
      { DB.drepRegistrationTxId = txId
      , DB.drepRegistrationCertIndex = idx
      , DB.drepRegistrationDeposit = fromIntegral . unCoin <$> mcoin
      , DB.drepRegistrationVotingAnchorId = votingAnchorId
      , DB.drepRegistrationDrepHashId = drepId
      }

insertDrepDeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole StandardCrypto ->
  Coin ->
  ReaderT SqlBackend m ()
insertDrepDeRegistration txId idx cred coin = do
  drepId <- insertCredDrepHash cred
  void
    . DB.insertDrepRegistration
    $ DB.DrepRegistration
      { DB.drepRegistrationTxId = txId
      , DB.drepRegistrationCertIndex = idx
      , DB.drepRegistrationDeposit = Just (-(fromIntegral $ unCoin coin))
      , DB.drepRegistrationVotingAnchorId = Nothing
      , DB.drepRegistrationDrepHashId = drepId
      }

insertCommitteeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole StandardCrypto ->
  Ledger.Credential 'HotCommitteeRole StandardCrypto ->
  ReaderT SqlBackend m ()
insertCommitteeRegistration txId idx khCold cred = do
  khHotId <- insertCommitteeHash cred
  khColdId <- insertCommitteeHash khCold
  void
    . DB.insertCommitteeRegistration
    $ DB.CommitteeRegistration
      { DB.committeeRegistrationTxId = txId
      , DB.committeeRegistrationCertIndex = idx
      , DB.committeeRegistrationColdKeyId = khColdId
      , DB.committeeRegistrationHotKeyId = khHotId
      }

insertCommitteeDeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole StandardCrypto ->
  Maybe (Anchor StandardCrypto) ->
  ReaderT SqlBackend m ()
insertCommitteeDeRegistration blockId txId idx khCold mAnchor = do
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor blockId DB.CommitteeDeRegAnchor
  khColdId <- insertCommitteeHash khCold
  void
    . DB.insertCommitteeDeRegistration
    $ DB.CommitteeDeRegistration
      { DB.committeeDeRegistrationTxId = txId
      , DB.committeeDeRegistrationCertIndex = idx
      , DB.committeeDeRegistrationColdKeyId = khColdId
      , DB.committeeDeRegistrationVotingAnchorId = votingAnchorId
      }

insertStakeDeregistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Ledger.Network ->
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration trce cache network epochNo txId idx mRedeemerId cred = do
  scId <- lift $ queryOrInsertStakeAddress trce cache EvictAndUpdateCache network cred
  void . lift . DB.insertStakeDeregistration $
    DB.StakeDeregistration
      { DB.stakeDeregistrationAddrId = scId
      , DB.stakeDeregistrationCertIndex = idx
      , DB.stakeDeregistrationEpochNo = unEpochNo epochNo
      , DB.stakeDeregistrationTxId = txId
      , DB.stakeDeregistrationRedeemerId = mRedeemerId
      }

insertStakeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  EpochNo ->
  Maybe Generic.Deposits ->
  DB.TxId ->
  Word16 ->
  Shelley.RewardAccount StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration tracer cache epochNo mDeposits txId idx rewardAccount = do
  saId <- lift $ queryOrInsertRewardAccount tracer cache UpdateCache rewardAccount
  void . lift . DB.insertStakeRegistration $
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
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Shelley.AdaPots ->
  ExceptT e (ReaderT SqlBackend m) ()
insertPots blockId slotNo epochNo pots =
  void
    . lift
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
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Ledger.Network ->
  EpochNo ->
  SlotNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  Ledger.KeyHash 'Ledger.StakePool StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegation trce cache network (EpochNo epoch) slotNo txId idx mRedeemerId cred poolkh = do
  addrId <- lift $ queryOrInsertStakeAddress trce cache UpdateCacheStrong network cred
  poolHashId <- lift $ queryPoolKeyOrInsert "insertDelegation" trce cache UpdateCache True poolkh
  void . lift . DB.insertDelegation $
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
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  StakeCred ->
  DRep StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegationVote trce cache network txId idx cred drep = do
  addrId <- lift $ queryOrInsertStakeAddress trce cache UpdateCacheStrong network cred
  drepId <- lift $ insertDrep drep
  void
    . lift
    . DB.insertDelegationVote
    $ DB.DelegationVote
      { DB.delegationVoteAddrId = addrId
      , DB.delegationVoteCertIndex = idx
      , DB.delegationVoteDrepHashId = drepId
      , DB.delegationVoteTxId = txId
      , DB.delegationVoteRedeemerId = Nothing
      }
