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
  insertDelegCert,
  insertConwayDelegCert,
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
  insertStakeAddress,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (Cache (..), CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCredDrepHash, insertDrep, insertVotingAnchor)
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember, insertPoolCert)
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Ledger.Conway.TxCert
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.DRep (DRep)
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
  DB.BlockId ->
  DB.TxId ->
  EpochNo ->
  SlotNo ->
  Map Word64 DB.RedeemerId ->
  Generic.TxCertificate ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCertificate syncEnv isMember blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) =
  case cert of
    Left (ShelleyTxCertDelegCert deleg) ->
      when (ioShelley iopts) $ insertDelegCert tracer cache network txId idx mRedeemerId epochNo slotNo deleg
    Left (ShelleyTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert tracer cache isMember network epochNo blkId txId idx pool
    Left (ShelleyTxCertMir mir) ->
      when (ioShelley iopts) $ insertMirCert tracer cache network txId idx mir
    Left (ShelleyTxCertGenesisDeleg _gen) ->
      when (ioShelley iopts) $
        liftIO $
          logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
    Right (ConwayTxCertDeleg deleg) ->
      insertConwayDelegCert syncEnv txId idx mRedeemerId epochNo slotNo deleg
    Right (ConwayTxCertPool pool) ->
      when (ioShelley iopts) $ insertPoolCert tracer cache isMember network epochNo blkId txId idx pool
    Right (ConwayTxCertGov c) ->
      when (ioGov iopts) $ case c of
        ConwayRegDRep cred coin anchor ->
          lift $ insertDrepRegistration txId idx cred (Just coin) (strictMaybeToMaybe anchor)
        ConwayUnRegDRep cred coin ->
          lift $ insertDrepDeRegistration txId idx cred coin
        ConwayAuthCommitteeHotKey khCold khHot ->
          lift $ insertCommitteeRegistration txId idx khCold khHot
        ConwayResignCommitteeColdKey khCold anchor ->
          lift $ insertCommitteeDeRegistration txId idx khCold (strictMaybeToMaybe anchor)
        ConwayUpdateDRep cred anchor ->
          lift $ insertDrepRegistration txId idx cred Nothing (strictMaybeToMaybe anchor)
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv
    mRedeemerId = mlookup ridx redeemers

insertDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ShelleyDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer cache network txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ShelleyRegCert cred -> insertStakeRegistration epochNo txId idx $ Generic.annotateStakingCred network cred
    ShelleyUnRegCert cred -> insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred
    ShelleyDelegCert cred poolkh -> insertDelegation tracer cache network epochNo slotNo txId idx mRedeemerId cred poolkh

insertConwayDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ConwayDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertConwayDelegCert syncEnv txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ConwayRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeRegistration epochNo txId idx $
          Generic.annotateStakingCred network cred
    ConwayUnRegCert cred _dep ->
      when (ioShelley iopts) $
        insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred
    ConwayDelegCert cred delegatee -> insertDeleg cred delegatee
    ConwayRegDelegCert cred delegatee _dep -> do
      when (ioShelley iopts) $
        insertStakeRegistration epochNo txId idx $
          Generic.annotateStakingCred network cred
      insertDeleg cred delegatee
  where
    insertDeleg cred = \case
      DelegStake poolkh ->
        when (ioShelley iopts) $
          insertDelegation trce cache network epochNo slotNo txId idx mRedeemerId cred poolkh
      DelegVote drep ->
        when (ioGov iopts) $
          insertDelegationVote cache network txId idx cred drep
      DelegStakeVote poolkh drep -> do
        when (ioShelley iopts) $
          insertDelegation trce cache network epochNo slotNo txId idx mRedeemerId cred poolkh
        when (ioGov iopts) $
          insertDelegationVote cache network txId idx cred drep

    trce = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv

insertMirCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  MIRCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMirCert _tracer cache network txId idx mcert = do
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
      addrId <- lift $ queryOrInsertStakeAddress cache CacheNew network cred
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
      addrId <- lift $ queryOrInsertStakeAddress cache CacheNew network cred
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
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'DRepRole StandardCrypto ->
  Maybe Coin ->
  Maybe (Anchor StandardCrypto) ->
  ReaderT SqlBackend m ()
insertDrepRegistration txId idx cred mcoin mAnchor = do
  drepId <- insertCredDrepHash cred
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor txId
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
insertCommitteeRegistration txId idx khCold khHot = do
  void
    . DB.insertCommitteeRegistration
    $ DB.CommitteeRegistration
      { DB.committeeRegistrationTxId = txId
      , DB.committeeRegistrationCertIndex = idx
      , DB.committeeRegistrationColdKey = Generic.unCredentialHash khCold
      , DB.committeeRegistrationHotKey = Generic.unCredentialHash khHot
      }

insertCommitteeDeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'ColdCommitteeRole StandardCrypto ->
  Maybe (Anchor StandardCrypto) ->
  ReaderT SqlBackend m ()
insertCommitteeDeRegistration txId idx khCold mAnchor = do
  votingAnchorId <- whenMaybe mAnchor $ insertVotingAnchor txId
  void
    . DB.insertCommitteeDeRegistration
    $ DB.CommitteeDeRegistration
      { DB.committeeDeRegistrationTxId = txId
      , DB.committeeDeRegistrationCertIndex = idx
      , DB.committeeDeRegistrationColdKey = Generic.unCredentialHash khCold
      , DB.committeeDeRegistrationVotingAnchorId = votingAnchorId
      }

insertStakeDeregistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred = do
  scId <- lift $ queryOrInsertStakeAddress cache EvictAndReturn network cred
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
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Shelley.RewardAcnt StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration epochNo txId idx rewardAccount = do
  -- We by-pass the cache here It's likely it won't hit.
  -- We don't store to the cache yet, since there are many addrresses
  -- which are registered and never used.
  saId <- lift $ insertStakeAddress rewardAccount Nothing
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = saId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationEpochNo = unEpochNo epochNo
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
    , DB.adaPotsDeposits = Generic.coinToDbLovelace $ Shelley.depositsAdaPot pots
    , DB.adaPotsFees = Generic.coinToDbLovelace $ Shelley.feesAdaPot pots
    , DB.adaPotsBlockId = blockId
    }

--------------------------------------------------------------------------------------------
-- Insert Delegation
--------------------------------------------------------------------------------------------
insertDelegation ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
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
  addrId <- lift $ queryOrInsertStakeAddress cache CacheNew network cred
  poolHashId <- lift $ queryPoolKeyOrInsert "insertDelegation" trce cache CacheNew True poolkh
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
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  StakeCred ->
  DRep StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegationVote cache network txId idx cred drep = do
  addrId <- lift $ queryOrInsertStakeAddress cache CacheNew network cred
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
