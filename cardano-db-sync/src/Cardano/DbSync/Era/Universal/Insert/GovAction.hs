{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.GovAction (
  insertConstitution,
  insertCostModel,
  insertCredDrepHash,
  insertDrep,
  insertDrepDistr,
  insertGovActionProposal,
  insertParamProposal,
  insertVotingProcedures,
  insertCommitteeHash,
  insertCommittee,
  insertVotingAnchor,
  resolveGovActionProposal,
  updateRatified,
  updateExpired,
  insertUpdateEnacted,
  updateDropped,
)
where

import Cardano.BM.Trace (logWarning)
import qualified Cardano.Crypto as Crypto
import Cardano.Db (DbWord64 (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (queryOrInsertRewardAccount, queryPoolKeyOrInsert, queryTxIdWithCache)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import Cardano.DbSync.Config.Types (ShelleyInsertConfig (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Bech32 (serialiseDrepToBech32)
import Cardano.DbSync.Util.Whitelist (shelleyStakeAddrWhitelistCheck)
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.CertState (DRep (..))
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core (DRepVotingThresholds (..), Era (..), PoolVotingThresholds (..))
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.DRep (DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Plutus.CostModels as Ledger
import Cardano.Ledger.Plutus.Language (Language)
import Cardano.Ledger.Shelley.API (Coin (..))
import Cardano.Prelude
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardConway, StandardCrypto)
import Prelude (zip3)

insertGovActionProposal ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  Maybe EpochNo ->
  Maybe (ConwayGovState StandardConway) ->
  (Word64, (GovActionId StandardCrypto, ProposalProcedure StandardConway)) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertGovActionProposal syncEnv blkId txId govExpiresAt mcgs (index, (govId, pp)) = do
  -- check if shelley stake address is in the whitelist
  when (shelleyStakeAddrWhitelistCheck syncEnv $ pProcReturnAddr pp) $ do
    addrId <- lift $ queryOrInsertRewardAccount syncEnv cache UpdateCache $ pProcReturnAddr pp
    votingAnchorId <- lift $ insertVotingAnchor blkId DB.GovActionAnchor $ pProcAnchor pp
    mParamProposalId <- lift $
      case pProcGovAction pp of
        ParameterChange _ pparams _ ->
          Just <$> insertParamProposal blkId txId (convertConwayParamProposal pparams)
        _ -> pure Nothing
    prevGovActionDBId <- case mprevGovAction of
      Nothing -> pure Nothing
      Just prevGovActionId -> resolveGovActionProposal syncEnv prevGovActionId
    govActionProposalId <-
      lift $
        DB.insertGovActionProposal $
          DB.GovActionProposal
            { DB.govActionProposalTxId = txId
            , DB.govActionProposalIndex = index
            , DB.govActionProposalPrevGovActionProposal = prevGovActionDBId
            , DB.govActionProposalDeposit = Generic.coinToDbLovelace $ pProcDeposit pp
            , DB.govActionProposalReturnAddress = addrId
            , DB.govActionProposalExpiration = unEpochNo <$> govExpiresAt
            , DB.govActionProposalVotingAnchorId = Just votingAnchorId
            , DB.govActionProposalType = Generic.toGovAction $ pProcGovAction pp
            , DB.govActionProposalDescription = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode (pProcGovAction pp)
            , DB.govActionProposalParamProposal = mParamProposalId
            , DB.govActionProposalRatifiedEpoch = Nothing
            , DB.govActionProposalEnactedEpoch = Nothing
            , DB.govActionProposalDroppedEpoch = Nothing
            , DB.govActionProposalExpiredEpoch = Nothing
            }
    case pProcGovAction pp of
      TreasuryWithdrawals mp _ -> lift $ mapM_ (insertTreasuryWithdrawal govActionProposalId) (Map.toList mp)
      UpdateCommittee {} -> lift $ insertNewCommittee govActionProposalId
      NewConstitution _ constitution -> void $ lift $ insertConstitution blkId (Just govActionProposalId) constitution
      _ -> pure ()
  where
    cache = envCache syncEnv
    mprevGovAction :: Maybe (GovActionId StandardCrypto) = case pProcGovAction pp of
      ParameterChange prv _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      HardForkInitiation prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NoConfidence prv -> unGovPurposeId <$> strictMaybeToMaybe prv
      UpdateCommittee prv _ _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NewConstitution prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      _ -> Nothing

    insertTreasuryWithdrawal ::
      DB.GovActionProposalId ->
      (Ledger.RewardAccount StandardCrypto, Coin) ->
      ReaderT SqlBackend m DB.TreasuryWithdrawalId
    insertTreasuryWithdrawal gaId (rwdAcc, coin) = do
      addrId <-
        queryOrInsertRewardAccount syncEnv cache UpdateCache rwdAcc
      DB.insertTreasuryWithdrawal $
        DB.TreasuryWithdrawal
          { DB.treasuryWithdrawalGovActionProposalId = gaId
          , DB.treasuryWithdrawalStakeAddressId = addrId
          , DB.treasuryWithdrawalAmount = Generic.coinToDbLovelace coin
          }

    insertNewCommittee ::
      DB.GovActionProposalId ->
      ReaderT SqlBackend m ()
    insertNewCommittee govActionProposalId = do
      whenJust mcgs $ \cgs ->
        case findProposedCommittee govId cgs of
          Right (Just committee) -> void $ insertCommittee (Just govActionProposalId) committee
          other ->
            liftIO $ logWarning (getTrace syncEnv) $ textShow other <> ": Failed to find committee for " <> textShow pp

insertCommittee :: (MonadIO m, MonadBaseControl IO m) => Maybe DB.GovActionProposalId -> Committee StandardConway -> ReaderT SqlBackend m DB.CommitteeId
insertCommittee mgapId committee = do
  committeeId <- insertCommitteeDB
  mapM_ (insertNewMember committeeId) (Map.toList $ committeeMembers committee)
  pure committeeId
  where
    r = unboundRational $ committeeThreshold committee -- TODO work directly with Ratio Word64. This is not currently supported in ledger
    insertNewMember committeeId (cred, e) = do
      chId <- insertCommitteeHash cred
      void . DB.insertCommitteeMember $
        DB.CommitteeMember
          { DB.committeeMemberCommitteeId = committeeId
          , DB.committeeMemberCommitteeHashId = chId
          , DB.committeeMemberExpirationEpoch = unEpochNo e
          }

    insertCommitteeDB =
      DB.insertCommittee $
        DB.Committee
          { DB.committeeGovActionProposalId = mgapId
          , DB.committeeQuorumNumerator = fromIntegral $ numerator r
          , DB.committeeQuorumDenominator = fromIntegral $ denominator r
          }

--------------------------------------------------------------------------------------
-- PROPOSAL
--------------------------------------------------------------------------------------
resolveGovActionProposal ::
  MonadIO m =>
  SyncEnv ->
  GovActionId StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.GovActionProposalId)
resolveGovActionProposal syncEnv gaId = do
  let txId = gaidTxId gaId
  gaTxId <- liftLookupFail "resolveGovActionProposal.queryTxId" $ queryTxIdWithCache (envCache syncEnv) txId
  let (GovActionIx index) = gaidGovActionIx gaId
  case ioShelley insertOpts of
    -- if we have whitelist for stake addresses then don't input the proposal
    ShelleyStakeAddrs _ -> pure Nothing
    _ -> do
      result <-
        liftLookupFail "resolveGovActionProposal.queryGovActionProposalId" $
          DB.queryGovActionProposalId gaTxId (fromIntegral index) -- TODO: Use Word32?
      pure $ Just result
  where
    insertOpts = soptInsertOptions $ envOptions syncEnv

insertParamProposal ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.TxId ->
  ParamProposal ->
  ReaderT SqlBackend m DB.ParamProposalId
insertParamProposal blkId txId pp = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (pppCostmdls pp)
  DB.insertParamProposal $
    DB.ParamProposal
      { DB.paramProposalRegisteredTxId = txId
      , DB.paramProposalEpochNo = unEpochNo <$> pppEpochNo pp
      , DB.paramProposalKey = pppKey pp
      , DB.paramProposalMinFeeA = fromIntegral <$> pppMinFeeA pp
      , DB.paramProposalMinFeeB = fromIntegral <$> pppMinFeeB pp
      , DB.paramProposalMaxBlockSize = fromIntegral <$> pppMaxBlockSize pp
      , DB.paramProposalMaxTxSize = fromIntegral <$> pppMaxTxSize pp
      , DB.paramProposalMaxBhSize = fromIntegral <$> pppMaxBhSize pp
      , DB.paramProposalKeyDeposit = Generic.coinToDbLovelace <$> pppKeyDeposit pp
      , DB.paramProposalPoolDeposit = Generic.coinToDbLovelace <$> pppPoolDeposit pp
      , DB.paramProposalMaxEpoch = fromIntegral . unEpochInterval <$> pppMaxEpoch pp
      , DB.paramProposalOptimalPoolCount = fromIntegral <$> pppOptimalPoolCount pp
      , DB.paramProposalInfluence = fromRational <$> pppInfluence pp
      , DB.paramProposalMonetaryExpandRate = toDouble <$> pppMonetaryExpandRate pp
      , DB.paramProposalTreasuryGrowthRate = toDouble <$> pppTreasuryGrowthRate pp
      , DB.paramProposalDecentralisation = toDouble <$> pppDecentralisation pp
      , DB.paramProposalEntropy = Generic.nonceToBytes =<< pppEntropy pp
      , DB.paramProposalProtocolMajor = getVersion . Ledger.pvMajor <$> pppProtocolVersion pp
      , DB.paramProposalProtocolMinor = fromIntegral . Ledger.pvMinor <$> pppProtocolVersion pp
      , DB.paramProposalMinUtxoValue = Generic.coinToDbLovelace <$> pppMinUtxoValue pp
      , DB.paramProposalMinPoolCost = Generic.coinToDbLovelace <$> pppMinPoolCost pp
      , -- New for Alonzo
        DB.paramProposalCoinsPerUtxoSize = Generic.coinToDbLovelace <$> pppCoinsPerUtxo pp
      , DB.paramProposalCostModelId = cmId
      , DB.paramProposalPriceMem = realToFrac <$> pppPriceMem pp
      , DB.paramProposalPriceStep = realToFrac <$> pppPriceStep pp
      , DB.paramProposalMaxTxExMem = DbWord64 <$> pppMaxTxExMem pp
      , DB.paramProposalMaxTxExSteps = DbWord64 <$> pppMaxTxExSteps pp
      , DB.paramProposalMaxBlockExMem = DbWord64 <$> pppMaxBlockExMem pp
      , DB.paramProposalMaxBlockExSteps = DbWord64 <$> pppMaxBlockExSteps pp
      , DB.paramProposalMaxValSize = DbWord64 . fromIntegral <$> pppMaxValSize pp
      , DB.paramProposalCollateralPercent = fromIntegral <$> pppCollateralPercentage pp
      , DB.paramProposalMaxCollateralInputs = fromIntegral <$> pppMaxCollateralInputs pp
      , -- New for Conway
        DB.paramProposalPvtMotionNoConfidence = toDouble . pvtMotionNoConfidence <$> pppPoolVotingThresholds pp
      , DB.paramProposalPvtCommitteeNormal = toDouble . pvtCommitteeNormal <$> pppPoolVotingThresholds pp
      , DB.paramProposalPvtCommitteeNoConfidence = toDouble . pvtCommitteeNoConfidence <$> pppPoolVotingThresholds pp
      , DB.paramProposalPvtHardForkInitiation = toDouble . pvtHardForkInitiation <$> pppPoolVotingThresholds pp
      , DB.paramProposalPvtppSecurityGroup = toDouble . pvtPPSecurityGroup <$> pppPoolVotingThresholds pp
      , DB.paramProposalDvtMotionNoConfidence = toDouble . dvtMotionNoConfidence <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtCommitteeNormal = toDouble . dvtCommitteeNormal <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtCommitteeNoConfidence = toDouble . dvtCommitteeNoConfidence <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtUpdateToConstitution = toDouble . dvtUpdateToConstitution <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtHardForkInitiation = toDouble . dvtHardForkInitiation <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtPPNetworkGroup = toDouble . dvtPPNetworkGroup <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtPPEconomicGroup = toDouble . dvtPPEconomicGroup <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtPPTechnicalGroup = toDouble . dvtPPTechnicalGroup <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtPPGovGroup = toDouble . dvtPPGovGroup <$> pppDRepVotingThresholds pp
      , DB.paramProposalDvtTreasuryWithdrawal = toDouble . dvtTreasuryWithdrawal <$> pppDRepVotingThresholds pp
      , DB.paramProposalCommitteeMinSize = DbWord64 . fromIntegral <$> pppCommitteeMinSize pp
      , DB.paramProposalCommitteeMaxTermLength = DbWord64 . fromIntegral . unEpochInterval <$> pppCommitteeMaxTermLength pp
      , DB.paramProposalGovActionLifetime = fromIntegral . unEpochInterval <$> pppGovActionLifetime pp
      , DB.paramProposalGovActionDeposit = DbWord64 . fromIntegral <$> pppGovActionDeposit pp
      , DB.paramProposalDrepDeposit = DbWord64 . fromIntegral <$> pppDRepDeposit pp
      , DB.paramProposalDrepActivity = fromIntegral . unEpochInterval <$> pppDRepActivity pp
      , DB.paramProposalMinFeeRefScriptCostPerByte = fromRational <$> pppMinFeeRefScriptCostPerByte pp
      }

insertConstitution :: (MonadIO m, MonadBaseControl IO m) => DB.BlockId -> Maybe DB.GovActionProposalId -> Constitution StandardConway -> ReaderT SqlBackend m DB.ConstitutionId
insertConstitution blockId mgapId constitution = do
  votingAnchorId <- insertVotingAnchor blockId DB.ConstitutionAnchor $ constitutionAnchor constitution
  DB.insertConstitution $
    DB.Constitution
      { DB.constitutionGovActionProposalId = mgapId
      , DB.constitutionVotingAnchorId = votingAnchorId
      , DB.constitutionScriptHash = Generic.unScriptHash <$> strictMaybeToMaybe (constitutionScript constitution)
      }

--------------------------------------------------------------------------------------
-- VOTING PROCEDURES
--------------------------------------------------------------------------------------
insertVotingProcedures ::
  (MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  [(GovActionId StandardCrypto, ProposalProcedure StandardConway)] ->
  (Voter StandardCrypto, [(GovActionId StandardCrypto, VotingProcedure StandardConway)]) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertVotingProcedures syncEnv blkId txId proposalPs (voter, actions) =
  -- TODO: cmdv will actions & proposalPs always be the same length?
  mapM_ (insertVotingProcedure syncEnv blkId txId voter) (zip3 [0 ..] actions (map snd proposalPs))

insertVotingProcedure ::
  (MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  Voter StandardCrypto ->
  (Word16, (GovActionId StandardCrypto, VotingProcedure StandardConway), ProposalProcedure StandardConway) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertVotingProcedure syncEnv blkId txId voter (index, (gaId, vp), proposalP) = do
  -- check if shelley stake address is in the whitelist
  when (shelleyStakeAddrWhitelistCheck syncEnv $ pProcReturnAddr proposalP) $ do
    maybeGovActionId <- resolveGovActionProposal syncEnv gaId
    case maybeGovActionId of
      Nothing -> pure ()
      Just govActionId -> do
        votingAnchorId <- whenMaybe (strictMaybeToMaybe $ vProcAnchor vp) $ lift . insertVotingAnchor blkId DB.OtherAnchor
        (mCommitteeVoterId, mDRepVoter, mStakePoolVoter) <- case voter of
          CommitteeVoter cred -> do
            khId <- lift $ insertCommitteeHash cred
            pure (Just khId, Nothing, Nothing)
          DRepVoter cred -> do
            drep <- lift $ insertCredDrepHash cred
            pure (Nothing, Just drep, Nothing)
          StakePoolVoter poolkh -> do
            poolHashId <- lift $ queryPoolKeyOrInsert "insertVotingProcedure" syncEnv (envCache syncEnv) UpdateCache False poolkh
            pure (Nothing, Nothing, Just poolHashId)
        void
          . lift
          . DB.insertVotingProcedure
          $ DB.VotingProcedure
            { DB.votingProcedureTxId = txId
            , DB.votingProcedureIndex = index
            , DB.votingProcedureGovActionProposalId = govActionId
            , DB.votingProcedureCommitteeVoter = mCommitteeVoterId
            , DB.votingProcedureDrepVoter = mDRepVoter
            , DB.votingProcedurePoolVoter = mStakePoolVoter
            , DB.votingProcedureVoterRole = Generic.toVoterRole voter
            , DB.votingProcedureVote = Generic.toVote $ vProcVote vp
            , DB.votingProcedureVotingAnchorId = votingAnchorId
            , DB.votingProcedureInvalid = Nothing
            }

insertVotingAnchor :: (MonadIO m, MonadBaseControl IO m) => DB.BlockId -> DB.AnchorType -> Anchor StandardCrypto -> ReaderT SqlBackend m DB.VotingAnchorId
insertVotingAnchor blockId anchorType anchor =
  DB.insertAnchor $
    DB.VotingAnchor
      { DB.votingAnchorBlockId = blockId
      , DB.votingAnchorUrl = DB.VoteUrl $ Ledger.urlToText $ anchorUrl anchor -- TODO: Conway check unicode and size of URL
      , DB.votingAnchorDataHash = Generic.safeHashToByteString $ anchorDataHash anchor
      , DB.votingAnchorType = anchorType
      }

insertCommitteeHash :: (MonadBaseControl IO m, MonadIO m) => Ledger.Credential kr StandardCrypto -> ReaderT SqlBackend m DB.CommitteeHashId
insertCommitteeHash cred = do
  DB.insertCommitteeHash
    DB.CommitteeHash
      { DB.committeeHashRaw = Generic.unCredentialHash cred
      , DB.committeeHashHasScript = Generic.hasCredScript cred
      }

--------------------------------------------------------------------------------------
-- DREP
--------------------------------------------------------------------------------------
insertDrep :: (MonadBaseControl IO m, MonadIO m) => DRep StandardCrypto -> ReaderT SqlBackend m DB.DrepHashId
insertDrep = \case
  DRepCredential cred -> insertCredDrepHash cred
  DRepAlwaysAbstain -> DB.insertAlwaysAbstainDrep
  DRepAlwaysNoConfidence -> DB.insertAlwaysNoConfidence

insertCredDrepHash :: (MonadBaseControl IO m, MonadIO m) => Ledger.Credential 'DRepRole StandardCrypto -> ReaderT SqlBackend m DB.DrepHashId
insertCredDrepHash cred = do
  DB.insertDrepHash
    DB.DrepHash
      { DB.drepHashRaw = Just bs
      , DB.drepHashView = serialiseDrepToBech32 bs
      , DB.drepHashHasScript = Generic.hasCredScript cred
      }
  where
    bs = Generic.unCredentialHash cred

insertDrepDistr :: forall m. (MonadBaseControl IO m, MonadIO m) => EpochNo -> PulsingSnapshot StandardConway -> ReaderT SqlBackend m ()
insertDrepDistr e pSnapshot = do
  drepsDB <- mapM mkEntry (Map.toList $ psDRepDistr pSnapshot)
  DB.insertManyDrepDistr drepsDB
  where
    mkEntry :: (DRep StandardCrypto, Ledger.CompactForm Coin) -> ReaderT SqlBackend m DB.DrepDistr
    mkEntry (drep, coin) = do
      drepId <- insertDrep drep
      pure $
        DB.DrepDistr
          { DB.drepDistrHashId = drepId
          , DB.drepDistrAmount = fromIntegral $ unCoin $ fromCompact coin
          , DB.drepDistrEpochNo = unEpochNo e
          , DB.drepDistrActiveUntil = unEpochNo <$> isActiveEpochNo drep
          }

    isActiveEpochNo :: DRep StandardCrypto -> Maybe EpochNo
    isActiveEpochNo = \case
      DRepAlwaysAbstain -> Nothing
      DRepAlwaysNoConfidence -> Nothing
      DRepCredential cred -> drepExpiry <$> Map.lookup cred (psDRepState pSnapshot)

insertCostModel ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  Map Language Ledger.CostModel ->
  ReaderT SqlBackend m DB.CostModelId
insertCostModel _blkId cms =
  DB.insertCostModel $
    DB.CostModel
      { DB.costModelHash = Crypto.abstractHashToBytes $ Crypto.serializeCborHash $ Ledger.mkCostModels cms
      , DB.costModelCosts = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode cms
      }

updateRatified ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  EpochNo ->
  [GovActionState StandardConway] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
updateRatified syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    mGaId <- resolveGovActionProposal syncEnv $ gasId action
    whenJust mGaId $ \gaId ->
      lift $ DB.updateGovActionRatified gaId (unEpochNo epochNo)

updateExpired ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  EpochNo ->
  [GovActionId StandardCrypto] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
updateExpired syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    mGaId <- resolveGovActionProposal syncEnv action
    whenJust mGaId $ \gaId ->
      lift $ DB.updateGovActionExpired gaId (unEpochNo epochNo)

updateDropped ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  EpochNo ->
  [GovActionId StandardCrypto] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
updateDropped syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    mGaId <- resolveGovActionProposal syncEnv action
    whenJust mGaId $ \gaId ->
      lift $ DB.updateGovActionDropped gaId (unEpochNo epochNo)

insertUpdateEnacted ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.BlockId ->
  EpochNo ->
  ConwayGovState StandardConway ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertUpdateEnacted syncEnv blkId epochNo enactedState = do
  whenJust (strictMaybeToMaybe (grPParamUpdate govIds)) $ \prevId -> do
    maybeGaId <- resolveGovActionProposal syncEnv $ unGovPurposeId prevId
    case maybeGaId of
      Nothing -> pure ()
      Just gaId -> void $ lift $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)

  whenJust (strictMaybeToMaybe (grHardFork govIds)) $ \prevId -> do
    maybeGaId <- resolveGovActionProposal syncEnv $ unGovPurposeId prevId
    case maybeGaId of
      Nothing -> pure ()
      Just gaId -> void $ lift $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)

  (mcommitteeId, mnoConfidenceGaId) <- handleCommittee syncEnv govIds epochNo enactedState

  constitutionId <- handleConstitution syncEnv blkId govIds epochNo enactedState

  void $
    lift $
      DB.insertEpochState
        DB.EpochState
          { DB.epochStateCommitteeId = mcommitteeId
          , DB.epochStateNoConfidenceId = mnoConfidenceGaId
          , DB.epochStateConstitutionId = Just constitutionId
          , DB.epochStateEpochNo = unEpochNo epochNo
          }
  where
    govIds = govStatePrevGovActionIds enactedState

handleCommittee ::
  (EraCrypto era ~ StandardCrypto, MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  GovRelation StrictMaybe era ->
  EpochNo ->
  ConwayGovState StandardConway ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.CommitteeId, Maybe DB.GovActionProposalId)
handleCommittee syncEnv govIds epochNo enactedState = do
  mCommitteeGaId <- case strictMaybeToMaybe (grCommittee govIds) of
    Nothing -> pure Nothing
    Just prevId -> do
      mGaId <- resolveGovActionProposal syncEnv $ unGovPurposeId prevId
      case mGaId of
        Nothing -> pure Nothing
        Just gaId -> do
          _nCommittee <- lift $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)
          pure $ Just gaId

  case (mCommitteeGaId, strictMaybeToMaybe (cgsCommittee enactedState)) of
    (Nothing, Nothing) -> pure (Nothing, Nothing)
    (Nothing, Just committee) -> do
      -- No enacted proposal means we're after conway genesis territory
      committeeIds <- lift $ DB.queryProposalCommittee Nothing
      case committeeIds of
        [] -> do
          committeeId <- lift $ insertCommittee Nothing committee
          pure (Just committeeId, Nothing)
        (committeeId : _rest) ->
          pure (Just committeeId, Nothing)
    (Just committeeGaId, Nothing) ->
      -- No committee with enacted action means it's a no confidence action.
      pure (Nothing, Just committeeGaId)
    (Just committeeGaId, Just committee) -> do
      committeeIds <- lift $ DB.queryProposalCommittee (Just committeeGaId)
      case committeeIds of
        [] -> do
          -- This should never happen. Having a committee and an enacted action, means
          -- the committee came from a proposal which should be returned from the query.
          liftIO $
            logWarning (getTrace syncEnv) $
              mconcat
                [ "The impossible happened! Couldn't find the committee "
                , textShow committee
                , " which was enacted by a proposal "
                , textShow committeeGaId
                ]
          pure (Nothing, Nothing)
        (committeeId : _rest) ->
          pure (Just committeeId, Nothing)

handleConstitution ::
  (EraCrypto era ~ StandardCrypto, MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  DB.BlockId ->
  GovRelation StrictMaybe era ->
  EpochNo ->
  ConwayGovState StandardConway ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.ConstitutionId
handleConstitution syncEnv blkId govIds epochNo enactedState = do
  mConstitutionGaId <- case strictMaybeToMaybe (grConstitution govIds) of
    Nothing -> pure Nothing
    Just prevId -> do
      mGaId <- resolveGovActionProposal syncEnv $ unGovPurposeId prevId
      case mGaId of
        Nothing -> pure Nothing
        Just gaId -> do
          _nConstitution <- lift $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)
          pure $ Just gaId

  constitutionIds <- lift $ DB.queryProposalConstitution mConstitutionGaId
  case constitutionIds of
    -- The first case can only happen once on the first Conway epoch.
    -- On next epochs there will be at least one constitution, so the query will return something.
    [] -> lift $ insertConstitution blkId Nothing (cgsConstitution enactedState)
    constitutionId : rest -> do
      unless (null rest) $
        liftIO $
          logWarning (getTrace syncEnv) $
            mconcat
              [ "Found multiple constitutions for proposal "
              , textShow mConstitutionGaId
              , ": "
              , textShow constitutionIds
              ]
      pure constitutionId
