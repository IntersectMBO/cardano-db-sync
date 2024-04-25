{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  insertVotingAnchor,
  resolveGovActionProposal,
  updateEnacted,
)
where

import Cardano.BM.Trace (Trace)
import qualified Cardano.Crypto as Crypto
import Cardano.Db (DbWord64 (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (queryOrInsertRewardAccount, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (Cache (..), CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Bech32 (serialiseDrepToBech32)
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.CertState (DRep (..))
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Core (DRepVotingThresholds (..), PoolVotingThresholds (..))
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
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardConway, StandardCrypto)

insertGovActionProposal ::
  forall m.
  (MonadIO m, MonadBaseControl IO m) =>
  Cache ->
  DB.BlockId ->
  DB.TxId ->
  Maybe EpochNo ->
  Maybe (StrictMaybe (Committee StandardConway)) ->
  (Word64, ProposalProcedure StandardConway) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertGovActionProposal cache blkId txId govExpiresAt mmCommittee (index, pp) = do
  addrId <-
    lift $ queryOrInsertRewardAccount cache CacheNew $ pProcReturnAddr pp
  votingAnchorId <- lift $ insertVotingAnchor txId DB.GovActionAnchor $ pProcAnchor pp
  mParamProposalId <- lift $
    case pProcGovAction pp of
      ParameterChange _ pparams _ ->
        Just <$> insertParamProposal blkId txId (convertConwayParamProposal pparams)
      _ -> pure Nothing
  prevGovActionDBId <- case mprevGovAction of
    Nothing -> pure Nothing
    Just prevGovActionId -> Just <$> resolveGovActionProposal prevGovActionId
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
    UpdateCommittee _ removed added q -> lift $ insertNewCommittee govActionProposalId removed added q
    NewConstitution _ constitution -> lift $ insertConstitution txId govActionProposalId constitution
    _ -> pure ()
  where
    mprevGovAction :: Maybe (GovActionId StandardCrypto) = case pProcGovAction pp of
      ParameterChange prv _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      HardForkInitiation prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NoConfidence prv -> unGovPurposeId <$> strictMaybeToMaybe prv
      UpdateCommittee prv _ _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NewConstitution prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      _ -> Nothing

    insertTreasuryWithdrawal gaId (rwdAcc, coin) = do
      addrId <-
        queryOrInsertRewardAccount cache CacheNew rwdAcc
      DB.insertTreasuryWithdrawal $
        DB.TreasuryWithdrawal
          { DB.treasuryWithdrawalGovActionProposalId = gaId
          , DB.treasuryWithdrawalStakeAddressId = addrId
          , DB.treasuryWithdrawalAmount = Generic.coinToDbLovelace coin
          }

    insertNewCommittee ::
      DB.GovActionProposalId ->
      Set (Ledger.Credential 'ColdCommitteeRole StandardCrypto) ->
      Map (Ledger.Credential 'ColdCommitteeRole StandardCrypto) EpochNo ->
      UnitInterval ->
      ReaderT SqlBackend m ()
    insertNewCommittee gapId removed added q = do
      insertNewCommitteeInfo gapId q
      insertMembers gapId removed added q

    insertNewCommitteeInfo gapId q =
      void . DB.insertNewCommitteeInfo $
        DB.NewCommitteeInfo
          { DB.newCommitteeInfoGovActionProposalId = gapId
          , DB.newCommitteeInfoQuorumNumerator = fromIntegral $ numerator r
          , DB.newCommitteeInfoQuorumDenominator = fromIntegral $ denominator r
          }
      where
        r = unboundRational q -- TODO work directly with Ratio Word64. This is not currently supported in ledger
    insertMembers gapId removed added q = do
      whenJust mmCommittee $ \mCommittee -> do
        -- Nothing means we're not in Conway so it can't happen.
        let committee = updatedCommittee removed added q mCommittee
        mapM_ (insertNewMember gapId) (Map.toList $ committeeMembers committee)

    insertNewMember gapId (cred, e) = do
      chId <- insertCommitteeHash cred
      void . DB.insertNewCommitteeMember $
        DB.NewCommitteeMember
          { DB.newCommitteeMemberGovActionProposalId = gapId
          , DB.newCommitteeMemberCommitteeHashId = chId
          , DB.newCommitteeMemberExpirationEpoch = unEpochNo e
          }

--------------------------------------------------------------------------------------
-- PROPOSAL
--------------------------------------------------------------------------------------
resolveGovActionProposal ::
  MonadIO m =>
  GovActionId StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.GovActionProposalId
resolveGovActionProposal gaId = do
  gaTxId <-
    liftLookupFail "resolveGovActionProposal.queryTxId" $
      DB.queryTxId $
        Generic.unTxHash $
          gaidTxId gaId
  let (GovActionIx index) = gaidGovActionIx gaId
  liftLookupFail "resolveGovActionProposal.queryGovActionProposalId" $
    DB.queryGovActionProposalId gaTxId (fromIntegral index) -- TODO: Use Word32?

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
      }

insertConstitution :: (MonadIO m, MonadBaseControl IO m) => DB.TxId -> DB.GovActionProposalId -> Constitution StandardConway -> ReaderT SqlBackend m ()
insertConstitution txId gapId constitution = do
  votingAnchorId <- insertVotingAnchor txId DB.OtherAnchor $ constitutionAnchor constitution
  void . DB.insertConstitution $
    DB.Constitution
      { DB.constitutionGovActionProposalId = gapId
      , DB.constitutionVotingAnchorId = votingAnchorId
      , DB.constitutionScriptHash = Generic.unScriptHash <$> strictMaybeToMaybe (constitutionScript constitution)
      }

--------------------------------------------------------------------------------------
-- VOTING PROCEDURES
--------------------------------------------------------------------------------------
insertVotingProcedures ::
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  (Voter StandardCrypto, [(GovActionId StandardCrypto, VotingProcedure StandardConway)]) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertVotingProcedures trce cache txId (voter, actions) =
  mapM_ (insertVotingProcedure trce cache txId voter) (zip [0 ..] actions)

insertVotingProcedure ::
  (MonadIO m, MonadBaseControl IO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  Voter StandardCrypto ->
  (Word16, (GovActionId StandardCrypto, VotingProcedure StandardConway)) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertVotingProcedure trce cache txId voter (index, (gaId, vp)) = do
  govActionId <- resolveGovActionProposal gaId
  votingAnchorId <- whenMaybe (strictMaybeToMaybe $ vProcAnchor vp) $ lift . insertVotingAnchor txId DB.OtherAnchor
  (mCommitteeVoterId, mDRepVoter, mStakePoolVoter) <- case voter of
    CommitteeVoter cred -> do
      khId <- lift $ insertCommitteeHash cred
      pure (Just khId, Nothing, Nothing)
    DRepVoter cred -> do
      drep <- lift $ insertCredDrepHash cred
      pure (Nothing, Just drep, Nothing)
    StakePoolVoter poolkh -> do
      poolHashId <- lift $ queryPoolKeyOrInsert "insertVotingProcedure" trce cache CacheNew False poolkh
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
      }

insertVotingAnchor :: (MonadIO m, MonadBaseControl IO m) => DB.TxId -> DB.AnchorType -> Anchor StandardCrypto -> ReaderT SqlBackend m DB.VotingAnchorId
insertVotingAnchor txId anchorType anchor =
  DB.insertAnchor $
    DB.VotingAnchor
      { DB.votingAnchorTxId = txId
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

updateEnacted :: forall m. (MonadBaseControl IO m, MonadIO m) => Bool -> EpochNo -> EnactState StandardConway -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
updateEnacted isEnacted epochNo enactedState = do
  whenJust (strictMaybeToMaybe (enactedState ^. ensPrevPParamUpdateL)) $ \prevId -> do
    gaId <- resolveGovActionProposal $ getPrevId prevId
    if isEnacted
      then lift $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)
      else lift $ DB.updateGovActionRatified gaId (unEpochNo epochNo)
  where
    getPrevId = unGovPurposeId
