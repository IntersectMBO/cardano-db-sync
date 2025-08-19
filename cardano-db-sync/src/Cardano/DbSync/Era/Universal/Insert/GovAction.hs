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
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Cache (queryOrInsertRewardAccount, queryPoolKeyOrInsert, queryTxIdWithCache)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import Cardano.DbSync.DbEvent (liftDbLookup, liftDbLookupEither)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Error (SyncNodeError, mkSyncNodeCallStack)
import Cardano.DbSync.Ledger.State
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
import Cardano.Ledger.Shelley.API (Coin (..), RewardAccount)
import Cardano.Prelude
import Control.Monad.Extra (whenJust)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Cardano.Block (ConwayEra)

insertGovActionProposal ::
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  Maybe EpochNo ->
  Maybe (ConwayGovState ConwayEra) ->
  (Word64, (GovActionId, ProposalProcedure ConwayEra)) ->
  ExceptT SyncNodeError DB.DbM ()
insertGovActionProposal syncEnv blkId txId govExpiresAt mcgs (index, (govId, pp)) = do
  addrId <- queryOrInsertRewardAccount syncEnv UpdateCache $ pProcReturnAddr pp
  votingAnchorId <- insertVotingAnchor blkId DB.GovActionAnchor $ pProcAnchor pp
  mParamProposalId <-
    case pProcGovAction pp of
      ParameterChange _ pparams _ ->
        Just <$> insertParamProposal blkId txId (convertConwayParamProposal pparams)
      _otherwise -> pure Nothing
  prevGovActionDBId <- case mprevGovAction of
    Nothing -> pure Nothing
    Just prevGovActionId -> Just <$> resolveGovActionProposal syncEnv prevGovActionId
  govActionProposalId <-
    lift $
      DB.insertGovActionProposal $
        DB.GovActionProposal
          { DB.govActionProposalTxId = txId
          , DB.govActionProposalIndex = index
          , DB.govActionProposalPrevGovActionProposal = prevGovActionDBId
          , DB.govActionProposalDeposit = Generic.coinToDbLovelace $ pProcDeposit pp
          , DB.govActionProposalReturnAddress = addrId
          , DB.govActionProposalExpiration = (\epochNum -> unEpochNo epochNum + 1) <$> govExpiresAt
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
    TreasuryWithdrawals mp _ -> insertTreasuryWithdrawalsBulk govActionProposalId (Map.toList mp)
    UpdateCommittee {} -> insertNewCommittee govActionProposalId
    NewConstitution _ constitution -> void $ insertConstitution blkId (Just govActionProposalId) constitution
    _otherwise -> pure ()
  where
    mprevGovAction :: Maybe GovActionId = case pProcGovAction pp of
      ParameterChange prv _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      HardForkInitiation prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NoConfidence prv -> unGovPurposeId <$> strictMaybeToMaybe prv
      UpdateCommittee prv _ _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NewConstitution prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      _otherwise -> Nothing

    -- Bulk insert treasury withdrawals
    insertTreasuryWithdrawalsBulk ::
      DB.GovActionProposalId ->
      [(RewardAccount, Coin)] ->
      ExceptT SyncNodeError DB.DbM ()
    insertTreasuryWithdrawalsBulk _ [] = pure ()
    insertTreasuryWithdrawalsBulk gaId withdrawals = do
      let withdrawalChunks = chunksOf maxBulkSize withdrawals
      mapM_ processChunk withdrawalChunks
      where
        processChunk chunk = do
          -- Bulk resolve all reward accounts for this chunk
          let rewardAccounts = map fst chunk
          addrIds <- mapM (queryOrInsertRewardAccount syncEnv UpdateCache) rewardAccounts
          -- Create treasury withdrawals with resolved IDs for this chunk
          let treasuryWithdrawals = zipWith createTreasuryWithdrawal addrIds (map snd chunk)
          lift $ DB.insertBulkTreasuryWithdrawal treasuryWithdrawals

        createTreasuryWithdrawal addrId coin =
          DB.TreasuryWithdrawal
            { DB.treasuryWithdrawalGovActionProposalId = gaId
            , DB.treasuryWithdrawalStakeAddressId = addrId
            , DB.treasuryWithdrawalAmount = Generic.coinToDbLovelace coin
            }

    insertNewCommittee ::
      DB.GovActionProposalId ->
      ExceptT SyncNodeError DB.DbM ()
    insertNewCommittee govActionProposalId = do
      whenJust mcgs $ \cgs ->
        case findProposedCommittee govId cgs of
          Right (Just committee) -> void $ insertCommittee (Just govActionProposalId) committee
          other ->
            liftIO $ logWarning (getTrace syncEnv) $ textShow other <> ": Failed to find committee for " <> textShow pp

insertCommittee ::
  Maybe DB.GovActionProposalId ->
  Committee ConwayEra ->
  ExceptT SyncNodeError DB.DbM DB.CommitteeId
insertCommittee mgapId committee = do
  committeeId <- lift insertCommitteeDB
  mapM_ (insertNewMember committeeId) (Map.toList $ committeeMembers committee)
  pure committeeId
  where
    r = unboundRational $ committeeThreshold committee -- TODO work directly with Ratio Word64. This is not currently supported in ledger
    insertNewMember committeeId (cred, e) = do
      chId <- insertCommitteeHash cred
      void
        . lift
        . DB.insertCommitteeMember
        $ DB.CommitteeMember
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
  SyncEnv ->
  GovActionId ->
  ExceptT SyncNodeError DB.DbM DB.GovActionProposalId
resolveGovActionProposal syncEnv gaId = do
  let govTxId = gaidTxId gaId
  gaTxId <-
    liftDbLookupEither
      mkSyncNodeCallStack
      $ queryTxIdWithCache syncEnv govTxId
  let (GovActionIx index) = gaidGovActionIx gaId
  liftDbLookup
    mkSyncNodeCallStack
    $ DB.queryGovActionProposalId gaTxId (fromIntegral index) -- TODO: Use Word32?

insertParamProposal ::
  DB.BlockId ->
  DB.TxId ->
  ParamProposal ->
  ExceptT SyncNodeError DB.DbM DB.ParamProposalId
insertParamProposal blkId txId pp = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (pppCostmdls pp)
  lift
    . DB.insertParamProposal
    $ DB.ParamProposal
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

insertConstitution ::
  DB.BlockId ->
  Maybe DB.GovActionProposalId ->
  Constitution ConwayEra ->
  ExceptT SyncNodeError DB.DbM DB.ConstitutionId
insertConstitution blockId mgapId constitution = do
  votingAnchorId <- insertVotingAnchor blockId DB.ConstitutionAnchor $ constitutionAnchor constitution
  lift
    . DB.insertConstitution
    $ DB.Constitution
      { DB.constitutionGovActionProposalId = mgapId
      , DB.constitutionVotingAnchorId = votingAnchorId
      , DB.constitutionScriptHash = Generic.unScriptHash <$> strictMaybeToMaybe (constitutionScript constitution)
      }

--------------------------------------------------------------------------------------
-- VOTING PROCEDURES
--------------------------------------------------------------------------------------
insertVotingProcedures ::
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  (Voter, [(GovActionId, VotingProcedure ConwayEra)]) ->
  ExceptT SyncNodeError DB.DbM ()
insertVotingProcedures syncEnv blkId txId (voter, actions) =
  mapM_ (insertVotingProcedure syncEnv blkId txId voter) (zip [0 ..] actions)

insertVotingProcedure ::
  SyncEnv ->
  DB.BlockId ->
  DB.TxId ->
  Voter ->
  (Word16, (GovActionId, VotingProcedure ConwayEra)) ->
  ExceptT SyncNodeError DB.DbM ()
insertVotingProcedure syncEnv blkId txId voter (index, (gaId, vp)) = do
  govActionId <- resolveGovActionProposal syncEnv gaId
  votingAnchorId <- whenMaybe (strictMaybeToMaybe $ vProcAnchor vp) $ insertVotingAnchor blkId DB.VoteAnchor
  (mCommitteeVoterId, mDRepVoter, mStakePoolVoter) <- case voter of
    CommitteeVoter cred -> do
      khId <- insertCommitteeHash cred
      pure (Just khId, Nothing, Nothing)
    DRepVoter cred -> do
      drep <- insertCredDrepHash cred
      pure (Nothing, Just drep, Nothing)
    StakePoolVoter poolkh -> do
      poolHashId <- queryPoolKeyOrInsert syncEnv "insertVotingProcedure" UpdateCache False poolkh
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

insertVotingAnchor :: DB.BlockId -> DB.AnchorType -> Anchor -> ExceptT SyncNodeError DB.DbM DB.VotingAnchorId
insertVotingAnchor blockId anchorType anchor =
  lift $
    DB.insertVotingAnchor $
      DB.VotingAnchor
        { DB.votingAnchorBlockId = blockId
        , DB.votingAnchorUrl = DB.VoteUrl $ Ledger.urlToText $ anchorUrl anchor -- TODO: Conway check unicode and size of URL
        , DB.votingAnchorDataHash = Generic.safeHashToByteString $ anchorDataHash anchor
        , DB.votingAnchorType = anchorType
        }

insertCommitteeHash :: Ledger.Credential kr -> ExceptT SyncNodeError DB.DbM DB.CommitteeHashId
insertCommitteeHash cred = do
  lift $
    DB.insertCommitteeHash
      DB.CommitteeHash
        { DB.committeeHashRaw = Generic.unCredentialHash cred
        , DB.committeeHashHasScript = Generic.hasCredScript cred
        }

--------------------------------------------------------------------------------------
-- DREP
--------------------------------------------------------------------------------------
insertDrep :: DRep -> ExceptT SyncNodeError DB.DbM DB.DrepHashId
insertDrep = \case
  DRepCredential cred -> insertCredDrepHash cred
  DRepAlwaysAbstain -> lift DB.insertDrepHashAlwaysAbstain
  DRepAlwaysNoConfidence -> lift DB.insertDrepHashAlwaysNoConfidence

insertCredDrepHash :: Ledger.Credential 'DRepRole -> ExceptT SyncNodeError DB.DbM DB.DrepHashId
insertCredDrepHash cred = do
  lift $
    DB.insertDrepHash
      DB.DrepHash
        { DB.drepHashRaw = Just bs
        , DB.drepHashView = serialiseDrepToBech32 bs
        , DB.drepHashHasScript = Generic.hasCredScript cred
        }
  where
    bs = Generic.unCredentialHash cred

insertDrepDistr :: EpochNo -> PulsingSnapshot ConwayEra -> ExceptT SyncNodeError DB.DbM ()
insertDrepDistr e pSnapshot = do
  let drepEntries = Map.toList $ psDRepDistr pSnapshot
      drepChunks = chunksOf maxBulkSize drepEntries
  mapM_ processChunk drepChunks
  where
    processChunk chunk = do
      drepsDB <- mapM mkEntry chunk
      lift $ DB.insertBulkDrepDistr drepsDB

    mkEntry :: (DRep, Ledger.CompactForm Coin) -> ExceptT SyncNodeError DB.DbM DB.DrepDistr
    mkEntry (drep, coin) = do
      drepId <- insertDrep drep
      pure $
        DB.DrepDistr
          { DB.drepDistrHashId = drepId
          , DB.drepDistrAmount = fromIntegral $ unCoin $ fromCompact coin
          , DB.drepDistrEpochNo = unEpochNo e
          , DB.drepDistrActiveUntil = unEpochNo <$> isActiveEpochNo drep
          }

    isActiveEpochNo :: DRep -> Maybe EpochNo
    isActiveEpochNo = \case
      DRepAlwaysAbstain -> Nothing
      DRepAlwaysNoConfidence -> Nothing
      DRepCredential cred -> drepExpiry <$> Map.lookup cred (psDRepState pSnapshot)

insertCostModel ::
  DB.BlockId ->
  Map Language Ledger.CostModel ->
  ExceptT SyncNodeError DB.DbM DB.CostModelId
insertCostModel _blkId cms =
  lift $
    DB.insertCostModel $
      DB.CostModel
        { DB.costModelHash = Crypto.abstractHashToBytes $ Crypto.serializeCborHash $ Ledger.mkCostModels cms
        , DB.costModelCosts = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode cms
        }

updateRatified ::
  SyncEnv ->
  EpochNo ->
  [GovActionState ConwayEra] ->
  ExceptT SyncNodeError DB.DbM ()
updateRatified syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal syncEnv $ gasId action
    lift $ DB.updateGovActionRatified gaId (unEpochNo epochNo)

updateExpired ::
  SyncEnv ->
  EpochNo ->
  [GovActionId] ->
  ExceptT SyncNodeError DB.DbM ()
updateExpired syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal syncEnv action
    lift $ DB.updateGovActionExpired gaId (unEpochNo epochNo)

updateDropped ::
  SyncEnv ->
  EpochNo ->
  [GovActionId] ->
  ExceptT SyncNodeError DB.DbM ()
updateDropped syncEnv epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal syncEnv action
    lift $ DB.updateGovActionDropped gaId (unEpochNo epochNo)

insertUpdateEnacted ::
  SyncEnv ->
  DB.BlockId ->
  EpochNo ->
  ConwayGovState ConwayEra ->
  ExceptT SyncNodeError DB.DbM ()
insertUpdateEnacted syncEnv blkId epochNo enactedState = do
  (mcommitteeId, mnoConfidenceGaId) <- handleCommittee
  constitutionId <- handleConstitution
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

    trce = getTrace syncEnv

    handleCommittee :: ExceptT SyncNodeError DB.DbM (Maybe DB.CommitteeId, Maybe DB.GovActionProposalId)
    handleCommittee = do
      mCommitteeGaId <- case strictMaybeToMaybe (grCommittee govIds) of
        Nothing -> pure Nothing
        Just prevId ->
          fmap Just <$> resolveGovActionProposal syncEnv $ unGovPurposeId prevId

      case (mCommitteeGaId, strictMaybeToMaybe (cgsCommittee enactedState)) of
        (Nothing, Nothing) -> pure (Nothing, Nothing)
        (Nothing, Just committee) -> do
          -- No enacted proposal means we're after conway genesis territory
          committeeIds <- lift $ DB.queryProposalCommittee Nothing
          case committeeIds of
            [] -> do
              committeeId <- insertCommittee Nothing committee
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
                logWarning trce $
                  mconcat
                    [ "The impossible happened! Couldn't find the committee "
                    , textShow committee
                    , " which was enacted by a proposal "
                    , textShow committeeGaId
                    ]
              pure (Nothing, Nothing)
            (committeeId : _rest) ->
              pure (Just committeeId, Nothing)

    handleConstitution :: ExceptT SyncNodeError DB.DbM DB.ConstitutionId
    handleConstitution = do
      mConstitutionGaId <- case strictMaybeToMaybe (grConstitution govIds) of
        Nothing -> pure Nothing
        Just prevId ->
          fmap Just <$> resolveGovActionProposal syncEnv $ unGovPurposeId prevId

      constitutionIds <- lift $ DB.queryProposalConstitution mConstitutionGaId
      case constitutionIds of
        -- The first case can only happen once on the first Conway epoch.
        -- On next epochs there will be at least one constitution, so the query will return something.
        [] -> insertConstitution blkId Nothing (cgsConstitution enactedState)
        constitutionId : rest -> do
          unless (null rest) $
            liftIO $
              logWarning trce $
                mconcat
                  [ "Found multiple constitutions for proposal "
                  , textShow mConstitutionGaId
                  , ": "
                  , textShow constitutionIds
                  ]
          pure constitutionId
