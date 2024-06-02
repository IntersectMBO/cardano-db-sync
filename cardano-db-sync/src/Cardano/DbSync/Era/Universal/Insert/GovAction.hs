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
import Cardano.DbSync.AppT (App, MonadAppDB (..), SyncEnv (..), askTrace)
import Cardano.DbSync.Cache (queryOrInsertRewardAccount, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (UpdateCache (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Era.Util (liftLookupFail)
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Cardano.Block (StandardConway, StandardCrypto)

insertGovActionProposal ::
  DB.BlockId ->
  DB.TxId ->
  Maybe EpochNo ->
  Maybe (StrictMaybe (Committee StandardConway)) ->
  (Word64, ProposalProcedure StandardConway) ->
  App ()
insertGovActionProposal blkId txId govExpiresAt mmCommittee (index, pp) = do
  cache <- asks envCache
  addrId <- queryOrInsertRewardAccount cache UpdateCache $ pProcReturnAddr pp
  votingAnchorId <- insertVotingAnchor blkId DB.GovActionAnchor $ pProcAnchor pp
  mParamProposalId <-
    case pProcGovAction pp of
      ParameterChange _ pparams _ ->
        Just <$> insertParamProposal blkId txId (convertConwayParamProposal pparams)
      _other -> pure Nothing
  prevGovActionDBId <- case mprevGovAction of
    Nothing -> pure Nothing
    Just prevGovActionId -> Just <$> resolveGovActionProposal prevGovActionId
  govActionProposalId <-
    dbQueryToApp $
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
    TreasuryWithdrawals mp _ -> mapM_ (insertTreasuryWithdrawal govActionProposalId) (Map.toList mp)
    UpdateCommittee _ removed added q -> insertNewCommittee (Just govActionProposalId) removed added q
    NewConstitution _ constitution -> void $ insertConstitution blkId (Just govActionProposalId) constitution
    _other -> pure ()
  where
    mprevGovAction :: Maybe (GovActionId StandardCrypto) = case pProcGovAction pp of
      ParameterChange prv _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      HardForkInitiation prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NoConfidence prv -> unGovPurposeId <$> strictMaybeToMaybe prv
      UpdateCommittee prv _ _ _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      NewConstitution prv _ -> unGovPurposeId <$> strictMaybeToMaybe prv
      _other -> Nothing

    insertTreasuryWithdrawal gaId (rwdAcc, coin) = do
      cache <- asks envCache
      addrId <- queryOrInsertRewardAccount cache UpdateCache rwdAcc
      dbQueryToApp $
        DB.insertTreasuryWithdrawal $
          DB.TreasuryWithdrawal
            { DB.treasuryWithdrawalGovActionProposalId = gaId
            , DB.treasuryWithdrawalStakeAddressId = addrId
            , DB.treasuryWithdrawalAmount = Generic.coinToDbLovelace coin
            }

    insertNewCommittee ::
      Maybe DB.GovActionProposalId ->
      Set (Ledger.Credential 'ColdCommitteeRole StandardCrypto) ->
      Map (Ledger.Credential 'ColdCommitteeRole StandardCrypto) EpochNo ->
      UnitInterval ->
      App ()
    insertNewCommittee gapId removed added q = do
      void $ insertCommittee' gapId (updatedCommittee removed added q <$> mmCommittee) q

insertCommittee :: Maybe DB.GovActionProposalId -> Committee StandardConway -> App DB.CommitteeId
insertCommittee mgapId committee = do
  insertCommittee' mgapId (Just committee) (committeeThreshold committee)

insertCommittee' ::
  Maybe DB.GovActionProposalId ->
  Maybe (Committee StandardConway) ->
  UnitInterval ->
  App DB.CommitteeId
insertCommittee' mgapId mcommittee q = do
  committeeId <- insertCommitteeDB
  whenJust mcommittee $ \committee ->
    mapM_ (insertNewMember committeeId) (Map.toList $ committeeMembers committee)
  pure committeeId
  where
    r = unboundRational q -- TODO work directly with Ratio Word64. This is not currently supported in ledger
    insertNewMember committeeId (cred, e) = do
      chId <- insertCommitteeHash cred
      void . dbQueryToApp $
        DB.insertCommitteeMember $
          DB.CommitteeMember
            { DB.committeeMemberCommitteeId = committeeId
            , DB.committeeMemberCommitteeHashId = chId
            , DB.committeeMemberExpirationEpoch = unEpochNo e
            }

    insertCommitteeDB =
      dbQueryToApp $
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
  GovActionId StandardCrypto ->
  App DB.GovActionProposalId
resolveGovActionProposal gaId = do
  gaTxId <-
    liftLookupFail "resolveGovActionProposal.queryTxId" $
      dbQueryToApp $
        DB.queryTxId $
          Generic.unTxHash $
            gaidTxId gaId
  let (GovActionIx index) = gaidGovActionIx gaId
  liftLookupFail "resolveGovActionProposal.queryGovActionProposalId" $
    dbQueryToApp $
      DB.queryGovActionProposalId gaTxId (fromIntegral index) -- TODO: Use Word32?

insertParamProposal ::
  DB.BlockId ->
  DB.TxId ->
  ParamProposal ->
  App DB.ParamProposalId
insertParamProposal blkId txId pp = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (pppCostmdls pp)
  dbQueryToApp $
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

insertConstitution :: DB.BlockId -> Maybe DB.GovActionProposalId -> Constitution StandardConway -> App DB.ConstitutionId
insertConstitution blockId gapId constitution = do
  votingAnchorId <- insertVotingAnchor blockId DB.OtherAnchor $ constitutionAnchor constitution
  dbQueryToApp $
    DB.insertConstitution $
      DB.Constitution
        { DB.constitutionGovActionProposalId = gapId
        , DB.constitutionVotingAnchorId = votingAnchorId
        , DB.constitutionScriptHash = Generic.unScriptHash <$> strictMaybeToMaybe (constitutionScript constitution)
        }

--------------------------------------------------------------------------------------
-- VOTING PROCEDURES
--------------------------------------------------------------------------------------
insertVotingProcedures ::
  DB.BlockId ->
  DB.TxId ->
  (Voter StandardCrypto, [(GovActionId StandardCrypto, VotingProcedure StandardConway)]) ->
  App ()
insertVotingProcedures blkId txId (voter, actions) =
  mapM_ (insertVotingProcedure blkId txId voter) (zip [0 ..] actions)

insertVotingProcedure ::
  DB.BlockId ->
  DB.TxId ->
  Voter StandardCrypto ->
  (Word16, (GovActionId StandardCrypto, VotingProcedure StandardConway)) ->
  App ()
insertVotingProcedure blkId txId voter (index, (gaId, vp)) = do
  cache <- asks envCache
  govActionId <- resolveGovActionProposal gaId
  votingAnchorId <- whenMaybe (strictMaybeToMaybe $ vProcAnchor vp) $ insertVotingAnchor blkId DB.OtherAnchor
  (mCommitteeVoterId, mDRepVoter, mStakePoolVoter) <- case voter of
    CommitteeVoter cred -> do
      khId <- insertCommitteeHash cred
      pure (Just khId, Nothing, Nothing)
    DRepVoter cred -> do
      drep <- insertCredDrepHash cred
      pure (Nothing, Just drep, Nothing)
    StakePoolVoter poolkh -> do
      poolHashId <- queryPoolKeyOrInsert "insertVotingProcedure" cache UpdateCache False poolkh
      pure (Nothing, Nothing, Just poolHashId)
  void
    . dbQueryToApp
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

insertVotingAnchor :: DB.BlockId -> DB.AnchorType -> Anchor StandardCrypto -> App DB.VotingAnchorId
insertVotingAnchor blockId anchorType anchor =
  dbQueryToApp $
    DB.insertAnchor $
      DB.VotingAnchor
        { DB.votingAnchorBlockId = blockId
        , DB.votingAnchorUrl = DB.VoteUrl $ Ledger.urlToText $ anchorUrl anchor -- TODO: Conway check unicode and size of URL
        , DB.votingAnchorDataHash = Generic.safeHashToByteString $ anchorDataHash anchor
        , DB.votingAnchorType = anchorType
        }

insertCommitteeHash :: Ledger.Credential kr StandardCrypto -> App DB.CommitteeHashId
insertCommitteeHash cred = do
  dbQueryToApp $
    DB.insertCommitteeHash
      DB.CommitteeHash
        { DB.committeeHashRaw = Generic.unCredentialHash cred
        , DB.committeeHashHasScript = Generic.hasCredScript cred
        }

--------------------------------------------------------------------------------------
-- DREP
--------------------------------------------------------------------------------------
insertDrep :: DRep StandardCrypto -> App DB.DrepHashId
insertDrep = \case
  DRepCredential cred -> insertCredDrepHash cred
  DRepAlwaysAbstain -> dbQueryToApp DB.insertAlwaysAbstainDrep
  DRepAlwaysNoConfidence -> dbQueryToApp DB.insertAlwaysNoConfidence

insertCredDrepHash :: Ledger.Credential 'DRepRole StandardCrypto -> App DB.DrepHashId
insertCredDrepHash cred = do
  dbQueryToApp $
    DB.insertDrepHash
      DB.DrepHash
        { DB.drepHashRaw = Just bs
        , DB.drepHashView = serialiseDrepToBech32 bs
        , DB.drepHashHasScript = Generic.hasCredScript cred
        }
  where
    bs = Generic.unCredentialHash cred

insertDrepDistr :: EpochNo -> PulsingSnapshot StandardConway -> App ()
insertDrepDistr e pSnapshot = do
  drepsDB <- mapM mkEntry (Map.toList $ psDRepDistr pSnapshot)
  dbQueryToApp $ DB.insertManyDrepDistr drepsDB
  where
    mkEntry :: (DRep StandardCrypto, Ledger.CompactForm Coin) -> App DB.DrepDistr
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
  DB.BlockId ->
  Map Language Ledger.CostModel ->
  App DB.CostModelId
insertCostModel _blkId cms =
  dbQueryToApp $
    DB.insertCostModel $
      DB.CostModel
        { DB.costModelHash = Crypto.abstractHashToBytes $ Crypto.serializeCborHash $ Ledger.mkCostModels cms
        , DB.costModelCosts = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode cms
        }

updateRatified ::
  EpochNo ->
  [GovActionState StandardConway] ->
  App ()
updateRatified epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal $ gasId action
    dbQueryToApp $ DB.updateGovActionRatified gaId (unEpochNo epochNo)

updateExpired ::
  EpochNo ->
  [GovActionId StandardCrypto] ->
  App ()
updateExpired epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal action
    dbQueryToApp $ DB.updateGovActionExpired gaId (unEpochNo epochNo)

updateDropped ::
  EpochNo ->
  [GovActionId StandardCrypto] ->
  App ()
updateDropped epochNo ratifiedActions = do
  forM_ ratifiedActions $ \action -> do
    gaId <- resolveGovActionProposal action
    dbQueryToApp $ DB.updateGovActionDropped gaId (unEpochNo epochNo)

insertUpdateEnacted ::
  DB.BlockId ->
  EpochNo ->
  ConwayGovState StandardConway ->
  App ()
insertUpdateEnacted blkId epochNo enactedState = do
  whenJust (strictMaybeToMaybe (grPParamUpdate govIds)) $ \prevId -> do
    gaId <- resolveGovActionProposal $ unGovPurposeId prevId
    void $ dbQueryToApp $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)

  whenJust (strictMaybeToMaybe (grHardFork govIds)) $ \prevId -> do
    gaId <- resolveGovActionProposal $ unGovPurposeId prevId
    void $ dbQueryToApp $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)

  (mcommitteeId, mnoConfidenceGaId) <- handleCommittee

  constitutionId <- handleConstitution

  void $
    dbQueryToApp $
      DB.insertEpochState
        DB.EpochState
          { DB.epochStateCommitteeId = mcommitteeId
          , DB.epochStateNoConfidenceId = mnoConfidenceGaId
          , DB.epochStateConstitutionId = Just constitutionId
          , DB.epochStateEpochNo = unEpochNo epochNo
          }
  where
    govIds = govStatePrevGovActionIds enactedState

    handleCommittee = do
      trce <- askTrace
      mCommitteeGaId <- case strictMaybeToMaybe (grCommittee govIds) of
        Nothing -> pure Nothing
        Just prevId -> do
          gaId <- resolveGovActionProposal $ unGovPurposeId prevId
          _nCommittee <- dbQueryToApp $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)
          pure $ Just gaId

      case (mCommitteeGaId, strictMaybeToMaybe (cgsCommittee enactedState)) of
        (Nothing, Nothing) -> pure (Nothing, Nothing)
        (Nothing, Just committee) -> do
          -- No enacted proposal means we're after conway genesis territory
          committeeIds <- dbQueryToApp $ DB.queryProposalCommittee Nothing
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
          committeeIds <- dbQueryToApp $ DB.queryProposalCommittee (Just committeeGaId)
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

    handleConstitution :: App DB.ConstitutionId
    handleConstitution = do
      trce <- askTrace
      mConstitutionGaId <- case strictMaybeToMaybe (grConstitution govIds) of
        Nothing -> pure Nothing
        Just prevId -> do
          gaId <- resolveGovActionProposal $ unGovPurposeId prevId
          _nConstitution <- dbQueryToApp $ DB.updateGovActionEnacted gaId (unEpochNo epochNo)
          pure $ Just gaId

      constitutionIds <- dbQueryToApp $ DB.queryProposalConstitution mConstitutionGaId
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
