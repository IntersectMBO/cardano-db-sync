{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.ChainGen where

import Cardano.Prelude hiding (from, isNothing, map, on)
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SCE
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SCG
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Core.MultiAsset as MultiAsset
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Core.StakeDeligation as SCSD
import qualified Cardano.Db.Schema.Variants as SV
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Function.Core (mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Function.Query (countAll, countWhere, parameterisedCountWhere)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), tableName)
import Cardano.Db.Types (Ada, DbAction (..), RewardSource, rewardSourceDecoder, word64ToAda)
import Data.Functor.Contravariant ((>$<))
import qualified Data.List.NonEmpty as NE
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as Text
import Prelude hiding (length, show, (.))

queryEpochParamWithEpochNoStmt :: HsqlStmt.Statement Word64 (Maybe (Entity SCE.EpochParam))
queryEpochParamWithEpochNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    epochParamTableN = tableName (Proxy @SCE.EpochParam)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> epochParamTableN
          , " WHERE epoch_no = $1"
          , " LIMIT 1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe SCE.entityEpochParamDecoder

-- | Query protocol parameters from @EpochParam@ by epoch number.
queryEpochParamWithEpochNo :: MonadIO m => Word64 -> DbAction m (Maybe SCE.EpochParam)
queryEpochParamWithEpochNo epochNo = do
  result <-
    runDbSession (mkDbCallStack "queryEpochParamWithEpochNo") $
      HsqlSes.statement epochNo queryEpochParamWithEpochNoStmt
  pure $ entityVal <$> result

------------------------------------------------------------------------------------------------

queryParamProposalWithEpochNoStmt :: HsqlStmt.Statement Word64 (Maybe (Entity SGV.ParamProposal))
queryParamProposalWithEpochNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    paramProposalTableN = tableName (Proxy @SGV.ParamProposal)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> paramProposalTableN
          , " WHERE epoch_no = $1"
          , " LIMIT 1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe SGV.entityParamProposalDecoder

-- | Query protocol parameter proposals from @ParamProposal@ by epoch number.
queryParamProposalWithEpochNo :: MonadIO m => Word64 -> DbAction m (Maybe SGV.ParamProposal)
queryParamProposalWithEpochNo epochNo = do
  result <-
    runDbSession (mkDbCallStack "queryParamProposalWithEpochNo") $
      HsqlSes.statement epochNo queryParamProposalWithEpochNoStmt
  pure $ entityVal <$> result

------------------------------------------------------------------------------------------------

queryParamWithEpochNoStmt :: HsqlStmt.Statement Word64 (Maybe (Entity SCE.EpochParam))
queryParamWithEpochNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    epochParamTableN = tableName (Proxy @SCE.EpochParam)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> epochParamTableN
          , " WHERE epoch_no = $1"
          , " LIMIT 1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe SCE.entityEpochParamDecoder

queryParamWithEpochNo :: MonadIO m => Word64 -> DbAction m (Maybe SCE.EpochParam)
queryParamWithEpochNo epochNo = do
  result <-
    runDbSession (mkDbCallStack "queryParamWithEpochNo") $
      HsqlSes.statement epochNo queryParamWithEpochNoStmt
  pure $ entityVal <$> result

------------------------------------------------------------------------------------------------

queryNullTxDepositExistsStmt :: HsqlStmt.Statement () Bool
queryNullTxDepositExistsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS ("
          , "  SELECT 1 FROM " <> txTableN
          , "  WHERE deposit IS NULL"
          , ")"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.bool)

-- | Query whether there any null tx deposits?
queryNullTxDepositExists :: MonadIO m => DbAction m Bool
queryNullTxDepositExists =
  runDbSession (mkDbCallStack "queryNullTxDepositExists") $
    HsqlSes.statement () queryNullTxDepositExistsStmt

------------------------------------------------------------------------------------------------

queryMultiAssetCountStmt :: HsqlStmt.Statement () Word
queryMultiAssetCountStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    multiAssetTableN = tableName (Proxy @MultiAsset.MultiAsset)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> multiAssetTableN
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryMultiAssetCount :: MonadIO m => DbAction m Word
queryMultiAssetCount =
  runDbSession (mkDbCallStack "queryMultiAssetCount") $
    HsqlSes.statement () queryMultiAssetCountStmt

------------------------------------------------------------------------------------------------

queryTxMetadataCountStmt :: HsqlStmt.Statement () Word
queryTxMetadataCountStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txMetadataTableN = tableName (Proxy @SCB.TxMetadata)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> txMetadataTableN
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryTxMetadataCount :: MonadIO m => DbAction m Word
queryTxMetadataCount =
  runDbSession (mkDbCallStack "queryTxMetadataCount") $
    HsqlSes.statement () queryTxMetadataCountStmt

------------------------------------------------------------------------------------------------

queryDRepDistrAmountStmt :: HsqlStmt.Statement (ByteString, Word64) (Maybe Word64)
queryDRepDistrAmountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    drepDistrTableN = tableName (Proxy @SCG.DrepDistr)
    drepHashTableN = tableName (Proxy @SCG.DrepHash)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT distr.amount"
          , " FROM " <> drepDistrTableN <> " distr"
          , " INNER JOIN " <> drepHashTableN <> " hash ON hash.id = distr.hash_id"
          , " WHERE hash.raw = $1 AND distr.epoch_no = $2"
          , " LIMIT 1"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]

    decoder = HsqlD.rowMaybe (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryDRepDistrAmount :: MonadIO m => ByteString -> Word64 -> DbAction m Word64
queryDRepDistrAmount drepHash epochNo = do
  result <-
    runDbSession (mkDbCallStack "queryDRepDistrAmount") $
      HsqlSes.statement (drepHash, epochNo) queryDRepDistrAmountStmt
  pure $ fromMaybe 0 result

------------------------------------------------------------------------------------------------

queryGovActionCountsStmt :: HsqlStmt.Statement () (Word, Word, Word, Word)
queryGovActionCountsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    govActionTableN = tableName (Proxy @SGV.GovActionProposal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , "  COUNT(CASE WHEN ratified_epoch IS NOT NULL THEN 1 END)::bigint,"
          , "  COUNT(CASE WHEN enacted_epoch IS NOT NULL THEN 1 END)::bigint,"
          , "  COUNT(CASE WHEN dropped_epoch IS NOT NULL THEN 1 END)::bigint,"
          , "  COUNT(CASE WHEN expired_epoch IS NOT NULL THEN 1 END)::bigint"
          , " FROM " <> govActionTableN
          ]
    decoder = HsqlD.singleRow $ do
      ratified <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      enacted <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      dropped <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      expired <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (ratified, enacted, dropped, expired)

queryGovActionCounts :: MonadIO m => DbAction m (Word, Word, Word, Word)
queryGovActionCounts =
  runDbSession (mkDbCallStack "queryGovActionCounts") $
    HsqlSes.statement () queryGovActionCountsStmt

------------------------------------------------------------------------------------------------

queryConstitutionAnchorStmt :: HsqlStmt.Statement Word64 (Maybe (Text, ByteString))
queryConstitutionAnchorStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    constitutionTableN = tableName (Proxy @SCG.Constitution)
    votingAnchorTableN = tableName (Proxy @SCG.VotingAnchor)
    epochStateTableN = tableName (Proxy @SCE.EpochState)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT anchor.url, anchor.data_hash"
          , " FROM " <> constitutionTableN <> " constit"
          , " INNER JOIN " <> votingAnchorTableN <> " anchor ON constit.voting_anchor_id = anchor.id"
          , " INNER JOIN " <> epochStateTableN <> " epoch ON constit.id = epoch.constitution_id"
          , " WHERE epoch.epoch_no = $1"
          , " LIMIT 1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

    decoder = HsqlD.rowMaybe $ do
      url <- HsqlD.column (HsqlD.nonNullable HsqlD.text)
      dataHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (url, dataHash)

queryConstitutionAnchor :: MonadIO m => Word64 -> DbAction m (Maybe (Text, ByteString))
queryConstitutionAnchor epochNo =
  runDbSession (mkDbCallStack "queryConstitutionAnchor") $
    HsqlSes.statement epochNo queryConstitutionAnchorStmt

------------------------------------------------------------------------------------------------

queryRewardRestsStmt :: HsqlStmt.Statement () [(RewardSource, Word64)]
queryRewardRestsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    rewardRestTableN = tableName (Proxy @SCSD.RewardRest)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT type, amount"
          , " FROM " <> rewardRestTableN
          ]

    decoder = HsqlD.rowList $ do
      rewardType <- HsqlD.column (HsqlD.nonNullable rewardSourceDecoder)
      amount <- HsqlD.column (HsqlD.nonNullable (fromMaybe 0 . toBoundedInteger <$> HsqlD.numeric))
      pure (rewardType, amount)

queryRewardRests :: MonadIO m => DbAction m [(RewardSource, Word64)]
queryRewardRests =
  runDbSession (mkDbCallStack "queryRewardRests") $
    HsqlSes.statement () queryRewardRestsStmt

------------------------------------------------------------------------------------------------

queryTreasuryDonationsStmt :: HsqlStmt.Statement () Word64
queryTreasuryDonationsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(treasury_donation), 0)"
          , " FROM " <> txTableN
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryTreasuryDonations :: MonadIO m => DbAction m Word64
queryTreasuryDonations =
  runDbSession (mkDbCallStack "queryTreasuryDonations") $
    HsqlSes.statement () queryTreasuryDonationsStmt

------------------------------------------------------------------------------------------------

queryVoteCountsStmt :: HsqlStmt.Statement (ByteString, Word16) (Word64, Word64, Word64)
queryVoteCountsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    votingProcedureTableN = tableName (Proxy @SCG.VotingProcedure)
    txTableN = tableName (Proxy @SCB.Tx)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , "  COUNT(CASE WHEN vote.vote = 'Yes' THEN 1 END)::bigint," -- Changed from 'VoteYes'
          , "  COUNT(CASE WHEN vote.vote = 'No' THEN 1 END)::bigint," -- Changed from 'VoteNo'
          , "  COUNT(CASE WHEN vote.vote = 'Abstain' THEN 1 END)::bigint" -- Changed from 'VoteAbstain'
          , " FROM " <> votingProcedureTableN <> " vote"
          , " INNER JOIN " <> txTableN <> " tx ON vote.tx_id = tx.id"
          , " WHERE tx.hash = $1 AND vote.index = $2"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int2)
        ]

    decoder = HsqlD.singleRow $ do
      yes <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      no <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      abstain <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (yes, no, abstain)

queryVoteCounts :: MonadIO m => ByteString -> Word16 -> DbAction m (Word64, Word64, Word64)
queryVoteCounts txHash idx =
  runDbSession (mkDbCallStack "queryVoteCounts") $
    HsqlSes.statement (txHash, idx) queryVoteCountsStmt

------------------------------------------------------------------------------------------------

queryEpochStateCountStmt :: HsqlStmt.Statement Word64 Word64
queryEpochStateCountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    epochStateTableN = tableName (Proxy @SCE.EpochState)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> epochStateTableN
          , " WHERE epoch_no = $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryEpochStateCount :: MonadIO m => Word64 -> DbAction m Word64
queryEpochStateCount epochNo =
  runDbSession (mkDbCallStack "queryEpochStateCount") $
    HsqlSes.statement epochNo queryEpochStateCountStmt

------------------------------------------------------------------------------------------------

queryCommitteeByTxHashStmt :: HsqlStmt.Statement ByteString (Maybe SCG.Committee)
queryCommitteeByTxHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    committeeTableN = tableName (Proxy @SCG.Committee)
    govActionProposalTableN = tableName (Proxy @SCG.GovActionProposal)
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT committee.*"
          , " FROM " <> committeeTableN <> " committee"
          , " INNER JOIN " <> govActionProposalTableN <> " govAction ON committee.gov_action_proposal_id = govAction.id"
          , " INNER JOIN " <> txTableN <> " tx ON govAction.tx_id = tx.id"
          , " WHERE tx.hash = $1"
          , " LIMIT 1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe SCG.committeeDecoder

queryCommitteeByTxHash :: MonadIO m => ByteString -> DbAction m (Maybe SCG.Committee)
queryCommitteeByTxHash txHash =
  runDbSession (mkDbCallStack "queryCommitteeByTxHash") $
    HsqlSes.statement txHash queryCommitteeByTxHashStmt

------------------------------------------------------------------------------------------------

queryCommitteeMemberCountByTxHashStmt :: HsqlStmt.Statement (Maybe ByteString) Word64
queryCommitteeMemberCountByTxHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    committeeMemberTableN = tableName (Proxy @SCG.CommitteeMember)
    committeeTableN = tableName (Proxy @SCG.Committee)
    govActionProposalTableN = tableName (Proxy @SCG.GovActionProposal)
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> committeeMemberTableN <> " member"
          , " INNER JOIN " <> committeeTableN <> " committee ON member.committee_id = committee.id"
          , " LEFT JOIN " <> govActionProposalTableN <> " govAction ON committee.gov_action_proposal_id = govAction.id"
          , " LEFT JOIN " <> txTableN <> " tx ON govAction.tx_id = tx.id"
          , " WHERE CASE WHEN $1 IS NOT NULL THEN tx.hash = $1 ELSE committee.gov_action_proposal_id IS NULL END"
          ]
    encoder = HsqlE.param (HsqlE.nullable HsqlE.bytea)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryCommitteeMemberCountByTxHash :: MonadIO m => Maybe ByteString -> DbAction m Word64
queryCommitteeMemberCountByTxHash txHash =
  runDbSession (mkDbCallStack "queryCommitteeMemberCountByTxHash") $
    HsqlSes.statement txHash queryCommitteeMemberCountByTxHashStmt

------------------------------------------------------------------------------------------------

queryTestTxIdsStmt :: HsqlStmt.Statement () (Word64, Word64)
queryTestTxIdsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , "  COALESCE(MIN(id), 0) as lower_bound,"
          , "  COUNT(*) as upper_bound"
          , " FROM " <> txTableN
          , " WHERE block_id > 1"
          ]
    decoder = HsqlD.singleRow $ do
      lower <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      upper <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (lower, upper)

-- | Exclude all 'faked' generated TxId values from the genesis block (block_id == 1).
queryTestTxIds :: MonadIO m => DbAction m (Word64, Word64)
queryTestTxIds =
  runDbSession (mkDbCallStack "queryTestTxIds") $
    HsqlSes.statement () queryTestTxIdsStmt

------------------------------------------------------------------------------------------------

queryTxFeeDepositStmt :: HsqlStmt.Statement Word64 (Maybe (Ada, Int64))
queryTxFeeDepositStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT fee, deposit"
          , " FROM " <> txTableN
          , " WHERE id = $1"
          , " LIMIT 1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe $ do
      fee <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      deposit <- HsqlD.column (HsqlD.nullable HsqlD.int8)
      pure (word64ToAda fee, fromMaybe 0 deposit)

queryTxFeeDeposit :: MonadIO m => Word64 -> DbAction m (Ada, Int64)
queryTxFeeDeposit txId = do
  result <-
    runDbSession (mkDbCallStack "queryTxFeeDeposit") $
      HsqlSes.statement txId queryTxFeeDepositStmt
  pure $ fromMaybe (0, 0) result

------------------------------------------------------------------------------------------------

queryTxInputsCoreStmt :: HsqlStmt.Statement Word64 [SVC.TxOutCore]
queryTxInputsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    txInTableN = tableName (Proxy @SCB.TxIn)
    txOutTableN = tableName (Proxy @SVC.TxOutCore)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*"
          , " FROM " <> txTableN <> " tx"
          , " INNER JOIN " <> txInTableN <> " txin ON tx.id = txin.tx_in_id"
          , " INNER JOIN " <> txOutTableN <> " txout ON txin.tx_out_id = txout.tx_id"
          , " WHERE tx.id = $1"
          , " AND txout.index = txin.tx_out_index"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList SVC.txOutCoreDecoder

queryTxInputsAddressStmt :: HsqlStmt.Statement Word64 [SVA.TxOutAddress]
queryTxInputsAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    txInTableN = tableName (Proxy @SCB.TxIn)
    txOutTableN = tableName (Proxy @SVA.TxOutAddress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*"
          , " FROM " <> txTableN <> " tx"
          , " INNER JOIN " <> txInTableN <> " txin ON tx.id = txin.tx_in_id"
          , " INNER JOIN " <> txOutTableN <> " txout ON txin.tx_out_id = txout.tx_id"
          , " WHERE tx.id = $1"
          , " AND txout.index = txin.tx_out_index"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList SVA.txOutAddressDecoder

queryTxInputs :: MonadIO m => SV.TxOutVariantType -> Word64 -> DbAction m [SV.TxOutW]
queryTxInputs txOutTableType txId = do
  case txOutTableType of
    SV.TxOutVariantCore -> do
      cores <-
        runDbSession (mkDbCallStack "queryTxInputsCore") $
          HsqlSes.statement txId queryTxInputsCoreStmt
      pure $ map SV.VCTxOutW cores
    SV.TxOutVariantAddress -> do
      addresses <-
        runDbSession (mkDbCallStack "queryTxInputsAddress") $
          HsqlSes.statement txId queryTxInputsAddressStmt
      pure $ map (`SV.VATxOutW` Nothing) addresses

------------------------------------------------------------------------------------------------

queryTxOutputsCoreStmt :: HsqlStmt.Statement Word64 [SVC.TxOutCore]
queryTxOutputsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    txOutTableN = tableName (Proxy @SVC.TxOutCore)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*"
          , " FROM " <> txTableN <> " tx"
          , " INNER JOIN " <> txOutTableN <> " txout ON tx.id = txout.tx_id"
          , " WHERE tx.id = $1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList SVC.txOutCoreDecoder

queryTxOutputsAddressStmt :: HsqlStmt.Statement Word64 [SVA.TxOutAddress]
queryTxOutputsAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    txOutTableN = tableName (Proxy @SVA.TxOutAddress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*"
          , " FROM " <> txTableN <> " tx"
          , " INNER JOIN " <> txOutTableN <> " txout ON tx.id = txout.tx_id"
          , " WHERE tx.id = $1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList SVA.txOutAddressDecoder

queryTxOutputs :: MonadIO m => SV.TxOutVariantType -> Word64 -> DbAction m [SV.TxOutW]
queryTxOutputs txOutTableType txId = do
  case txOutTableType of
    SV.TxOutVariantCore -> do
      cores <-
        runDbSession (mkDbCallStack "queryTxOutputs TxOutVariantCore") $
          HsqlSes.statement txId queryTxOutputsCoreStmt
      pure $ map SV.VCTxOutW cores
    SV.TxOutVariantAddress -> do
      addresses <-
        runDbSession (mkDbCallStack "queryTxOutputs TxOutVariantAddress") $
          HsqlSes.statement txId queryTxOutputsAddressStmt
      pure $ map (`SV.VATxOutW` Nothing) addresses

------------------------------------------------------------------------------------------------

queryTxWithdrawalStmt :: HsqlStmt.Statement Word64 Ada
queryTxWithdrawalStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    withdrawalTableN = tableName (Proxy @SCB.Withdrawal)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(amount), 0)"
          , " FROM " <> withdrawalTableN
          , " WHERE tx_id = $1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow $ do
      amount <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure $ word64ToAda amount

-- | It is probably not possible to have two withdrawals in a single Tx.
-- If it is possible then there will be an accounting error.
queryTxWithdrawal :: MonadIO m => Word64 -> DbAction m Ada
queryTxWithdrawal txId =
  runDbSession (mkDbCallStack "queryTxWithdrawal") $
    HsqlSes.statement txId queryTxWithdrawalStmt

------------------------------------------------------------------------------------------------

queryRewardsWithStakeAddrStmt :: HsqlStmt.Statement (Maybe Word64) [(RewardSource, ByteString)]
queryRewardsWithStakeAddrStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    rewardTableN = tableName (Proxy @SCSD.Reward)
    stakeAddressTableN = tableName (Proxy @SCSD.StakeAddress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT reward.type, stake_addr.hash_raw"
          , " FROM " <> rewardTableN <> " reward"
          , " INNER JOIN " <> stakeAddressTableN <> " stake_addr ON reward.addr_id = stake_addr.id"
          , " WHERE ($1 IS NULL OR reward.spendable_epoch = $1)"
          ]

    encoder = HsqlE.param (HsqlE.nullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowList $ do
      rewardType <- HsqlD.column (HsqlD.nonNullable rewardSourceDecoder)
      hashRaw <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (rewardType, hashRaw)

queryRewardRestsWithStakeAddrStmt :: HsqlStmt.Statement (Maybe Word64) [(RewardSource, ByteString)]
queryRewardRestsWithStakeAddrStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    rewardRestTableN = tableName (Proxy @SCSD.RewardRest)
    stakeAddressTableN = tableName (Proxy @SCSD.StakeAddress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT ireward.type, stake_addr.hash_raw"
          , " FROM " <> rewardRestTableN <> " ireward"
          , " INNER JOIN " <> stakeAddressTableN <> " stake_addr ON ireward.addr_id = stake_addr.id"
          , " WHERE ($1 IS NULL OR ireward.spendable_epoch = $1)"
          ]

    encoder = HsqlE.param (HsqlE.nullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowList $ do
      rewardType <- HsqlD.column (HsqlD.nonNullable rewardSourceDecoder)
      hashRaw <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (rewardType, hashRaw)

queryRewardsAndRestsWithStakeAddr :: MonadIO m => Maybe Word64 -> DbAction m [(RewardSource, ByteString)]
queryRewardsAndRestsWithStakeAddr mEpoch = do
  res1 <-
    runDbSession (mkDbCallStack "queryRewardsWithStakeAddr") $
      HsqlSes.statement mEpoch queryRewardsWithStakeAddrStmt
  res2 <-
    runDbSession (mkDbCallStack "queryRewardRestsWithStakeAddr") $
      HsqlSes.statement mEpoch queryRewardRestsWithStakeAddrStmt
  pure (res1 <> res2)

------------------------------------------------------------------------------------------------
-- assertAddrValues counts
----------------------------------------------------------------------------------------------

queryStakeRegistrationCount :: MonadIO m => DbAction m Word64
queryStakeRegistrationCount =
  runDbSession (mkDbCallStack "countStakeRegistrations") $
    HsqlSes.statement () (countAll @SCSD.StakeRegistration)

queryStakeDeregistrationCount :: MonadIO m => DbAction m Word64
queryStakeDeregistrationCount =
  runDbSession (mkDbCallStack "countStakeDeregistrations") $
    HsqlSes.statement () (countAll @SCSD.StakeDeregistration)

queryDelegationCount :: MonadIO m => DbAction m Word64
queryDelegationCount =
  runDbSession (mkDbCallStack "countDelegations") $
    HsqlSes.statement () (countAll @SCSD.Delegation)

queryWithdrawalCount :: MonadIO m => DbAction m Word64
queryWithdrawalCount =
  runDbSession (mkDbCallStack "countWithdrawals") $
    HsqlSes.statement () (countAll @SCB.Withdrawal)

------------------------------------------------------------------------------------------------

queryEpochStakeCountGen :: MonadIO m => DbAction m Word64
queryEpochStakeCountGen =
  runDbSession (mkDbCallStack "queryEpochStakeCount") $
    HsqlSes.statement () (countAll @SCSD.EpochStake)

------------------------------------------------------------------------------------------------

queryEpochStakeByEpochCount :: MonadIO m => Word64 -> DbAction m Word64
queryEpochStakeByEpochCount epochNo =
  runDbSession (mkDbCallStack "queryEpochStakeByEpoch") $
    HsqlSes.statement epochNo (parameterisedCountWhere @SCSD.EpochStake "epoch_no" "= $1" encoder)
  where
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

------------------------------------------------------------------------------------------------

queryZeroFeeInvalidTxCount :: MonadIO m => DbAction m Word64
queryZeroFeeInvalidTxCount =
  runDbSession (mkDbCallStack "queryZeroFeeInvalidTx") $
    HsqlSes.statement () (countWhere @SCB.Tx "fee" "= 0 AND valid_contract = FALSE")

------------------------------------------------------------------------------------------------

queryDatumByBytesCount :: MonadIO m => ByteString -> DbAction m Word64
queryDatumByBytesCount bs =
  runDbSession (mkDbCallStack "queryDatumByBytes") $
    HsqlSes.statement bs (parameterisedCountWhere @SCB.Datum "bytes" "= $1" encoder)
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)

------------------------------------------------------------------------------------------------
-- assertAlonzoCounts/assertBabbageCounts counts
------------------------------------------------------------------------------------------------

queryScriptCount :: MonadIO m => DbAction m Word64
queryScriptCount =
  runDbSession (mkDbCallStack "countScripts") $
    HsqlSes.statement () (countAll @SCB.Script)

queryRedeemerCount :: MonadIO m => DbAction m Word64
queryRedeemerCount =
  runDbSession (mkDbCallStack "countRedeemers") $
    HsqlSes.statement () (countAll @SCB.Redeemer)

queryDatumCount :: MonadIO m => DbAction m Word64
queryDatumCount =
  runDbSession (mkDbCallStack "countDatums") $
    HsqlSes.statement () (countAll @SCB.Datum)

queryCollateralTxInCount :: MonadIO m => DbAction m Word64
queryCollateralTxInCount =
  runDbSession (mkDbCallStack "countCollateralTxIn") $
    HsqlSes.statement () (countAll @SCB.CollateralTxIn)

queryRedeemerDataCount :: MonadIO m => DbAction m Word64
queryRedeemerDataCount =
  runDbSession (mkDbCallStack "countRedeemerData") $
    HsqlSes.statement () (countAll @SCB.RedeemerData)

queryReferenceTxInCount :: MonadIO m => DbAction m Word64
queryReferenceTxInCount =
  runDbSession (mkDbCallStack "countReferenceTxIn") $
    HsqlSes.statement () (countAll @SCB.ReferenceTxIn)

queryCollateralTxOutCoreCount :: MonadIO m => DbAction m Word64
queryCollateralTxOutCoreCount =
  runDbSession (mkDbCallStack "countCollateralTxOutCore") $
    HsqlSes.statement () (countAll @SVC.CollateralTxOutCore)

queryCollateralTxOutAddressCount :: MonadIO m => DbAction m Word64
queryCollateralTxOutAddressCount =
  runDbSession (mkDbCallStack "countCollateralTxOutAddress") $
    HsqlSes.statement () (countAll @SVA.CollateralTxOutAddress)

queryInlineDatumCoreCount :: MonadIO m => DbAction m Word64
queryInlineDatumCoreCount =
  runDbSession (mkDbCallStack "countInlineDatumCore") $
    HsqlSes.statement () (countWhere @SVC.TxOutCore "inline_datum_id" "IS NOT NULL")

queryInlineDatumAddressCount :: MonadIO m => DbAction m Word64
queryInlineDatumAddressCount =
  runDbSession (mkDbCallStack "countInlineDatumAddress") $
    HsqlSes.statement () (countWhere @SVA.TxOutAddress "inline_datum_id" "IS NOT NULL")

queryReferenceScriptCoreCount :: MonadIO m => DbAction m Word64
queryReferenceScriptCoreCount =
  runDbSession (mkDbCallStack "countReferenceScriptCore") $
    HsqlSes.statement () (countWhere @SVC.TxOutCore "reference_script_id" "IS NOT NULL")

queryReferenceScriptAddressCount :: MonadIO m => DbAction m Word64
queryReferenceScriptAddressCount =
  runDbSession (mkDbCallStack "countReferenceScriptAddress") $
    HsqlSes.statement () (countWhere @SVA.TxOutAddress "reference_script_id" "IS NOT NULL")

------------------------------------------------------------------------------------------------
-- poolCountersQuery counts
------------------------------------------------------------------------------------------------

queryPoolHashCount :: MonadIO m => DbAction m Word64
queryPoolHashCount =
  runDbSession (mkDbCallStack "countPoolHash") $
    HsqlSes.statement () (countAll @SCP.PoolHash)

queryPoolMetadataRefCount :: MonadIO m => DbAction m Word64
queryPoolMetadataRefCount =
  runDbSession (mkDbCallStack "countPoolMetadataRef") $
    HsqlSes.statement () (countAll @SCP.PoolMetadataRef)

queryPoolUpdateCount :: MonadIO m => DbAction m Word64
queryPoolUpdateCount =
  runDbSession (mkDbCallStack "countPoolUpdate") $
    HsqlSes.statement () (countAll @SCP.PoolUpdate)

queryPoolOwnerCount :: MonadIO m => DbAction m Word64
queryPoolOwnerCount =
  runDbSession (mkDbCallStack "countPoolOwner") $
    HsqlSes.statement () (countAll @SCP.PoolOwner)

queryPoolRetireCount :: MonadIO m => DbAction m Word64
queryPoolRetireCount =
  runDbSession (mkDbCallStack "countPoolRetire") $
    HsqlSes.statement () (countAll @SCP.PoolRetire)

queryPoolRelayCount :: MonadIO m => DbAction m Word64
queryPoolRelayCount =
  runDbSession (mkDbCallStack "countPoolRelay") $
    HsqlSes.statement () (countAll @SCP.PoolRelay)

------------------------------------------------------------------------------
-- Database Column Order Information
------------------------------------------------------------------------------

data ColumnInfo = ColumnInfo
  { columnName :: !Text
  , ordinalPosition :: !Int
  }
  deriving (Show, Eq)

-- | Simple column comparison result
data ColumnComparisonResult = ColumnComparisonResult
  { ccrTableName :: !Text
  , ccrTypeName :: !Text
  , ccrExpectedColumns :: ![Text] -- From columnNames
  , ccrDatabaseColumns :: ![Text] -- From database ordinal_position order
  }
  deriving (Show, Eq)

-- | Get the actual column order from the database
getTableColumnOrderStmt :: Text -> HsqlStmt.Statement () [ColumnInfo]
getTableColumnOrderStmt tableN =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT column_name, ordinal_position "
          , "FROM information_schema.columns "
          , "WHERE table_name = '" <> tableN <> "' "
          , "ORDER BY ordinal_position"
          ]
    decoder = HsqlD.rowList columnInfoDecoder

columnInfoDecoder :: HsqlD.Row ColumnInfo
columnInfoDecoder =
  ColumnInfo
    <$> HsqlD.column (HsqlD.nonNullable HsqlD.text)
    <*> HsqlD.column (HsqlD.nonNullable (fromIntegral <$> HsqlD.int4))

------------------------------------------------------------------------------
-- Main Query Function
------------------------------------------------------------------------------

-- | Compare expected columns with actual database columns
queryTableColumns :: forall a m. (MonadIO m, DbInfo a) => Proxy a -> DbAction m ColumnComparisonResult
queryTableColumns proxy = do
  let table = tableName proxy
      typeName = Text.pack $ show (typeRep proxy)
      expectedCols = NE.toList $ columnNames proxy

  -- Get actual database column order
  columnInfos <-
    runDbSession (mkDbCallStack "queryTableColumns") $
      HsqlSes.statement () (getTableColumnOrderStmt table)

  let allDbCols = map columnName columnInfos
  -- Remove "id" column if present (it's not in columnNames)
  let dbCols = filter (/= "id") allDbCols

  pure $
    ColumnComparisonResult
      { ccrTableName = table
      , ccrExpectedColumns = expectedCols
      , ccrTypeName = typeName
      , ccrDatabaseColumns = dbCols
      }
