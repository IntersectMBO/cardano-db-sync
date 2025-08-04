{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.ChainGen where

import Cardano.Prelude hiding (from, isNothing, map, on)
import Data.Functor.Contravariant ((>$<))
import qualified Data.List.NonEmpty as NE
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt
import Prelude hiding (length, show, (.))

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SCE
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SCG
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Core.MultiAsset as MultiAsset
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Core.StakeDelegation as SCSD
import qualified Cardano.Db.Schema.Variants as SV
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Function.Core (runSession, runSessionEntity)
import Cardano.Db.Statement.Function.Query (countAll, countWhere, parameterisedCountWhere)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), tableName)
import Cardano.Db.Types (Ada, DbM, RewardSource, rewardSourceDecoder, word64ToAda)

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
queryEpochParamWithEpochNo :: Word64 -> DbM (Maybe SCE.EpochParam)
queryEpochParamWithEpochNo epochNo =
  runSessionEntity $ HsqlSes.statement epochNo queryEpochParamWithEpochNoStmt

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
queryParamProposalWithEpochNo :: Word64 -> DbM (Maybe SGV.ParamProposal)
queryParamProposalWithEpochNo epochNo =
  runSessionEntity $ HsqlSes.statement epochNo queryParamProposalWithEpochNoStmt

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

queryParamWithEpochNo :: Word64 -> DbM (Maybe SCE.EpochParam)
queryParamWithEpochNo epochNo =
  runSessionEntity $ HsqlSes.statement epochNo queryParamWithEpochNoStmt

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
queryNullTxDepositExists :: DbM Bool
queryNullTxDepositExists =
  runSession $
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

queryMultiAssetCount :: DbM Word
queryMultiAssetCount =
  runSession $
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

queryTxMetadataCount :: DbM Word
queryTxMetadataCount =
  runSession $
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

queryDRepDistrAmount :: ByteString -> Word64 -> DbM Word64
queryDRepDistrAmount drepHash epochNo = do
  result <-
    runSession $
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

queryGovActionCounts :: DbM (Word, Word, Word, Word)
queryGovActionCounts =
  runSession $
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

queryConstitutionAnchor :: Word64 -> DbM (Maybe (Text, ByteString))
queryConstitutionAnchor epochNo =
  runSession $
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

queryRewardRests :: DbM [(RewardSource, Word64)]
queryRewardRests =
  runSession $
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

queryTreasuryDonations :: DbM Word64
queryTreasuryDonations =
  runSession $
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

queryVoteCounts :: ByteString -> Word16 -> DbM (Word64, Word64, Word64)
queryVoteCounts txHash idx =
  runSession $
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

queryEpochStateCount :: Word64 -> DbM Word64
queryEpochStateCount epochNo =
  runSession $
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

queryCommitteeByTxHash :: ByteString -> DbM (Maybe SCG.Committee)
queryCommitteeByTxHash txHash =
  runSession $
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

queryCommitteeMemberCountByTxHash :: Maybe ByteString -> DbM Word64
queryCommitteeMemberCountByTxHash txHash =
  runSession $
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
queryTestTxIds :: DbM (Word64, Word64)
queryTestTxIds =
  runSession $
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

queryTxFeeDeposit :: Word64 -> DbM (Ada, Int64)
queryTxFeeDeposit txId = do
  result <- runSession $ HsqlSes.statement txId queryTxFeeDepositStmt
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

queryTxInputs :: SV.TxOutVariantType -> Word64 -> DbM [SV.TxOutW]
queryTxInputs txOutTableType txId = do
  case txOutTableType of
    SV.TxOutVariantCore -> do
      cores <-
        runSession $
          HsqlSes.statement txId queryTxInputsCoreStmt
      pure $ map SV.VCTxOutW cores
    SV.TxOutVariantAddress -> do
      addresses <-
        runSession $
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

queryTxOutputs :: SV.TxOutVariantType -> Word64 -> DbM [SV.TxOutW]
queryTxOutputs txOutTableType txId = do
  case txOutTableType of
    SV.TxOutVariantCore -> do
      cores <-
        runSession $
          HsqlSes.statement txId queryTxOutputsCoreStmt
      pure $ map SV.VCTxOutW cores
    SV.TxOutVariantAddress -> do
      addresses <-
        runSession $
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
queryTxWithdrawal :: Word64 -> DbM Ada
queryTxWithdrawal txId =
  runSession $ HsqlSes.statement txId queryTxWithdrawalStmt

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

queryRewardsAndRestsWithStakeAddr :: Maybe Word64 -> DbM [(RewardSource, ByteString)]
queryRewardsAndRestsWithStakeAddr mEpoch = do
  res1 <-
    runSession $
      HsqlSes.statement mEpoch queryRewardsWithStakeAddrStmt
  res2 <-
    runSession $
      HsqlSes.statement mEpoch queryRewardRestsWithStakeAddrStmt
  pure (res1 <> res2)

------------------------------------------------------------------------------------------------
-- assertAddrValues counts
----------------------------------------------------------------------------------------------

queryStakeRegistrationCount :: DbM Word64
queryStakeRegistrationCount =
  runSession $
    HsqlSes.statement () (countAll @SCSD.StakeRegistration)

queryStakeDeregistrationCount :: DbM Word64
queryStakeDeregistrationCount =
  runSession $
    HsqlSes.statement () (countAll @SCSD.StakeDeregistration)

queryDelegationCount :: DbM Word64
queryDelegationCount =
  runSession $
    HsqlSes.statement () (countAll @SCSD.Delegation)

queryWithdrawalCount :: DbM Word64
queryWithdrawalCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.Withdrawal)

------------------------------------------------------------------------------------------------

queryEpochStakeCountGen :: DbM Word64
queryEpochStakeCountGen =
  runSession $
    HsqlSes.statement () (countAll @SCSD.EpochStake)

------------------------------------------------------------------------------------------------

queryEpochStakeByEpochCount :: Word64 -> DbM Word64
queryEpochStakeByEpochCount epochNo =
  runSession $
    HsqlSes.statement epochNo (parameterisedCountWhere @SCSD.EpochStake "epoch_no" "= $1" encoder)
  where
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

------------------------------------------------------------------------------------------------

queryZeroFeeInvalidTxCount :: DbM Word64
queryZeroFeeInvalidTxCount =
  runSession $
    HsqlSes.statement () (countWhere @SCB.Tx "fee" "= 0 AND valid_contract = FALSE")

------------------------------------------------------------------------------------------------

queryDatumByBytesCount :: ByteString -> DbM Word64
queryDatumByBytesCount bs =
  runSession $
    HsqlSes.statement bs (parameterisedCountWhere @SCB.Datum "bytes" "= $1" encoder)
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)

------------------------------------------------------------------------------------------------
-- assertAlonzoCounts/assertBabbageCounts counts
------------------------------------------------------------------------------------------------

queryScriptCount :: DbM Word64
queryScriptCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.Script)

queryRedeemerCount :: DbM Word64
queryRedeemerCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.Redeemer)

queryDatumCount :: DbM Word64
queryDatumCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.Datum)

queryCollateralTxInCount :: DbM Word64
queryCollateralTxInCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.CollateralTxIn)

queryRedeemerDataCount :: DbM Word64
queryRedeemerDataCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.RedeemerData)

queryReferenceTxInCount :: DbM Word64
queryReferenceTxInCount =
  runSession $
    HsqlSes.statement () (countAll @SCB.ReferenceTxIn)

queryCollateralTxOutCoreCount :: DbM Word64
queryCollateralTxOutCoreCount =
  runSession $
    HsqlSes.statement () (countAll @SVC.CollateralTxOutCore)

queryCollateralTxOutAddressCount :: DbM Word64
queryCollateralTxOutAddressCount =
  runSession $
    HsqlSes.statement () (countAll @SVA.CollateralTxOutAddress)

queryInlineDatumCoreCount :: DbM Word64
queryInlineDatumCoreCount =
  runSession $
    HsqlSes.statement () (countWhere @SVC.TxOutCore "inline_datum_id" "IS NOT NULL")

queryInlineDatumAddressCount :: DbM Word64
queryInlineDatumAddressCount =
  runSession $
    HsqlSes.statement () (countWhere @SVA.TxOutAddress "inline_datum_id" "IS NOT NULL")

queryReferenceScriptCoreCount :: DbM Word64
queryReferenceScriptCoreCount =
  runSession $
    HsqlSes.statement () (countWhere @SVC.TxOutCore "reference_script_id" "IS NOT NULL")

queryReferenceScriptAddressCount :: DbM Word64
queryReferenceScriptAddressCount =
  runSession $
    HsqlSes.statement () (countWhere @SVA.TxOutAddress "reference_script_id" "IS NOT NULL")

------------------------------------------------------------------------------------------------
-- poolCountersQuery counts
------------------------------------------------------------------------------------------------

queryPoolHashCount :: DbM Word64
queryPoolHashCount =
  runSession $
    HsqlSes.statement () (countAll @SCP.PoolHash)

queryPoolMetadataRefCount :: DbM Word64
queryPoolMetadataRefCount =
  runSession $
    HsqlSes.statement () (countAll @SCP.PoolMetadataRef)

queryPoolUpdateCount :: DbM Word64
queryPoolUpdateCount =
  runSession $
    HsqlSes.statement () (countAll @SCP.PoolUpdate)

queryPoolOwnerCount :: DbM Word64
queryPoolOwnerCount =
  runSession $
    HsqlSes.statement () (countAll @SCP.PoolOwner)

queryPoolRetireCount :: DbM Word64
queryPoolRetireCount =
  runSession $
    HsqlSes.statement () (countAll @SCP.PoolRetire)

queryPoolRelayCount :: DbM Word64
queryPoolRelayCount =
  runSession $
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
queryTableColumns :: forall a. DbInfo a => Proxy a -> DbM ColumnComparisonResult
queryTableColumns proxy = do
  let table = tableName proxy
      typeName = Text.pack $ show (typeRep proxy)
      expectedCols = NE.toList $ columnNames proxy

  -- Get actual database column order
  columnInfos <-
    runSession $
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
