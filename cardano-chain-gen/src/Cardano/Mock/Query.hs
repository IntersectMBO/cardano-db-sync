{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Query (
  queryVersionMajorFromEpoch,
  queryParamProposalFromEpoch,
  queryParamFromEpoch,
  queryNullTxDepositExists,
  queryMultiAssetCount,
  queryTxMetadataCount,
  queryDRepDistrAmount,
  queryGovActionCounts,
  queryConstitutionAnchor,
  queryRewardRests,
  queryTreasuryDonations,
  queryVoteCounts,
) where

import qualified Cardano.Db as Db
import Cardano.Prelude hiding (from, on)
import Database.Esqueleto.Experimental
import Prelude ()

-- | Query protocol parameters from @EpochParam@ by epoch number. Note that epoch
--   parameters are inserted at the beginning of the next epoch.
--
-- TODO[sgillespie]: It would probably be better to return @Db.EpochParam@, but
-- persistent seems to be having trouble with the data:
--
--     PersistMarshalError "Couldn't parse field `govActionLifetime` from table
--     `epoch_param`. Failed to parse Haskell type `Word64`; expected integer from
--     database, but received: PersistRational (0 % 1).
queryVersionMajorFromEpoch ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe Word16)
queryVersionMajorFromEpoch epochNo = do
  res <- selectOne $ do
    prop <- from $ table @Db.EpochParam
    where_ (prop ^. Db.EpochParamEpochNo ==. val epochNo)
    pure (prop ^. Db.EpochParamProtocolMajor)
  pure $ unValue <$> res

-- | Query protocol parameter proposals from @ParamProposal@ by epoch number.
queryParamProposalFromEpoch ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe Db.ParamProposal)
queryParamProposalFromEpoch epochNo = do
  res <- selectOne $ do
    prop <- from $ table @Db.ParamProposal
    where_ $ prop ^. Db.ParamProposalEpochNo ==. val (Just epochNo)
    pure prop
  pure $ entityVal <$> res

queryParamFromEpoch ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe Db.EpochParam)
queryParamFromEpoch epochNo = do
  res <- selectOne $ do
    param <- from $ table @Db.EpochParam
    where_ $ param ^. Db.EpochParamEpochNo ==. val epochNo
    pure param
  pure (entityVal <$> res)

-- | Query whether there any null tx deposits?
queryNullTxDepositExists :: MonadIO io => ReaderT SqlBackend io Bool
queryNullTxDepositExists = do
  res <- select $ do
    tx <- from $ table @Db.Tx
    where_ $ isNothing_ (tx ^. Db.TxDeposit)
  pure $ not (null res)

queryMultiAssetCount :: MonadIO io => ReaderT SqlBackend io Word
queryMultiAssetCount = do
  res <- select $ do
    _ <- from (table @Db.MultiAsset)
    pure countRows

  pure $ maybe 0 unValue (listToMaybe res)

queryTxMetadataCount :: MonadIO io => ReaderT SqlBackend io Word
queryTxMetadataCount = do
  res <- selectOne $ do
    _ <- from (table @Db.TxMetadata)
    pure countRows

  pure $ maybe 0 unValue res

queryDRepDistrAmount ::
  MonadIO io =>
  ByteString ->
  Word64 ->
  ReaderT SqlBackend io Word64
queryDRepDistrAmount drepHash epochNo = do
  res <- selectOne $ do
    (distr :& hash) <-
      from
        $ table @Db.DrepDistr
          `innerJoin` table @Db.DrepHash
        `on` (\(distr :& hash) -> (hash ^. Db.DrepHashId) ==. (distr ^. Db.DrepDistrHashId))

    where_ $ hash ^. Db.DrepHashRaw ==. just (val drepHash)
    where_ $ distr ^. Db.DrepDistrEpochNo ==. val epochNo

    pure (distr ^. Db.DrepDistrAmount)

  pure $ maybe 0 unValue res

queryGovActionCounts ::
  MonadIO io =>
  ReaderT SqlBackend io (Word, Word, Word, Word)
queryGovActionCounts = do
  ratified <- countNonNulls Db.GovActionProposalRatifiedEpoch
  enacted <- countNonNulls Db.GovActionProposalEnactedEpoch
  dropped <- countNonNulls Db.GovActionProposalDroppedEpoch
  expired <- countNonNulls Db.GovActionProposalExpiredEpoch

  pure (ratified, enacted, dropped, expired)
  where
    countNonNulls ::
      (MonadIO io, PersistField field) =>
      EntityField Db.GovActionProposal (Maybe field) ->
      ReaderT SqlBackend io Word
    countNonNulls field = do
      res <- selectOne $ do
        e <- from $ table @Db.GovActionProposal
        where_ $ not_ (isNothing_ (e ^. field))
        pure countRows

      pure (maybe 0 unValue res)

queryConstitutionAnchor ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe (Text, ByteString))
queryConstitutionAnchor epochNo = do
  res <- selectOne $ do
    (_ :& anchor :& epochState) <-
      from
        $ table @Db.Constitution
          `innerJoin` table @Db.VotingAnchor
        `on` ( \(constit :& anchor) ->
                (constit ^. Db.ConstitutionVotingAnchorId) ==. (anchor ^. Db.VotingAnchorId)
             )
          `innerJoin` table @Db.EpochState
        `on` ( \(constit :& _ :& epoch) ->
                just (constit ^. Db.ConstitutionId) ==. (epoch ^. Db.EpochStateConstitutionId)
             )

    where_ (epochState ^. Db.EpochStateEpochNo ==. val epochNo)

    pure (anchor ^. Db.VotingAnchorUrl, anchor ^. Db.VotingAnchorDataHash)

  pure $ bimap (Db.unVoteUrl . unValue) unValue <$> res

queryRewardRests ::
  MonadIO io =>
  ReaderT SqlBackend io [(Db.RewardSource, Word64)]
queryRewardRests = do
  res <- select $ do
    reward <- from $ table @Db.RewardRest
    pure (reward ^. Db.RewardRestType, reward ^. Db.RewardRestAmount)

  pure $ map (bimap unValue (Db.unDbLovelace . unValue)) res

queryTreasuryDonations ::
  MonadIO io =>
  ReaderT SqlBackend io Word64
queryTreasuryDonations = do
  res <- selectOne $ do
    txs <- from $ table @Db.Tx
    pure $ sum_ (txs ^. Db.TxTreasuryDonation)

  let total = join (unValue <$> res)
  pure $ maybe 0 Db.unDbLovelace total

queryVoteCounts ::
  MonadIO io =>
  ByteString ->
  Word16 ->
  ReaderT SqlBackend io (Word64, Word64, Word64)
queryVoteCounts txHash idx = do
  yes <- countVotes Db.VoteYes
  no <- countVotes Db.VoteNo
  abstain <- countVotes Db.VoteAbstain

  pure (yes, no, abstain)
  where
    countVotes v = do
      res <- selectOne $ do
        (vote :& tx) <-
          from
            $ table @Db.VotingProcedure
              `innerJoin` table @Db.Tx
            `on` (\(vote :& tx) -> vote ^. Db.VotingProcedureTxId ==. tx ^. Db.TxId)
        where_ $
          vote ^. Db.VotingProcedureVote ==. val v
            &&. tx ^. Db.TxHash ==. val txHash
            &&. vote ^. Db.VotingProcedureIndex ==. val idx
        pure countRows
      pure (maybe 0 unValue res)
