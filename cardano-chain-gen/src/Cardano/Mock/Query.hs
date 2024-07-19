{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Query (
  queryVersionMajorFromEpoch,
  queryParamProposalFromEpoch,
  queryNullTxDepositExists,
  queryMultiAssetCount,
  queryTxMetadataCount,
  queryDRepDistrAmount,
  queryGovActionCounts,
  queryConstitutionAnchor,
  queryRewardRests,
  queryCollateralTxOutCount,
  queryMultiAssetMetadataPolicy,
  queryPoolUpdateCount,
  queryStakeAddressCount,
  queryStakeAddressHashRaw,
  queryStakeDeRegCount,
  queryStakeRegCount,
  queryTreasuryDonations,
  countTxOutNonNullStakeAddrIds,
) where

import qualified Cardano.Db as Db
import Cardano.Prelude hiding (from, isNothing, on)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short (ShortByteString, toShort)
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

queryMultiAssetMetadataPolicy :: MonadIO io => ReaderT SqlBackend io (Maybe ShortByteString)
queryMultiAssetMetadataPolicy = do
  res <- selectOne $ do
    metadataPolicy <- from $ table @Db.MultiAsset
    pure $ metadataPolicy ^. Db.MultiAssetPolicy
  pure $ toShort . Base16.encode . unValue <$> res

queryStakeAddressHashRaw :: MonadIO io => ReaderT SqlBackend io (Maybe ShortByteString)
queryStakeAddressHashRaw = do
  res <- selectOne $ do
    stakeAddress <- from $ table @Db.StakeAddress
    pure $ stakeAddress ^. Db.StakeAddressHashRaw
  pure $ toShort . Base16.encode . unValue <$> res

queryStakeAddressCount :: MonadIO io => ReaderT SqlBackend io Word
queryStakeAddressCount = do
  res <- selectOne $ do
    _ <- from (table @Db.StakeAddress)
    pure countRows
  pure $ maybe 0 unValue res

queryCollateralTxOutCount :: MonadIO io => ReaderT SqlBackend io Word
queryCollateralTxOutCount = do
  res <- selectOne $ do
    _ <- from (table @Db.CollateralTxOut)
    pure countRows
  pure $ maybe 0 unValue res

queryPoolUpdateCount :: MonadIO io => ReaderT SqlBackend io Word
queryPoolUpdateCount = do
  res <- selectOne $ do
    _ <- from (table @Db.PoolUpdate)
    pure countRows
  pure $ maybe 0 unValue res

queryStakeDeRegCount :: MonadIO io => ReaderT SqlBackend io Word
queryStakeDeRegCount = do
  res <- selectOne $ do
    _ <- from (table @Db.StakeDeregistration)
    pure countRows
  pure $ maybe 0 unValue res

queryStakeRegCount :: MonadIO io => ReaderT SqlBackend io Word
queryStakeRegCount = do
  res <- selectOne $ do
    _ <- from (table @Db.StakeRegistration)
    pure countRows
  pure $ maybe 0 unValue res

countTxOutNonNullStakeAddrIds :: (MonadIO m) => SqlPersistT m Word
countTxOutNonNullStakeAddrIds = do
  result <- selectOne $ do
    txOut <- from $ table @Db.TxOut
    where_ $ not_ (isNothing $ txOut ^. Db.TxOutStakeAddressId)
    pure countRows
  pure $ maybe 0 unValue result
