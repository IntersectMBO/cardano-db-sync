{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Operations.Query (
  LookupFail (..),
  -- queries used by db-sync
  queryBlockCount,
  queryBlockCountAfterBlockNo,
  queryBlockHashBlockNo,
  queryBlockNo,
  queryBlockNoAndEpoch,
  queryNearestBlockSlotNo,
  queryBlockHash,
  queryReverseIndexBlockId,
  queryMinIdsAfterReverseIndex,
  queryBlockTxCount,
  queryBlockId,
  queryCalcEpochEntry,
  queryCurrentEpochNo,
  queryNormalEpochRewardCount,
  queryGenesis,
  queryLatestBlock,
  queryLatestPoints,
  queryLatestEpochNo,
  queryLatestBlockId,
  queryLatestSlotNo,
  queryMeta,
  queryCountSlotNosGreaterThan,
  queryCountSlotNo,
  queryScript,
  queryDatum,
  queryRedeemerData,
  querySlotHash,
  queryMultiAssetId,
  queryTxCount,
  queryTxId,
  queryEpochFromNum,
  queryEpochStakeCount,
  queryForEpochId,
  queryLatestEpoch,
  queryMinRefId,
  queryMinRefIdNullable,
  queryMaxRefId,
  existsPoolHashId,
  existsPoolMetadataRefId,
  existsVotingAnchorId,
  queryAdaPotsId,
  queryBlockHeight,
  queryAllExtraMigrations,
  queryMinMaxEpochStake,
  queryGovActionProposalId,
  queryDrepHashAlwaysAbstain,
  queryDrepHashAlwaysNoConfidence,
  queryCommitteeHash,
  queryProposalConstitution,
  queryProposalCommittee,
  queryPoolHashId,
  queryStakeAddress,
  queryStakeRefPtr,
  queryPoolUpdateByBlock,
  -- queries used in smash
  queryOffChainPoolData,
  queryPoolRegister,
  queryRetiredPools,
  queryUsedTicker,
  queryReservedTicker,
  queryReservedTickers,
  queryDelistedPools,
  queryOffChainPoolFetchError,
  existsDelistedPool,
  -- queries used in tools
  queryDepositUpToBlockNo,
  queryEpochEntry,
  queryFeesUpToBlockNo,
  queryFeesUpToSlotNo,
  queryLatestCachedEpochNo,
  queryLatestBlockNo,
  querySlotNosGreaterThan,
  querySlotNos,
  querySlotUtcTime,
  queryWithdrawalsUpToBlockNo,
  queryAdaPots,
  -- queries used only in tests
  queryRewardCount,
  queryRewardRestCount,
  queryTxInCount,
  queryEpochCount,
  queryCostModel,
  queryTxInRedeemer,
  queryTxInFailedTx,
  queryInvalidTx,
  queryDeregistrationScript,
  queryDelegationScript,
  queryWithdrawalScript,
  queryStakeAddressScript,
  querySchemaVersion,
  queryPreviousSlotNo,
  queryMinBlock,
  -- utils
  listToMaybe,
) where

import Cardano.Db.Error
import Cardano.Db.Operations.QueryHelper (defaultUTCTime, isJust, maybeToEither, unValue2, unValue3, unValue5, unValueSumAda)
import Cardano.Db.Schema.BaseSchema
import Cardano.Db.Types
import Cardano.Ledger.BaseTypes (CertIx (..), TxIx (..))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Extra (join, whenJust)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ratio (numerator)
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime (..))
import Data.Tuple.Extra (uncurry3)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (
  Entity (..),
  PersistEntity,
  PersistField,
  SqlBackend,
  Value (Value, unValue),
  asc,
  count,
  countRows,
  desc,
  entityKey,
  entityVal,
  from,
  in_,
  innerJoin,
  isNothing,
  just,
  leftJoin,
  limit,
  max_,
  min_,
  on,
  orderBy,
  persistIdField,
  select,
  selectOne,
  sum_,
  table,
  val,
  valList,
  where_,
  (&&.),
  (<.),
  (<=.),
  (==.),
  (>.),
  (>=.),
  (?.),
  (^.),
  (||.),
  type (:&) ((:&)),
 )
import Database.Persist.Class.PersistQuery (selectList)
import Database.Persist.Types (SelectOpt (Asc))

{- HLINT ignore "Redundant ^." -}
{- HLINT ignore "Fuse on/on" -}
{- HLINT ignore "Reduce duplication" -}

-- If you squint, these Esqueleto queries almost look like SQL queries.
--
-- Queries in this module are split in a hierchical order. First queries that are used by db-sync
-- during syncing, then by smash, by tools and finally by test. This is useful to make sure we have
-- all the necessary indexes during syncing, but not more than that, based on the queries db-sync
-- does.

-- | Count the number of blocks in the Block table.
queryBlockCount :: MonadIO m => ReaderT SqlBackend m Word
queryBlockCount = do
  res <- select $ do
    _blk <- from $ table @Block
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of blocks in the Block table after a 'BlockNo'.
queryBlockCountAfterBlockNo :: MonadIO m => Word64 -> Bool -> ReaderT SqlBackend m Word
queryBlockCountAfterBlockNo blockNo queryEq = do
  res <- select $ do
    blk <- from $ table @Block
    where_
      ( if queryEq
          then blk ^. BlockBlockNo >=. just (val (fromIntegral blockNo))
          else blk ^. BlockBlockNo >. just (val (fromIntegral blockNo))
      )
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the 'BlockNo' associated with the given hash.
queryBlockHashBlockNo :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (Maybe Word64))
queryBlockHashBlockNo hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val hash)
    pure $ blk ^. BlockBlockNo
  pure $ maybeToEither (DbLookupBlockHash hash) unValue (listToMaybe res)

queryBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockNo blkNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo ==. just (val blkNo))
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

queryBlockNoAndEpoch :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe (BlockId, Word64))
queryBlockNoAndEpoch blkNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo ==. just (val blkNo))
    pure (blk ^. BlockId, blk ^. BlockEpochNo)
  pure $ convertBlockQuery (listToMaybe res)

-- | Retrieves the nearest block with a slot number equal to or greater than the given slot number.
queryNearestBlockSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe (BlockId, Word64))
queryNearestBlockSlotNo slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isNothing (blk ^. BlockSlotNo) ||. blk ^. BlockSlotNo >=. just (val slotNo))
    orderBy [asc (blk ^. BlockSlotNo)]
    limit 1
    pure (blk ^. BlockId, blk ^. BlockBlockNo)
  pure $ convertBlockQuery (listToMaybe res)

queryBlockHash :: MonadIO m => Block -> ReaderT SqlBackend m (Maybe (BlockId, Word64))
queryBlockHash hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val (blockHash hash))
    pure (blk ^. BlockId, blk ^. BlockEpochNo)
  pure $ convertBlockQuery (listToMaybe res)

queryMinBlock :: MonadIO m => ReaderT SqlBackend m (Maybe (BlockId, Word64))
queryMinBlock = do
  res <- select $ do
    blk <- from $ table @Block
    orderBy [asc (blk ^. BlockId)]
    limit 1
    pure (blk ^. BlockId, blk ^. BlockBlockNo)
  pure $ convertBlockQuery (listToMaybe res)

convertBlockQuery :: Maybe (Value (Key Block), Value (Maybe Word64)) -> Maybe (BlockId, Word64)
convertBlockQuery mr =
  case mr of
    Nothing -> Nothing
    Just (_, Value Nothing) -> Nothing -- Should never happen.
    Just (Value blkid, Value (Just epoch)) -> Just (blkid, epoch)

queryReverseIndexBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m [Maybe Text]
queryReverseIndexBlockId blockId = do
  res <- select $ do
    (blk :& ridx) <-
      from
        $ table @Block
          `leftJoin` table @ReverseIndex
        `on` (\(blk :& ridx) -> just (blk ^. BlockId) ==. ridx ?. ReverseIndexBlockId)
    where_ (blk ^. BlockId >=. val blockId)
    orderBy [asc (blk ^. BlockId)]
    pure $ ridx ?. ReverseIndexMinIds
  pure $ fmap unValue res

queryMinIdsAfterReverseIndex :: MonadIO m => ReverseIndexId -> ReaderT SqlBackend m [Text]
queryMinIdsAfterReverseIndex rollbackId = do
  res <- select $ do
    rl <- from $ table @ReverseIndex
    where_ (rl ^. ReverseIndexId >=. val rollbackId)
    orderBy [desc (rl ^. ReverseIndexId)]
    pure $ rl ^. ReverseIndexMinIds
  pure $ fmap unValue res

-- | Get the number of transactions in the specified block.
queryBlockTxCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word64
queryBlockTxCount blkId = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxBlockId ==. val blkId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the 'BlockId' associated with the given hash.
queryBlockId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail BlockId)
queryBlockId hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val hash)
    pure $ blk ^. BlockId
  pure $ maybeToEither (DbLookupBlockHash hash) unValue (listToMaybe res)

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epoch entry for the last epoch.
queryCalcEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m Epoch
queryCalcEpochEntry epochNum = do
  blockResult <- select $ do
    block <- from $ table @Block
    where_ (block ^. BlockEpochNo ==. just (val epochNum))
    pure (countRows, min_ (block ^. BlockTime), max_ (block ^. BlockTime))
  queryTxWithBlocks epochNum blockResult

-- | Get the PostgreSQL row index (EpochId) that matches the given epoch number.
queryForEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe EpochId)
queryForEpochId epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @Epoch
    where_ (epoch ^. EpochNo ==. val epochNum)
    pure (epoch ^. EpochId)
  pure $ unValue <$> res

-- | Get an epoch given it's number.
queryEpochFromNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Epoch)
queryEpochFromNum epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @Epoch
    where_ (epoch ^. EpochNo ==. val epochNum)
    pure epoch
  pure $ entityVal <$> res

-- | Get the most recent epoch in the Epoch DB table.
queryLatestEpoch :: MonadIO m => ReaderT SqlBackend m (Maybe Epoch)
queryLatestEpoch = do
  res <- selectOne $ do
    epoch <- from $ table @Epoch
    orderBy [desc (epoch ^. EpochNo)]
    pure epoch
  pure $ entityVal <$> res

-- | Count the number of epochs in Epoch table.
queryEpochCount :: MonadIO m => ReaderT SqlBackend m Word
queryEpochCount = do
  res <- select $ from (table @Epoch) >> pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryTxWithBlocks ::
  MonadIO m =>
  Word64 ->
  [(Value Word64, Value (Maybe UTCTime), Value (Maybe UTCTime))] ->
  ReaderT SqlBackend m Epoch
queryTxWithBlocks epochNum blockResult = do
  txRes <- select $ do
    (tx :& blk) <-
      from
        $ table @Tx
          `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (blk ^. BlockEpochNo ==. just (val epochNum))
    pure (sum_ (tx ^. TxOutSum), sum_ (tx ^. TxFee), count (tx ^. TxOutSum))
  case (listToMaybe blockResult, listToMaybe txRes) of
    (Just blk, Just tx) -> pure $ parseAndCalulateNewEpoch epochNum (unValue3 blk) (unValue3 tx)
    (Just blk, Nothing) -> pure $ convertBlk epochNum (unValue3 blk)
    _otherwise -> pure $ emptyEpoch epochNum

parseAndCalulateNewEpoch ::
  Word64 ->
  (Word64, Maybe UTCTime, Maybe UTCTime) ->
  (Maybe Rational, Maybe Rational, Word64) ->
  Epoch
parseAndCalulateNewEpoch epochNum (blkCount, minBlockTime, maxBlockTime) (sumTxOut, sumTxFee, txCount) =
  case (minBlockTime, maxBlockTime, sumTxOut, sumTxFee) of
    (Just start, Just end, Just outSum, Just fees) ->
      Epoch
        (fromIntegral $ numerator outSum)
        (DbLovelace . fromIntegral $ numerator fees)
        txCount
        blkCount
        epochNum
        start
        end
    (Just start, Just end, Nothing, Nothing) ->
      Epoch 0 (DbLovelace 0) txCount blkCount epochNum start end
    _otherwise ->
      emptyEpoch epochNum

convertBlk :: Word64 -> (Word64, Maybe UTCTime, Maybe UTCTime) -> Epoch
convertBlk epochNum (blkCount, b, c) =
  case (b, c) of
    (Just start, Just end) -> Epoch 0 (DbLovelace 0) 0 blkCount epochNum start end
    _otherwise -> emptyEpoch epochNum

-- We only return this when something has screwed up.
emptyEpoch :: Word64 -> Epoch
emptyEpoch epochNum =
  Epoch
    { epochOutSum = 0
    , epochFees = DbLovelace 0
    , epochTxCount = 0
    , epochBlkCount = 0
    , epochNo = epochNum
    , epochStartTime = defaultUTCTime
    , epochEndTime = defaultUTCTime
    }

queryCurrentEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryCurrentEpochNo = do
  res <- select $ do
    blk <- from $ table @Block
    pure $ max_ (blk ^. BlockEpochNo)
  pure $ join (unValue =<< listToMaybe res)

queryNormalEpochRewardCount ::
  MonadIO m =>
  Word64 ->
  ReaderT SqlBackend m Word64
queryNormalEpochRewardCount epochNum = do
  res <- select $ do
    rwd <- from $ table @Reward
    where_ (rwd ^. RewardSpendableEpoch ==. val epochNum)
    where_ (rwd ^. RewardType `in_` valList [RwdMember, RwdLeader])
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryGenesis :: MonadIO m => ReaderT SqlBackend m (Either LookupFail BlockId)
queryGenesis = do
  res <- select $ do
    blk <- from (table @Block)
    where_ (isNothing (blk ^. BlockPreviousId))
    pure $ blk ^. BlockId
  case res of
    [blk] -> pure $ Right (unValue blk)
    _ -> pure $ Left DBMultipleGenesis

-- | Get the latest block.
queryLatestBlock :: MonadIO m => ReaderT SqlBackend m (Maybe Block)
queryLatestBlock = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure blk
  pure $ fmap entityVal (listToMaybe res)

queryLatestPoints :: MonadIO m => ReaderT SqlBackend m [(Maybe Word64, ByteString)]
queryLatestPoints = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 5
    pure (blk ^. BlockSlotNo, blk ^. BlockHash)
  pure $ fmap unValue2 res

queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestEpochNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    orderBy [desc (blk ^. BlockEpochNo)]
    limit 1
    pure (blk ^. BlockEpochNo)
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => ReaderT SqlBackend m (Maybe BlockId)
queryLatestBlockId = do
  res <- select $ do
    blk <- from $ table @Block
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

-- | Get the latest slot number
queryLatestSlotNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestSlotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure $ blk ^. BlockSlotNo
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

{-# INLINEABLE queryMeta #-}

-- | Get the network metadata.
queryMeta :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Meta)
queryMeta = do
  res <- select . from $ table @Meta
  pure $ case res of
    [] -> Left DbMetaEmpty
    [m] -> Right $ entityVal m
    _ -> Left DbMetaMultipleRows

queryScript :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe ScriptId)
queryScript hsh = do
  xs <- select $ do
    script <- from $ table @Script
    where_ (script ^. ScriptHash ==. val hsh)
    pure (script ^. ScriptId)
  pure $ unValue <$> listToMaybe xs

queryDatum :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe DatumId)
queryDatum hsh = do
  xs <- select $ do
    datum <- from $ table @Datum
    where_ (datum ^. DatumHash ==. val hsh)
    pure (datum ^. DatumId)
  pure $ unValue <$> listToMaybe xs

queryRedeemerData :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe RedeemerDataId)
queryRedeemerData hsh = do
  xs <- select $ do
    rdmrDt <- from $ table @RedeemerData
    where_ (rdmrDt ^. RedeemerDataHash ==. val hsh)
    pure (rdmrDt ^. RedeemerDataId)
  pure $ unValue <$> listToMaybe xs

querySlotHash :: MonadIO m => SlotNo -> ReaderT SqlBackend m [(SlotNo, ByteString)]
querySlotHash slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo ==. just (val $ unSlotNo slotNo))
    pure (blk ^. BlockHash)
  pure $ (\vh -> (slotNo, unValue vh)) <$> res

queryMultiAssetId :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe MultiAssetId)
queryMultiAssetId policy assetName = do
  res <- select $ do
    ma <- from $ table @MultiAsset
    where_ (ma ^. MultiAssetPolicy ==. val policy &&. ma ^. MultiAssetName ==. val assetName)
    pure (ma ^. MultiAssetId)
  pure $ unValue <$> listToMaybe res

queryCountSlotNosGreaterThan :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryCountSlotNosGreaterThan slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo >. just (val slotNo))
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Like 'queryCountSlotNosGreaterThan', but returns all slots in the same order.
queryCountSlotNo :: MonadIO m => ReaderT SqlBackend m Word64
queryCountSlotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxCount = do
  res <- select $ do
    _ <- from $ table @Tx
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- -- | Get the 'TxId' associated with the given hash.
queryTxId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail TxId)
queryTxId hash = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxHash ==. val hash)
    pure (tx ^. TxId)
  pure $ maybeToEither (DbLookupTxHash hash) unValue (listToMaybe res)

queryEpochStakeCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryEpochStakeCount epoch = do
  res <- select $ do
    epochStake <- from $ table @EpochStake
    where_ (epochStake ^. EpochStakeEpochNo ==. val epoch)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryMinRefId ::
  forall m field record.
  (MonadIO m, PersistEntity record, PersistField field) =>
  EntityField record field ->
  field ->
  ReaderT SqlBackend m (Maybe (Key record))
queryMinRefId txIdField txId = do
  res <- select $ do
    rec <- from $ table @record
    where_ (rec ^. txIdField >=. val txId)
    orderBy [asc (rec ^. persistIdField)]
    limit 1
    pure $ rec ^. persistIdField
  pure $ unValue <$> listToMaybe res

queryMinRefIdNullable ::
  forall m field record.
  (MonadIO m, PersistEntity record, PersistField field) =>
  EntityField record (Maybe field) ->
  field ->
  ReaderT SqlBackend m (Maybe (Key record))
queryMinRefIdNullable txIdField txId = do
  res <- select $ do
    rec <- from $ table @record
    where_ (isJust (rec ^. txIdField))
    where_ (rec ^. txIdField >=. just (val txId))
    orderBy [asc (rec ^. persistIdField)]
    limit 1
    pure $ rec ^. persistIdField
  pure $ unValue <$> listToMaybe res

queryMaxRefId ::
  forall m field record.
  (MonadIO m, PersistEntity record, PersistField field) =>
  EntityField record field ->
  field ->
  Bool ->
  ReaderT SqlBackend m (Maybe (Key record))
queryMaxRefId txIdField txId eq = do
  res <- select $ do
    rec <- from $ table @record
    if eq
      then where_ (rec ^. txIdField <=. val txId)
      else where_ (rec ^. txIdField <. val txId)
    orderBy [desc (rec ^. persistIdField)]
    limit 1
    pure $ rec ^. persistIdField
  pure $ unValue <$> listToMaybe res

existsPoolHashId :: MonadIO m => PoolHashId -> ReaderT SqlBackend m Bool
existsPoolHashId phid = do
  res <- select $ do
    poolHash <- from $ table @PoolHash
    where_ (poolHash ^. PoolHashId ==. val phid)
    limit 1
    pure (poolHash ^. PoolHashId)
  pure $ not (null res)

-- db-sync
existsPoolMetadataRefId :: MonadIO m => PoolMetadataRefId -> ReaderT SqlBackend m Bool
existsPoolMetadataRefId pmrid = do
  res <- select $ do
    pmr <- from $ table @PoolMetadataRef
    where_ (pmr ^. PoolMetadataRefId ==. val pmrid)
    limit 1
    pure (pmr ^. PoolMetadataRefId)
  pure $ not (null res)

existsVotingAnchorId :: MonadIO m => VotingAnchorId -> ReaderT SqlBackend m Bool
existsVotingAnchorId vaId = do
  res <- select $ do
    votingAnchor <- from $ table @VotingAnchor
    where_ (votingAnchor ^. VotingAnchorId ==. val vaId)
    limit 1
    pure (votingAnchor ^. VotingAnchorId)
  pure $ not (null res)

queryAdaPotsId :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe (Entity AdaPots))
queryAdaPotsId blkId = do
  res <- select $ do
    adaPots <- from $ table @AdaPots
    where_ (adaPots ^. AdaPotsBlockId ==. val blkId)
    pure adaPots
  pure $ listToMaybe res

-- | Get the current block height.
queryBlockHeight :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryBlockHeight = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockBlockNo)
    orderBy [desc (blk ^. BlockBlockNo)]
    limit 1
    pure (blk ^. BlockBlockNo)
  pure $ unValue =<< listToMaybe res

queryAllExtraMigrations :: MonadIO m => ReaderT SqlBackend m [ExtraMigration]
queryAllExtraMigrations = do
  res <- select $ do
    ems <- from $ table @ExtraMigrations
    pure (ems ^. ExtraMigrationsToken)
  pure $ read . unpack . unValue <$> res

queryMinMaxEpochStake :: MonadIO m => ReaderT SqlBackend m (Maybe Word64, Maybe Word64)
queryMinMaxEpochStake = do
  maxEpoch <- select $ do
    es <- from $ table @EpochStake
    orderBy [desc (es ^. EpochStakeId)]
    limit 1
    pure (es ^. EpochStakeEpochNo)
  minEpoch <- select $ do
    es <- from $ table @EpochStake
    orderBy [asc (es ^. EpochStakeId)]
    limit 1
    pure (es ^. EpochStakeEpochNo)
  pure (unValue <$> listToMaybe minEpoch, unValue <$> listToMaybe maxEpoch)

queryGovActionProposalId :: MonadIO m => TxId -> Word64 -> ReaderT SqlBackend m (Either LookupFail GovActionProposalId)
queryGovActionProposalId txId index = do
  res <- select $ do
    ga <- from $ table @GovActionProposal
    where_ (ga ^. GovActionProposalTxId ==. val txId)
    where_ (ga ^. GovActionProposalIndex ==. val index)
    pure ga
  pure $ maybeToEither (DbLookupGovActionPair txId index) entityKey (listToMaybe res)

queryDrepHashAlwaysAbstain :: MonadIO m => ReaderT SqlBackend m (Maybe DrepHashId)
queryDrepHashAlwaysAbstain = do
  res <- select $ do
    dh <- from $ table @DrepHash
    where_ (isNothing (dh ^. DrepHashRaw))
    where_ (dh ^. DrepHashView ==. val hardcodedAlwaysAbstain)
    pure $ dh ^. DrepHashId
  pure $ unValue <$> listToMaybe res

queryDrepHashAlwaysNoConfidence :: MonadIO m => ReaderT SqlBackend m (Maybe DrepHashId)
queryDrepHashAlwaysNoConfidence = do
  res <- select $ do
    dh <- from $ table @DrepHash
    where_ (isNothing (dh ^. DrepHashRaw))
    where_ (dh ^. DrepHashView ==. val hardcodedAlwaysNoConfidence)
    pure $ dh ^. DrepHashId
  pure $ unValue <$> listToMaybe res

queryCommitteeHash :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe CommitteeHashId)
queryCommitteeHash hash = do
  res <- select $ do
    ch <- from $ table @CommitteeHash
    where_ (ch ^. CommitteeHashRaw ==. val hash)
    pure $ ch ^. CommitteeHashId
  pure $ unValue <$> listToMaybe res

queryProposalConstitution :: MonadIO m => Maybe GovActionProposalId -> ReaderT SqlBackend m [ConstitutionId]
queryProposalConstitution mgapId = do
  res <- select $ do
    c <- from $ table @Constitution
    where_ (bl c)
    pure $ c ^. ConstitutionId
  pure $ unValue <$> res
  where
    bl c = case mgapId of
      Nothing -> isNothing (c ^. ConstitutionGovActionProposalId)
      Just vl -> c ^. ConstitutionGovActionProposalId ==. val (Just vl)

queryProposalCommittee :: MonadIO m => Maybe GovActionProposalId -> ReaderT SqlBackend m [CommitteeId]
queryProposalCommittee mgapId = do
  res <- select $ do
    c <- from $ table @Committee
    where_ (bl c)
    pure $ c ^. CommitteeId
  pure $ unValue <$> res
  where
    bl c = case mgapId of
      Nothing -> isNothing (c ^. CommitteeGovActionProposalId)
      Just vl -> c ^. CommitteeGovActionProposalId ==. val (Just vl)

queryPoolHashId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe PoolHashId)
queryPoolHashId hash = do
  res <- select $ do
    phash <- from $ table @PoolHash
    where_ (phash ^. PoolHashHashRaw ==. val hash)
    pure (phash ^. PoolHashId)
  pure $ unValue <$> listToMaybe res

queryStakeAddress ::
  MonadIO m =>
  ByteString ->
  (ByteString -> Text) ->
  ReaderT SqlBackend m (Either LookupFail StakeAddressId)
queryStakeAddress addr toText = do
  res <- select $ do
    saddr <- from $ table @StakeAddress
    where_ (saddr ^. StakeAddressHashRaw ==. val addr)
    pure (saddr ^. StakeAddressId)
  pure $ maybeToEither (DbLookupMessage $ "StakeAddress " <> toText addr) unValue (listToMaybe res)

queryStakeRefPtr :: MonadIO m => Ptr -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeRefPtr (Ptr (SlotNo slot) (TxIx txIx) (CertIx certIx)) = do
  res <- select $ do
    (blk :& tx :& sr) <-
      from
        $ table @Block
          `innerJoin` table @Tx
        `on` (\(blk :& tx) -> blk ^. BlockId ==. tx ^. TxBlockId)
          `innerJoin` table @StakeRegistration
        `on` (\(_blk :& tx :& sr) -> sr ^. StakeRegistrationTxId ==. tx ^. TxId)

    where_ (blk ^. BlockSlotNo ==. just (val slot))
    where_ (tx ^. TxBlockIndex ==. val (fromIntegral txIx))
    where_ (sr ^. StakeRegistrationCertIndex ==. val (fromIntegral certIx))
    -- Need to order by DelegationSlotNo descending for correct behavior when there are two
    -- or more delegation certificates in a single epoch.
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (sr ^. StakeRegistrationAddrId)
  pure $ unValue <$> listToMaybe res

-- Check if there are other PoolUpdates in the same blocks for the same pool
queryPoolUpdateByBlock :: MonadIO m => BlockId -> PoolHashId -> ReaderT SqlBackend m Bool
queryPoolUpdateByBlock blkId poolHashId = do
  res <- select $ do
    (blk :& _tx :& poolUpdate) <-
      from
        $ table @Block
          `innerJoin` table @Tx
        `on` (\(blk :& tx) -> blk ^. BlockId ==. tx ^. TxBlockId)
          `innerJoin` table @PoolUpdate
        `on` (\(_blk :& tx :& poolUpdate) -> tx ^. TxId ==. poolUpdate ^. PoolUpdateRegisteredTxId)
    where_ (poolUpdate ^. PoolUpdateHashId ==. val poolHashId)
    where_ (blk ^. BlockId ==. val blkId)
    limit 1
    pure (blk ^. BlockEpochNo)
  pure $ not (null res)

{--------------------------------------------
  Queries use in SMASH
----------------------------------------------}

queryOffChainPoolData :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe (Text, ByteString))
queryOffChainPoolData poolHash poolMetadataHash = do
  res <- select $ do
    (pod :& ph) <-
      from
        $ table @OffChainPoolData
          `innerJoin` table @PoolHash
        `on` (\(pod :& ph) -> pod ^. OffChainPoolDataPoolId ==. ph ^. PoolHashId)
    where_ (ph ^. PoolHashHashRaw ==. val poolHash)
    where_ (pod ^. OffChainPoolDataHash ==. val poolMetadataHash)
    limit 1
    pure (pod ^. OffChainPoolDataTickerName, pod ^. OffChainPoolDataBytes)
  pure $ unValue2 <$> listToMaybe res

queryPoolRegister :: MonadIO m => Maybe ByteString -> ReaderT SqlBackend m [PoolCert]
queryPoolRegister mPoolHash = do
  res <- select $ do
    (poolUpdate :& poolHash :& poolMeta :& tx :& blk) <-
      from
        $ table @PoolUpdate
          `innerJoin` table @PoolHash
        `on` (\(poolUpdate :& poolHash) -> poolUpdate ^. PoolUpdateHashId ==. poolHash ^. PoolHashId)
          `innerJoin` table @PoolMetadataRef
        `on` (\(poolUpdate :& _poolHash :& poolMeta) -> poolUpdate ^. PoolUpdateMetaId ==. just (poolMeta ^. PoolMetadataRefId))
          `innerJoin` table @Tx
        `on` (\(poolUpdate :& _poolHash :& _poolMeta :& tx) -> poolUpdate ^. PoolUpdateRegisteredTxId ==. tx ^. TxId)
          `innerJoin` table @Block
        `on` (\(_poolUpdate :& _poolHash :& _poolMeta :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)

    whenJust mPoolHash $ \ph ->
      where_ (poolHash ^. PoolHashHashRaw ==. val ph)
    pure
      ( poolHash ^. PoolHashHashRaw
      , poolMeta ^. PoolMetadataRefHash
      , blk ^. BlockBlockNo
      , tx ^. TxBlockIndex
      , poolUpdate ^. PoolUpdateCertIndex
      )
  pure $ toUpdateInfo . unValue5 <$> res
  where
    toUpdateInfo (poolHash, metaHash, blkNo, txIndex, retIndex) =
      PoolCert
        { pcHash = poolHash
        , pcCertAction = Register metaHash
        , pcCertNo = CertNo blkNo txIndex retIndex
        }

queryRetiredPools :: MonadIO m => Maybe ByteString -> ReaderT SqlBackend m [PoolCert]
queryRetiredPools mPoolHash = do
  res <- select $ do
    (retired :& poolHash :& tx :& blk) <-
      from
        $ table @PoolRetire
          `innerJoin` table @PoolHash
        `on` (\(retired :& poolHash) -> retired ^. PoolRetireHashId ==. poolHash ^. PoolHashId)
          `innerJoin` table @Tx
        `on` (\(retired :& _poolHash :& tx) -> retired ^. PoolRetireAnnouncedTxId ==. tx ^. TxId)
          `innerJoin` table @Block
        `on` (\(_retired :& _poolHash :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    whenJust mPoolHash $ \ph ->
      where_ (poolHash ^. PoolHashHashRaw ==. val ph)
    pure
      ( poolHash ^. PoolHashHashRaw
      , retired ^. PoolRetireRetiringEpoch
      , blk ^. BlockBlockNo
      , tx ^. TxBlockIndex
      , retired ^. PoolRetireCertIndex
      )
  pure $ toRetirementInfo . unValue5 <$> res
  where
    toRetirementInfo (hsh, retEpoch, blkNo, txIndex, retIndex) =
      PoolCert
        { pcHash = hsh
        , pcCertAction = Retirement retEpoch
        , pcCertNo = CertNo blkNo txIndex retIndex
        }

queryUsedTicker :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe Text)
queryUsedTicker poolHash metaHash = do
  res <- select $ do
    (pod :& ph) <-
      from
        $ table @OffChainPoolData
          `innerJoin` table @PoolHash
        `on` (\(pod :& ph) -> ph ^. PoolHashId ==. pod ^. OffChainPoolDataPoolId)
    where_ (ph ^. PoolHashHashRaw ==. val poolHash)
    where_ (pod ^. OffChainPoolDataHash ==. val metaHash)
    pure $ pod ^. OffChainPoolDataTickerName
  pure $ unValue <$> listToMaybe res

queryReservedTicker :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe ByteString)
queryReservedTicker tickerName = do
  res <- select $ do
    ticker <- from $ table @ReservedPoolTicker
    where_ (ticker ^. ReservedPoolTickerName ==. val tickerName)
    pure $ ticker ^. ReservedPoolTickerPoolHash
  pure $ unValue <$> listToMaybe res

queryReservedTickers :: MonadIO m => ReaderT SqlBackend m [ReservedPoolTicker]
queryReservedTickers =
  fmap entityVal <$> selectList [] []

-- Return delisted Pool hashes.
queryDelistedPools :: MonadIO m => ReaderT SqlBackend m [ByteString]
queryDelistedPools = do
  res <- select $ do
    delistedPool <- from $ table @DelistedPool
    pure $ delistedPool ^. DelistedPoolHashRaw
  pure $ unValue <$> res

-- Returns also the metadata hash
queryOffChainPoolFetchError :: MonadIO m => ByteString -> Maybe UTCTime -> ReaderT SqlBackend m [(OffChainPoolFetchError, ByteString)]
queryOffChainPoolFetchError hash Nothing = do
  res <- select $ do
    (offChainPoolFetchError :& poolHash :& poolMetadataRef) <-
      from
        $ table @OffChainPoolFetchError
          `innerJoin` table @PoolHash
        `on` (\(offChainPoolFetchError :& poolHash) -> offChainPoolFetchError ^. OffChainPoolFetchErrorPoolId ==. poolHash ^. PoolHashId)
          `innerJoin` table @PoolMetadataRef
        `on` (\(offChainPoolFetchError :& _ :& poolMetadataRef) -> offChainPoolFetchError ^. OffChainPoolFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)

    where_ (poolHash ^. PoolHashHashRaw ==. val hash)
    orderBy [desc (offChainPoolFetchError ^. OffChainPoolFetchErrorFetchTime)]
    limit 10
    pure (offChainPoolFetchError, poolMetadataRef ^. PoolMetadataRefHash)
  pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)
queryOffChainPoolFetchError hash (Just fromTime) = do
  res <- select $ do
    (offChainPoolFetchError :& poolHash :& poolMetadataRef) <-
      from
        $ table @OffChainPoolFetchError
          `innerJoin` table @PoolHash
        `on` (\(offChainPoolFetchError :& poolHash) -> offChainPoolFetchError ^. OffChainPoolFetchErrorPoolId ==. poolHash ^. PoolHashId)
          `innerJoin` table @PoolMetadataRef
        `on` (\(offChainPoolFetchError :& _poolHash :& poolMetadataRef) -> offChainPoolFetchError ^. OffChainPoolFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)
    where_
      ( poolHash
          ^. PoolHashHashRaw
          ==. val hash
          &&. offChainPoolFetchError
            ^. OffChainPoolFetchErrorFetchTime
            >=. val fromTime
      )
    orderBy [desc (offChainPoolFetchError ^. OffChainPoolFetchErrorFetchTime)]
    limit 10
    pure (offChainPoolFetchError, poolMetadataRef ^. PoolMetadataRefHash)
  pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)

existsDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
existsDelistedPool ph = do
  res <- select $ do
    delistedPool <- from $ table @DelistedPool
    where_ (delistedPool ^. DelistedPoolHashRaw ==. val ph)
    limit 1
    pure (delistedPool ^. DelistedPoolId)
  pure $ not (null res)

{---------------------------------------------------------
  Queries use in Tools (valiadtion and snapshot creation)
----------------------------------------------------------}

queryDepositUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryDepositUpToBlockNo blkNo = do
  res <- select $ do
    (tx :& blk) <-
      from
        $ table @Tx
          `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (blk ^. BlockBlockNo <=. just (val blkNo))
    pure $ sum_ (tx ^. TxDeposit)
  pure $ unValueSumAda (listToMaybe res)

queryEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail Epoch)
queryEpochEntry epochNum = do
  res <- select $ do
    epoch <- from $ table @Epoch
    where_ (epoch ^. EpochNo ==. val epochNum)
    pure epoch
  pure $ maybeToEither (DbLookupEpochNo epochNum) entityVal (listToMaybe res)

-- | Get the fees paid in all block from genesis up to and including the specified block.
queryFeesUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToBlockNo blkNo = do
  res <- select $ do
    (tx :& blk) <-
      from
        $ table @Tx
          `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (blk ^. BlockBlockNo <=. just (val blkNo))
    pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

queryFeesUpToSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToSlotNo slotNo = do
  res <- select $ do
    (tx :& blk) <-
      from
        $ table @Tx
          `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (isJust $ blk ^. BlockSlotNo)
    where_ (blk ^. BlockSlotNo <=. just (val slotNo))
    pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

queryLatestCachedEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestCachedEpochNo = do
  res <- select $ do
    epoch <- from $ table @Epoch
    orderBy [desc (epoch ^. EpochNo)]
    limit 1
    pure (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res

-- | Get the 'BlockNo' of the latest block.
queryLatestBlockNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestBlockNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockBlockNo)
    orderBy [desc (blk ^. BlockBlockNo)]
    limit 1
    pure $ blk ^. BlockBlockNo
  pure $ listToMaybe (mapMaybe unValue res)

querySlotNosGreaterThan :: MonadIO m => Word64 -> ReaderT SqlBackend m [SlotNo]
querySlotNosGreaterThan slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    -- Want all BlockNos where the block satisfies this predicate.
    where_ (blk ^. BlockSlotNo >. just (val slotNo))
    -- Return them in descending order so we can delete the highest numbered
    -- ones first.
    orderBy [desc (blk ^. BlockSlotNo)]
    pure (blk ^. BlockSlotNo)
  pure $ mapMaybe (fmap SlotNo . unValue) res

-- | Like 'querySlotNosGreaterThan', but returns all slots in the same order.
querySlotNos :: MonadIO m => ReaderT SqlBackend m [SlotNo]
querySlotNos = do
  res <- select $ do
    blk <- from $ table @Block
    -- Return them in descending order so we can delete the highest numbered
    -- ones first.
    orderBy [desc (blk ^. BlockSlotNo)]
    pure (blk ^. BlockSlotNo)
  pure $ mapMaybe (fmap SlotNo . unValue) res

-- | Calculate the slot time (as UTCTime) for a given slot number.
-- This will fail if the slot is empty.
querySlotUtcTime :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail UTCTime)
querySlotUtcTime slotNo = do
  le <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo ==. just (val slotNo))
    pure (blk ^. BlockTime)
  pure $ maybe (Left $ DbLookupSlotNo slotNo) (Right . unValue) (listToMaybe le)

queryWithdrawalsUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryWithdrawalsUpToBlockNo blkNo = do
  res <- select $ do
    (_tx :& wdrl :& blk) <-
      from
        $ table @Tx
          `innerJoin` table @Withdrawal
        `on` (\(tx :& wdrl) -> tx ^. TxId ==. wdrl ^. WithdrawalTxId)
          `innerJoin` table @Block
        `on` (\(tx :& _wdrl :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (blk ^. BlockBlockNo <=. val (Just $ fromIntegral blkNo))
    pure $ sum_ (wdrl ^. WithdrawalAmount)
  pure $ unValueSumAda (listToMaybe res)

queryAdaPots :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe AdaPots)
queryAdaPots blkId = do
  res <- select $ do
    adaPots <- from $ table @AdaPots
    where_ (adaPots ^. AdaPotsBlockId ==. val blkId)
    pure adaPots
  pure $ fmap entityVal (listToMaybe res)

{-----------------------
  Queries use in tests
------------------------}

queryRewardCount :: MonadIO m => ReaderT SqlBackend m Word64
queryRewardCount = do
  res <- select $ do
    _ <- from $ table @Reward
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryRewardRestCount :: MonadIO m => ReaderT SqlBackend m Word64
queryRewardRestCount = do
  res <- select $ do
    _ <- from $ table @RewardRest
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxInCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxInCount = do
  res <- select $ from (table @TxIn) >> pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryCostModel :: MonadIO m => ReaderT SqlBackend m [CostModelId]
queryCostModel =
  fmap entityKey <$> selectList [] [Asc CostModelId]

queryTxInRedeemer :: MonadIO m => ReaderT SqlBackend m [TxIn]
queryTxInRedeemer = do
  res <- select $ do
    tx_in <- from $ table @TxIn
    where_ (isJust $ tx_in ^. TxInRedeemerId)
    pure tx_in
  pure $ entityVal <$> res

-- | Gets all the 'TxIn' of invalid txs
queryTxInFailedTx :: MonadIO m => ReaderT SqlBackend m [TxIn]
queryTxInFailedTx = do
  res <- select $ do
    (tx_in :& tx) <-
      from
        $ table @TxIn
          `innerJoin` table @Tx
        `on` (\(tx_in :& tx) -> tx_in ^. TxInTxInId ==. tx ^. TxId)
    where_ (tx ^. TxValidContract ==. val False)
    pure tx_in
  pure $ entityVal <$> res

queryInvalidTx :: MonadIO m => ReaderT SqlBackend m [Tx]
queryInvalidTx = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxValidContract ==. val False)
    pure tx
  pure $ entityVal <$> res

queryDeregistrationScript :: MonadIO m => ReaderT SqlBackend m [StakeDeregistration]
queryDeregistrationScript = do
  res <- select $ do
    dereg <- from $ table @StakeDeregistration
    where_ (isJust $ dereg ^. StakeDeregistrationRedeemerId)
    pure dereg
  pure $ entityVal <$> res

queryDelegationScript :: MonadIO m => ReaderT SqlBackend m [Delegation]
queryDelegationScript = do
  res <- select $ do
    deleg <- from $ table @Delegation
    where_ (isJust $ deleg ^. DelegationRedeemerId)
    pure deleg
  pure $ entityVal <$> res

queryWithdrawalScript :: MonadIO m => ReaderT SqlBackend m [Withdrawal]
queryWithdrawalScript = do
  res <- select $ do
    wtdr <- from $ table @Withdrawal
    where_ (isJust $ wtdr ^. WithdrawalRedeemerId)
    pure wtdr
  pure $ entityVal <$> res

queryStakeAddressScript :: MonadIO m => ReaderT SqlBackend m [StakeAddress]
queryStakeAddressScript = do
  res <- select $ do
    st_addr <- from $ table @StakeAddress
    where_ (isJust $ st_addr ^. StakeAddressScriptHash)
    pure st_addr
  pure $ entityVal <$> res

querySchemaVersion :: MonadIO m => ReaderT SqlBackend m (Maybe SchemaVersion)
querySchemaVersion = do
  res <- select $ do
    sch <- from $ table @SchemaVersion
    orderBy [desc (sch ^. SchemaVersionStageOne)]
    limit 1
    pure (sch ^. SchemaVersionStageOne, sch ^. SchemaVersionStageTwo, sch ^. SchemaVersionStageThree)
  pure $ uncurry3 SchemaVersion . unValue3 <$> listToMaybe res

-- | Given a 'SlotNo' return the 'SlotNo' of the previous block.
queryPreviousSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Word64)
queryPreviousSlotNo slotNo = do
  res <- select $ do
    (blk :& pblk) <-
      from
        $ table @Block
          `innerJoin` table @Block
        `on` (\(blk :& pblk) -> blk ^. BlockPreviousId ==. just (pblk ^. BlockId))
    where_ (blk ^. BlockSlotNo ==. just (val slotNo))
    pure $ pblk ^. BlockSlotNo
  pure $ unValue =<< listToMaybe res
