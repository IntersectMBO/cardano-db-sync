{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Query
  ( LookupFail (..)
  , queryAddressBalanceAtSlot
  , queryAddressOutputs
  , queryGenesis
  , queryBlock
  , queryBlockCount
  , queryBlockCountAfterBlockNo
  , queryBlockHeight
  , queryBlockId
  , queryBlockNoId
  , queryBlockSlotNo
  , queryBlockNo
  , queryMainBlock
  , queryBlockTxCount
  , queryBlocksAfterSlot
  , queryCalcEpochEntry
  , queryCheckPoints
  , queryCurrentEpochNo
  , queryDepositUpToBlockNo
  , queryEpochEntry
  , queryEpochNo
  , queryRewardCount
  , queryRewards
  , queryNormalEpochRewardCount
  , queryNullPoolRewardExists
  , queryEpochRewardCount
  , queryRewardsSpend
  , queryFeesUpToBlockNo
  , queryFeesUpToSlotNo
  , queryGenesisSupply
  , queryShelleyGenesisSupply
  , queryLatestBlock
  , queryLatestPoints
  , queryLatestCachedEpochNo
  , queryLatestEpochNo
  , queryLatestBlockId
  , queryLatestBlockNo
  , queryLatestSlotNo
  , queryPreviousSlotNo
  , queryMeta
  , queryMultiAssetId
  , queryNetworkName
  , querySlotNosGreaterThan
  , queryLastSlotNoGreaterThan
  , queryCountSlotNosGreaterThan
  , queryLastSlotNo
  , queryCountSlotNo
  , querySchemaVersion
  , queryScript
  , queryDatum
  , queryDatumPage
  , queryDatumCount
  , querydatumInfo
  , queryRedeemerData
  , queryRedeemerDataPage
  , queryRedeemerDataCount
  , queryRedeemerDataInfo
  , querySelectCount
  , querySlotHash
  , querySlotUtcTime
  , queryTotalSupply
  , queryTxCount
  , queryTxId
  , queryTxInCount
  , queryTxOutCount
  , queryTxOutValue
  , queryTxOutCredentials
  , queryEpochStakeCount
  , queryUtxoAtBlockNo
  , queryUtxoAtSlotNo
  , queryWithdrawalsUpToBlockNo
  , queryCostModel
  , queryAdaPots
  , queryPoolOfflineData
  , queryPoolRegister
  , queryRetiredPools
  , queryUsedTicker
  , queryReservedTicker
  , queryReservedTickers
  , queryDelistedPools
  , queryPoolOfflineFetchError
  , queryScriptOutputs
  , queryTxInRedeemer
  , queryTxInFailedTx
  , queryInvalidTx
  , queryDeregistrationScript
  , queryDelegationScript
  , queryWithdrawalScript
  , queryStakeAddressScript
  , queryStakeAddressIdsAfter
  , existsDelistedPool
  , existsPoolHash
  , existsPoolHashId
  , existsPoolMetadataRefId

  , entityPair
  , isJust
  , listToMaybe
  , maybeToEither
  , renderLookupFail
  , txOutSpentB
  , txOutSpentP
  , txOutUnspentP
  , unBlockId
  , unTxId
  , unTxInId
  , unTxOutId
  , unValue2
  , unValue3
  , unValue4
  , unValue5
  , unValueSumAda
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.Extra (join, mapMaybeM, whenJust)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Fixed (Micro)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ratio (numerator)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime (..))
import           Data.Tuple.Extra (uncurry3)
import           Data.Word (Word64)

import           Database.Esqueleto.Experimental (Entity, From, PersistEntity, PersistField,
                   SqlBackend, SqlExpr, SqlQuery, Value (Value, unValue), ValueList, asc, count,
                   countRows, desc, entityKey, entityVal, exists, from, in_, innerJoin, isNothing,
                   just, leftJoin, limit, max_, min_, notExists, not_, offset, on, orderBy, select,
                   subList_select, sum_, table, type (:&) ((:&)), unSqlBackendKey, val, valList,
                   where_, (&&.), (<=.), (==.), (>.), (>=.), (?.), (^.), (||.))
import           Database.Esqueleto.Experimental.From (ToFrom (..))
import           Database.Persist.Class.PersistQuery (selectList)

import           Cardano.Db.Error
import           Cardano.Db.Schema
import           Cardano.Db.Types
import           Database.Persist.Types (SelectOpt (Asc))

{- HLINT ignore "Redundant ^." -}
{- HLINT ignore "Fuse on/on" -}
{- HLINT ignore "Reduce duplication" -}

-- If you squint, these Esqueleto queries almost look like SQL queries.

queryAddressBalanceAtSlot :: MonadIO m => Text -> Word64 -> ReaderT SqlBackend m Ada
queryAddressBalanceAtSlot addr slotNo = do
    eblkId <- select $ do
      blk <- from (table @Block)
      where_ (blk ^. BlockSlotNo ==. just (val slotNo))
      pure (blk ^. BlockId)
    maybe (pure 0) (queryAddressBalanceAtBlockId . unValue) (listToMaybe eblkId)
  where
    queryAddressBalanceAtBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
    queryAddressBalanceAtBlockId blkid = do
        -- tx1 refers to the tx of the input spending this output (if it is ever spent)
        -- tx2 refers to the tx of the output
        res <- select $ do
          (txout :& _ :& _ :& blk :& _) <-
            from $ table @TxOut
            `leftJoin` table @TxIn
            `on` (\(txout :& txin) -> just (txout ^. TxOutTxId) ==. txin ?. TxInTxOutId)
            `leftJoin` table @Tx
            `on` (\(_ :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
            `leftJoin` table @Block
            `on` (\(_ :& _ :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
            `leftJoin` table @Tx
            `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. TxOutTxId) ==. tx2 ?. TxId )
          where_ $ (txout ^. TxOutTxId `in_` txLessEqual blkid)
            &&. (isNothing (blk ?. BlockBlockNo)||. (blk ?. BlockId >. just (val blkid)))
          where_ (txout ^. TxOutAddress ==. val addr)
          pure $ sum_ (txout ^. TxOutValue)
        pure $ unValueSumAda (listToMaybe res)

queryAddressOutputs :: MonadIO m => ByteString -> ReaderT SqlBackend m DbLovelace
queryAddressOutputs addr = do
    res <- select $ do
      txout <- from $ table @TxOut
      where_ (txout ^. TxOutAddressRaw ==. val addr)
      pure $ sum_ (txout ^. TxOutValue)
    pure $ convert (listToMaybe res)
  where
    convert v = case unValue <$> v of
      Just (Just x) -> x
      _ -> DbLovelace 0

queryGenesis :: MonadIO m => ReaderT SqlBackend m (Either LookupFail BlockId)
queryGenesis = do
  res <- select $ do
    blk <- from (table @Block)
    where_ (isNothing (blk ^. BlockPreviousId))
    pure $ blk ^. BlockId
  case res of
    [blk] -> pure $ Right (unValue blk)
    _ -> pure $ Left DBMultipleGenesis

-- | Get the 'Block' associated with the given hash.
queryBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail Block)
queryBlock hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val hash)
    pure blk
  pure $ maybeToEither (DbLookupBlockHash hash) entityVal (listToMaybe res)

-- | Count the number of blocks in the Block table.
queryBlockCount :: MonadIO m => ReaderT SqlBackend m Word
queryBlockCount = do
  res <- select $ do
    _blk <- from $ table @ Block
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

-- | Get the 'BlockId' associated with the given hash.
queryBlockNoId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (BlockId, Maybe Word64))
queryBlockNoId hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val hash)
    pure (blk ^. BlockId, blk ^. BlockBlockNo)
  pure $ maybeToEither (DbLookupBlockHash hash) unValue2 (listToMaybe res)

-- | Count the number of blocks in the Block table after a 'BlockNo'.
queryBlockCountAfterBlockNo :: MonadIO m => Word64 -> Bool -> ReaderT SqlBackend m Word
queryBlockCountAfterBlockNo blockNo queryEq = do
  res <- select $ do
    blk <- from $ table @ Block
    where_ (if queryEq then blk ^. BlockBlockNo >=. just (val (fromIntegral blockNo))
                       else blk ^. BlockBlockNo >.  just (val (fromIntegral blockNo)))
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the 'SlotNo' associated with the given hash.
queryBlockSlotNo :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (Maybe Word64))
queryBlockSlotNo hash = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockHash ==. val hash)
    pure $ blk ^. BlockSlotNo
  pure $ maybeToEither (DbLookupBlockHash hash) unValue (listToMaybe res)

queryBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockNo blkNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo ==. just (val blkNo))
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

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

-- | Get the latest 'Block' associated with the given hash, skipping any EBBs.
queryMainBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail Block)
queryMainBlock hash = do
    res <- select  $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockHash ==. val hash)
      pure $ blk ^. BlockId
    maybe (pure $ Left (DbLookupBlockHash hash)) queryMainBlockId (unValue <$> listToMaybe res)
  where
    queryMainBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m (Either LookupFail Block)
    queryMainBlockId blkid = do
      res <- select $ do
        blk <- from $ table @Block
        where_ (isJust (blk ^. BlockBlockNo) &&. blk ^. BlockId <=. val blkid)
        orderBy [desc (blk ^. BlockSlotNo)]
        limit 1
        pure blk
      pure $ maybeToEither (DbLookupBlockId $ unBlockId blkid) entityVal (listToMaybe res)

-- | Get the number of transactions in the specified block.
queryBlockTxCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word64
queryBlockTxCount blkId = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxBlockId ==. val blkId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryBlocksAfterSlot :: MonadIO m => Word64 -> ReaderT SqlBackend m Int
queryBlocksAfterSlot slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo >. just (val slotNo))
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epoch entry for the last epoch.
-- When following the chain, this is called for each new block of the current epoch.
queryCalcEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m Epoch
queryCalcEpochEntry epochNum = do
    blkRes <- select $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockEpochNo ==. just (val epochNum))
      pure (countRows, min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    txRes <- select $ do
      (tx :& blk) <-
        from $ table @Tx
        `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      where_ (blk ^. BlockEpochNo ==. just (val epochNum))
      pure (sum_ (tx ^. TxOutSum), sum_ (tx ^. TxFee), count (tx ^. TxOutSum))
    case (listToMaybe blkRes, listToMaybe txRes) of
      (Just blk, Just tx) -> pure $ convertAll (unValue3 blk) (unValue3 tx)
      (Just blk, Nothing) -> pure $ convertBlk (unValue3 blk)
      _otherwise -> pure emptyEpoch
  where
    convertAll
        :: (Word64, Maybe UTCTime, Maybe UTCTime) -> (Maybe Rational, Maybe Rational, Word64)
        -> Epoch
    convertAll (blkCount, b, c) (d, e, txCount) =
      case (b, c, d, e) of
        (Just start, Just end, Just outSum, Just fees) ->
            Epoch (fromIntegral $ numerator outSum) (DbLovelace . fromIntegral $ numerator fees)
                        txCount blkCount epochNum start end
        (Just start, Just end, Nothing, Nothing) ->
            Epoch 0 (DbLovelace 0) txCount blkCount epochNum start end
        _otherwise ->
            emptyEpoch

    convertBlk :: (Word64, Maybe UTCTime, Maybe UTCTime) -> Epoch
    convertBlk (blkCount, b, c) =
      case (b, c) of
        (Just start, Just end) -> Epoch 0 (DbLovelace 0) 0 blkCount epochNum start end
        _otherwise -> emptyEpoch

    -- We only return this when something has screwed up.
    emptyEpoch :: Epoch
    emptyEpoch =
      Epoch
        { epochOutSum = 0
        , epochFees = DbLovelace 0
        , epochTxCount = 0
        , epochBlkCount = 0
        , epochNo = epochNum
        , epochStartTime = defaultUTCTime
        , epochEndTime = defaultUTCTime
        }

    defaultUTCTime :: UTCTime
    defaultUTCTime = read "2000-01-01 00:00:00.000000 UTC"

queryCheckPoints :: MonadIO m => Word64 -> ReaderT SqlBackend m [(Word64, ByteString)]
queryCheckPoints limitCount = do
    latest <- select $ do
      blk <- from $ table @Block
      where_ (isJust $ blk ^. BlockSlotNo)
      orderBy [desc (blk ^. BlockSlotNo)]
      limit (fromIntegral limitCount)
      pure (blk ^. BlockSlotNo)
    case mapMaybe unValue latest of
      [] -> pure []
      xs -> mapMaybeM querySpacing xs
  where
    querySpacing :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe (Word64, ByteString))
    querySpacing blkNo = do
       rows <- select $ do
         blk <- from $ table @Block
         where_ (blk ^. BlockSlotNo ==. just (val blkNo))
         pure (blk ^. BlockSlotNo, blk ^. BlockHash)
       pure $ convert =<< listToMaybe rows

    convert :: (Value (Maybe Word64), Value ByteString) -> Maybe (Word64, ByteString)
    convert (va, vb) =
      case (unValue va, unValue vb) of
        (Nothing, _ ) -> Nothing
        (Just a, b) -> Just (a, b)

queryDepositUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryDepositUpToBlockNo blkNo = do
  res <- select $ do
    (tx :& blk) <-
      from $ table @Tx
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

-- | Get the Epoch number for a given block. Returns '0' for the genesis block
-- even though the DB entry for the genesis block is 'NULL'.
queryEpochNo :: MonadIO m => BlockId -> ReaderT SqlBackend m (Either LookupFail (Maybe Word64))
queryEpochNo blkId = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockId ==. val blkId)
    pure $ blk ^. BlockEpochNo
  pure $ maybeToEither (DbLookupBlockId $ unBlockId blkId) unValue (listToMaybe res)

queryCurrentEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryCurrentEpochNo = do
    res <- select $ do
      blk <- from $ table @Block
      pure $ max_ (blk ^. BlockEpochNo)
    pure $ join (unValue =<< listToMaybe res)

queryRewardCount :: MonadIO m => ReaderT SqlBackend m Word64
queryRewardCount = do
  res <- select $ do
    _ <- from $ table @Reward
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryEpochRewardCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryEpochRewardCount epochNum = do
  res <- select $ do
    rwds <- from $ table @Reward
    where_ (rwds ^. RewardSpendableEpoch ==. val epochNum)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryRewardsSpend :: MonadIO m => Word64 -> ReaderT SqlBackend m [Reward]
queryRewardsSpend epochNum = do
  res <- select $ do
    rwds <- from $ table @Reward
    where_ (rwds ^. RewardSpendableEpoch ==. val epochNum)
    pure rwds
  pure $ entityVal <$> res

queryRewards :: MonadIO m => Word64 -> ReaderT SqlBackend m [Reward]
queryRewards epochNum = do
  res <- select $ do
    rwds <- from $ table @Reward
    where_ (rwds ^. RewardEarnedEpoch ==. val epochNum)
    pure rwds
  pure $ entityVal <$> res

queryNormalEpochRewardCount
    :: MonadIO m
    => Word64 -> ReaderT SqlBackend m Word64
queryNormalEpochRewardCount epochNum = do
  res <- select $ do
    rwd <- from $ table @Reward
    where_ (rwd ^. RewardSpendableEpoch ==. val epochNum)
    where_ (rwd ^. RewardType `in_` valList [RwdMember, RwdLeader])
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryNullPoolRewardExists :: MonadIO m => Reward -> ReaderT SqlBackend m Bool
queryNullPoolRewardExists newRwd = do
  res <- select $ do
    rwd <- from $ table @Reward
    where_ (rwd ^. RewardAddrId ==. val (rewardAddrId newRwd))
    where_ (rwd ^. RewardType ==. val (rewardType newRwd))
    where_ (rwd ^. RewardEarnedEpoch ==. val (rewardEarnedEpoch newRwd))
    limit 1
    pure (rwd ^. RewardId)
  pure $ not (null res)

-- | Get the fees paid in all block from genesis up to and including the specified block.
queryFeesUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToBlockNo blkNo = do
  res <- select $ do
    (tx :& blk) <-
      from $ table @Tx
      `innerJoin` table @Block
      `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (blk ^. BlockBlockNo <=. just (val blkNo))
    pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

queryFeesUpToSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToSlotNo slotNo = do
  res <- select $ do
    (tx :& blk) <-
      from $ table @Tx
      `innerJoin` table @Block
      `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
    where_ (isJust $ blk ^. BlockSlotNo)
    where_ (blk ^. BlockSlotNo <=. just (val slotNo))
    pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

-- | Return the total Genesis coin supply.
queryGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryGenesisSupply = do
    res <- select $ do
      (_tx :& txOut :& blk) <-
        from $ table @Tx
        `innerJoin` table @TxOut
        `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. TxOutTxId)
        `innerJoin` table @Block
        `on` (\(tx :& _txOut :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      where_ (isNothing $ blk ^. BlockPreviousId)
      pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Return the total Shelley Genesis coin supply. The Shelley Genesis Block
-- is the unique which has a non-null PreviousId, but has null Epoch.
queryShelleyGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryShelleyGenesisSupply = do
    res <- select $ do
      (txOut :& _tx :& blk) <-
        from $ table @TxOut
        `innerJoin` table @Tx
        `on` (\(txOut :& tx) -> tx ^. TxId ==. txOut ^. TxOutTxId)
        `innerJoin` table @Block
        `on` (\(_txOut :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      where_ (isJust $ blk ^. BlockPreviousId)
      where_ (isNothing $ blk ^. BlockEpochNo)
      pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => ReaderT SqlBackend m (Maybe BlockId)
queryLatestBlockId = do
  res <- select $ do
    blk <- from $ table @Block
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

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

queryLatestCachedEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestCachedEpochNo = do
  res <- select $ do
    epoch <- from $ table @Epoch
    orderBy [desc (epoch ^. EpochNo)]
    limit 1
    pure (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res

queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestEpochNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (isJust $ blk ^. BlockSlotNo)
    orderBy [desc (blk ^. BlockEpochNo)]
    limit 1
    pure (blk ^. BlockEpochNo)
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

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

-- | Given a 'SlotNo' return the 'SlotNo' of the previous block.
queryPreviousSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Word64)
queryPreviousSlotNo slotNo = do
  res <- select $ do
    (blk :& pblk) <-
      from $ table @Block
      `innerJoin` table @Block
      `on` (\(blk :& pblk) -> blk ^. BlockPreviousId ==. just (pblk ^. BlockId))
    where_ (blk ^. BlockSlotNo ==. just (val slotNo))
    pure $ pblk ^. BlockSlotNo
  pure $ unValue =<< listToMaybe res

querySchemaVersion :: MonadIO m => ReaderT SqlBackend m (Maybe SchemaVersion)
querySchemaVersion = do
  res <- select $ do
    sch <- from $ table @SchemaVersion
    orderBy [desc (sch ^. SchemaVersionStageOne)]
    limit 1
    pure (sch ^. SchemaVersionStageOne, sch ^. SchemaVersionStageTwo, sch ^. SchemaVersionStageThree)
  pure $ uncurry3 SchemaVersion . unValue3 <$> listToMaybe res

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

queryDatumPage :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity Datum]
queryDatumPage ofs lmt =
  select $ do
      datum <- from $ table @Datum
      orderBy [asc (datum ^. DatumId)]
      limit lmt
      offset ofs
      pure datum

queryDatumCount :: MonadIO m => ReaderT SqlBackend m Word64
queryDatumCount = do
  xs <- select $ do
    _ <- from $ table @Datum
    pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

querydatumInfo :: MonadIO m => DatumId -> ReaderT SqlBackend m (Maybe (ByteString, Maybe Word64))
querydatumInfo datumId = do
  res <- select $ do
    (_blk :& _tx :& datum :& prevBlock) <-
      from $ table @Block
      `innerJoin` table @Tx
      `on` (\(blk :& tx) -> tx ^. TxBlockId ==. blk ^. BlockId)
      `innerJoin` table @Datum
      `on` (\(_blk :& tx :& datum) -> datum ^. DatumTxId ==. tx ^. TxId)
      `innerJoin` table @Block
      `on` (\(blk :& _tx :& _datum :& prevBlk) -> blk ^. BlockPreviousId ==. just (prevBlk ^. BlockId))
    where_ (datum ^. DatumId ==. val datumId)
    pure (prevBlock ^. BlockHash, prevBlock ^. BlockSlotNo)
  pure $ unValue2 <$> listToMaybe res

queryRedeemerData :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe RedeemerDataId)
queryRedeemerData hsh = do
  xs <- select $ do
      rdmrDt <- from $ table @RedeemerData
      where_ (rdmrDt ^. RedeemerDataHash ==. val hsh)
      pure (rdmrDt ^. RedeemerDataId)
  pure $ unValue <$> listToMaybe xs

queryRedeemerDataPage :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity RedeemerData]
queryRedeemerDataPage ofs lmt =
  select $ do
      redeemerData <- from $ table @RedeemerData
      orderBy [asc (redeemerData ^. RedeemerDataId)]
      limit lmt
      offset ofs
      pure redeemerData

queryRedeemerDataCount :: MonadIO m => ReaderT SqlBackend m Word64
queryRedeemerDataCount = do
  xs <- select $ do
    _ <- from $ table @RedeemerData
    pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

queryRedeemerDataInfo :: MonadIO m => RedeemerDataId -> ReaderT SqlBackend m (Maybe (ByteString, Maybe Word64))
queryRedeemerDataInfo rdmDataId = do
  res <- select $ do
    (_blk :& _tx :& rdmData :& prevBlock) <-
      from $ table @Block
      `innerJoin` table @Tx
      `on` (\(blk :& tx) -> tx ^. TxBlockId ==. blk ^. BlockId)
      `innerJoin` table @RedeemerData
      `on` (\(_blk :& tx :& rdmData) -> rdmData ^. RedeemerDataTxId ==. tx ^. TxId)
      `innerJoin` table @Block
      `on` (\(blk :& _tx :& _rdmData :& prevBlk) -> blk ^. BlockPreviousId ==. just (prevBlk ^. BlockId))
    where_ (rdmData ^. RedeemerDataId ==. val rdmDataId)
    pure (prevBlock ^. BlockHash, prevBlock ^. BlockSlotNo)
  pure $ unValue2 <$> listToMaybe res

-- | Count the number of rows that match the select with the supplied predicate.
querySelectCount :: forall table table' m . (MonadIO m, PersistEntity table, ToFrom (From (SqlExpr (Entity table))) table')
                 => (table' -> SqlQuery ()) -> ReaderT SqlBackend m Word
querySelectCount predicate = do
  xs <- select $ do
    x <- from (table @table)
    predicate x
    pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

querySlotHash :: MonadIO m => SlotNo -> ReaderT SqlBackend m [(SlotNo, ByteString)]
querySlotHash slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo ==. just (val $ unSlotNo slotNo))
    pure (blk ^. BlockHash)
  pure $ (\vh -> (slotNo, unValue vh)) <$> res

{-# INLINABLE queryMeta #-}
-- | Get the network metadata.
queryMeta :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Meta)
queryMeta = do
  res <- select . from $ table @Meta
  pure $ case res of
            [] -> Left DbMetaEmpty
            [m] -> Right $ entityVal m
            _ -> Left DbMetaMultipleRows

queryMultiAssetId :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe MultiAssetId)
queryMultiAssetId policy assetName = do
  res <- select $ do
    ma <- from $ table @MultiAsset
    where_ (ma ^. MultiAssetPolicy ==. val policy &&. ma ^. MultiAssetName ==. val assetName)
    pure (ma ^. MultiAssetId)
  pure $ unValue <$> listToMaybe res

-- | Get the network name from the Meta table.
queryNetworkName :: MonadIO m => ReaderT SqlBackend m (Maybe Text)
queryNetworkName = do
  res <- select $ do
    meta <- from $ table @Meta
    pure (meta ^. MetaNetworkName)
  pure $ unValue <$> listToMaybe res

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

queryLastSlotNoGreaterThan :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe SlotNo)
queryLastSlotNoGreaterThan slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    -- Want all BlockNos where the block satisfies this predicate.
    where_ (blk ^. BlockSlotNo >. just (val slotNo))
    -- Return them in descending order so we can delete the highest numbered
    -- ones first.
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (blk ^. BlockSlotNo)
  pure $ listToMaybe $ mapMaybe (fmap SlotNo . unValue) res

queryCountSlotNosGreaterThan :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryCountSlotNosGreaterThan slotNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo >. just (val slotNo))
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Like 'queryLastSlotNoGreaterThan', but returns all slots in the same order.
queryLastSlotNo :: MonadIO m => ReaderT SqlBackend m (Maybe SlotNo)
queryLastSlotNo = do
  res <- select $ do
    blk <- from $ table @Block
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (blk ^. BlockSlotNo)
  pure $ listToMaybe $ mapMaybe (fmap SlotNo . unValue) res

-- | Like 'queryCountSlotNosGreaterThan', but returns all slots in the same order.
queryCountSlotNo :: MonadIO m => ReaderT SqlBackend m Word64
queryCountSlotNo = do
  res <- select $ do
    _ <- from $ table @Block
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Calculate the slot time (as UTCTime) for a given slot number.
-- This will fail if the slot is empty.
querySlotUtcTime :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail UTCTime)
querySlotUtcTime slotNo = do
    le <- select $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockSlotNo ==. just (val slotNo))
      pure (blk ^. BlockTime)
    pure $ maybe (Left $ DbLookupSlotNo slotNo) (Right . unValue) (listToMaybe le)

-- | Get the current total supply of Lovelace. This only returns the on-chain supply which
-- does not include staking rewards that have not yet been withdrawn. Before wihdrawal
-- rewards are part of the ledger state and hence not on chain.
queryTotalSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryTotalSupply = do
    res <- select $ do
      txOut <- from $ table @TxOut
      txOutUnspentP txOut
      pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxCount = do
  res <- select $ do
    _ <- from $ table @Tx
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the 'TxId' associated with the given hash.
queryTxId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail TxId)
queryTxId hash = do
  res <- select $ do
    tx <- from $ table @Tx
    where_ (tx ^. TxHash ==. val hash)
    pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityKey (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxInCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxInCount = do
  res <- select $ from (table @TxIn) >> pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of transaction outputs in the TxOut table.
queryTxOutCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxOutCount = do
  res <- select $ from (table @TxOut) >> pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Give a (tx hash, index) pair, return the TxOut value.
queryTxOutValue :: MonadIO m => (ByteString, Word64) -> ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
queryTxOutValue (hash, index) = do
  res <- select $ do
    (tx :& txOut) <- from $ table @Tx
      `innerJoin` table @TxOut
      `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. TxOutTxId)
    where_ (txOut ^. TxOutIndex ==. val index &&. tx ^. TxHash ==. val hash)
    pure (txOut ^. TxOutTxId, txOut ^. TxOutValue)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

-- | Give a (tx hash, index) pair, return the TxOut Credentials.
queryTxOutCredentials :: MonadIO m => (ByteString, Word64) -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryTxOutCredentials (hash, index) = do
  res <- select $ do
    (tx :& txOut) <- from $ table @Tx
      `innerJoin` table @TxOut
      `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. TxOutTxId)
    where_ (txOut ^. TxOutIndex ==. val index &&. tx ^. TxHash ==. val hash)
    pure (txOut ^. TxOutPaymentCred, txOut ^. TxOutAddressHasScript)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

-- | Get the UTxO set after the specified 'BlockId' has been applied to the chain.
-- Not exported because 'BlockId' to 'BlockHash' relationship may not be the same
-- across machines.
queryUtxoAtBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtBlockId blkid = do
    outputs <- select $ do
      (txout :& _txin :& _tx1 :& blk :& tx2) <-
        from $ table @TxOut
        `leftJoin` table @TxIn
        `on` (\(txout :& txin) -> (just (txout ^. TxOutTxId) ==. txin ?. TxInTxOutId) &&.
                                  (just (txout ^. TxOutIndex) ==. txin ?. TxInTxOutIndex))
        `leftJoin` table @Tx
        `on` (\(_txout :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
        `leftJoin` table @Block
        `on` (\(_txout :& _txin :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
        `leftJoin` table @Tx
        `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. TxOutTxId) ==. tx2 ?. TxId )

      where_ $ (txout ^. TxOutTxId `in_` txLessEqual blkid) &&.
        (isNothing (blk ?. BlockBlockNo)||. (blk ?. BlockId >. just (val blkid)))
      pure (txout, tx2 ?. TxHash)
    pure $ mapMaybe convert outputs
  where
    convert :: (Entity TxOut, Value (Maybe ByteString)) -> Maybe (TxOut, ByteString)
    convert = \case
      (out, Value (Just hash')) -> Just (entityVal out, hash')
      (_, Value Nothing) -> Nothing

queryEpochStakeCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryEpochStakeCount epoch = do
  res <- select $ do
    epochStake <- from $ table @ EpochStake
    where_ (epochStake ^. EpochStakeEpochNo ==. val epoch)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryUtxoAtBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtBlockNo blkNo = do
  eblkId <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo ==. just (val blkNo))
    pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId . unValue) (listToMaybe eblkId)

queryUtxoAtSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtSlotNo slotNo = do
  eblkId <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo ==. just (val slotNo))
    pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId . unValue) (listToMaybe eblkId)

queryWithdrawalsUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryWithdrawalsUpToBlockNo blkNo = do
  res <- select $ do
    (_tx :& wdrl :& blk) <-
      from $ table @Tx
      `innerJoin` table @Withdrawal
      `on` (\(tx :& wdrl) -> tx ^. TxId ==. wdrl ^. WithdrawalTxId)
      `innerJoin` table @Block
      `on` (\(tx :& _wdrl :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)

    where_ (blk ^. BlockBlockNo <=. just (val blkNo))
    pure $ sum_ (wdrl ^. WithdrawalAmount)
  pure $ unValueSumAda (listToMaybe res)

queryCostModel :: MonadIO m => ReaderT SqlBackend m [CostModelId]
queryCostModel =
  fmap entityKey <$> selectList [] [Asc CostModelId]

queryAdaPots :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe AdaPots)
queryAdaPots blkId = do
  res <- select $ do
    adaPots <- from $ table @AdaPots
    where_ (adaPots  ^. AdaPotsBlockId ==. val blkId)
    pure adaPots
  pure $ fmap entityVal (listToMaybe res)

queryUsedTicker :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe Text)
queryUsedTicker poolHash metaHash = do
  res <- select $ do
    (pod :& ph) <-
      from $ table @PoolOfflineData
      `innerJoin` table @PoolHash
      `on` (\(pod :& ph) -> ph ^. PoolHashId ==. pod ^. PoolOfflineDataPoolId)
    where_ (ph ^. PoolHashHashRaw ==. val poolHash)
    where_ (pod ^. PoolOfflineDataHash ==. val metaHash)
    pure $ pod ^. PoolOfflineDataTickerName
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

queryPoolOfflineData :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe (Text, ByteString))
queryPoolOfflineData poolHash poolMetadataHash = do
  res <- select $ do
    (pod :& ph) <-
      from $ table @PoolOfflineData
      `innerJoin` table @PoolHash
      `on` (\(pod :& ph) -> pod ^. PoolOfflineDataPoolId ==. ph ^. PoolHashId)
    where_ (ph ^. PoolHashHashRaw ==.  val poolHash)
    where_ (pod  ^. PoolOfflineDataHash ==. val poolMetadataHash)
    limit 1
    pure (pod ^. PoolOfflineDataTickerName, pod ^. PoolOfflineDataBytes)
  pure $ unValue2 <$> listToMaybe res

queryPoolRegister :: MonadIO m => Maybe ByteString -> ReaderT SqlBackend m [PoolCert]
queryPoolRegister mPoolHash = do
    res <- select $ do
      (poolUpdate :& poolHash :& poolMeta :& tx :& blk) <-
        from $ table @PoolUpdate
        `innerJoin` table @PoolHash
        `on` (\(poolUpdate :& poolHash) -> poolUpdate ^. PoolUpdateHashId ==. poolHash ^. PoolHashId)
        `innerJoin` table @PoolMetadataRef
        `on` (\(poolUpdate :& _poolHash :& poolMeta) -> poolUpdate ^. PoolUpdateMetaId ==.  just (poolMeta ^. PoolMetadataRefId))
        `innerJoin` table @Tx
        `on` (\(poolUpdate :& _poolHash :& _poolMeta :& tx) -> poolUpdate ^. PoolUpdateRegisteredTxId ==. tx ^. TxId)
        `innerJoin` table @Block
        `on` (\(_poolUpdate :& _poolHash :& _poolMeta :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)

      whenJust mPoolHash $ \ph ->
        where_ (poolHash  ^. PoolHashHashRaw ==. val ph)
      pure (poolHash ^. PoolHashHashRaw, poolMeta ^. PoolMetadataRefHash, blk ^. BlockBlockNo,
            tx ^. TxBlockIndex, poolUpdate ^. PoolUpdateCertIndex)
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
        from $ table @PoolRetire
        `innerJoin` table @PoolHash
        `on` (\(retired :& poolHash) -> retired ^. PoolRetireHashId ==. poolHash ^. PoolHashId)
        `innerJoin` table @Tx
        `on` (\(retired :& _poolHash :& tx) -> retired ^. PoolRetireAnnouncedTxId ==. tx ^. TxId)
        `innerJoin` table @Block
        `on` (\(_retired :& _poolHash :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
      whenJust mPoolHash $ \ph ->
        where_ (poolHash  ^. PoolHashHashRaw ==. val ph)
      pure (poolHash ^. PoolHashHashRaw, retired ^. PoolRetireRetiringEpoch,
             blk ^. BlockBlockNo, tx ^. TxBlockIndex, retired ^. PoolRetireCertIndex)
    pure $ toRetirementInfo . unValue5 <$> res
  where
    toRetirementInfo (hsh, retEpoch, blkNo, txIndex, retIndex) =
      PoolCert
        { pcHash = hsh
        , pcCertAction = Retirement retEpoch
        , pcCertNo = CertNo blkNo txIndex retIndex
        }

-- Return delisted Pool hashes.
queryDelistedPools :: MonadIO m => ReaderT SqlBackend m [ByteString]
queryDelistedPools = do
  res <- select $ do
    delistedPool <- from $ table @DelistedPool
    pure $ delistedPool ^. DelistedPoolHashRaw
  pure $ unValue <$> res

-- Returns also the metadata hash
queryPoolOfflineFetchError :: MonadIO m => ByteString -> Maybe UTCTime -> ReaderT SqlBackend m [(PoolOfflineFetchError, ByteString)]
queryPoolOfflineFetchError hash Nothing = do
    res <- select $ do
      (poolOfflineFetchError :& poolHash :& poolMetadataRef) <- from $ table @PoolOfflineFetchError
        `innerJoin` table @PoolHash
        `on` (\(poolOfflineFetchError :& poolHash) -> poolOfflineFetchError ^. PoolOfflineFetchErrorPoolId ==. poolHash ^. PoolHashId)
        `innerJoin` table @PoolMetadataRef
        `on` (\(poolOfflineFetchError :& _ :& poolMetadataRef) -> poolOfflineFetchError ^. PoolOfflineFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)

      where_ (poolHash ^. PoolHashHashRaw ==. val hash)
      orderBy [desc (poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime)]
      limit 10
      pure (poolOfflineFetchError, poolMetadataRef ^. PoolMetadataRefHash)
    pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)

queryPoolOfflineFetchError hash (Just fromTime) = do
    res <- select $ do
      (poolOfflineFetchError :& poolHash :& poolMetadataRef) <-
        from $ table @PoolOfflineFetchError
        `innerJoin` table @PoolHash
        `on` (\(poolOfflineFetchError :& poolHash) -> poolOfflineFetchError ^. PoolOfflineFetchErrorPoolId ==. poolHash ^. PoolHashId)
        `innerJoin` table @PoolMetadataRef
        `on` (\(poolOfflineFetchError :& _poolHash :& poolMetadataRef) -> poolOfflineFetchError ^. PoolOfflineFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)
      where_ (poolHash ^. PoolHashHashRaw ==. val hash
               &&. poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime >=. val fromTime)
      orderBy [desc (poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime)]
      limit 10
      pure (poolOfflineFetchError, poolMetadataRef ^. PoolMetadataRefHash)
    pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)

queryScriptOutputs :: MonadIO m => ReaderT SqlBackend m [TxOut]
queryScriptOutputs = do
    res <- select $ do
      tx_out <- from $ table @TxOut
      where_ (tx_out ^. TxOutAddressHasScript ==. val True)
      pure tx_out
    pure $ entityVal <$> res

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
        from $ table @TxIn
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

queryStakeAddressIdsAfter :: MonadIO m => Word64 -> Bool -> ReaderT SqlBackend m [StakeAddressId]
queryStakeAddressIdsAfter blockNo queryEq = do
    res <- select $ do
      (_tx :& blk :& st_addr) <-
        from $ table @Tx
        `innerJoin` table @Block
        `on` (\(tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
        `innerJoin` table @StakeAddress
        `on` (\(tx :& _blk :& st_addr) -> tx ^. TxId  ==. st_addr ^. StakeAddressTxId)
      where_ (isJust $ blk ^. BlockBlockNo)
      where_ (if queryEq then blk ^. BlockBlockNo >=. val (Just blockNo) 
                         else blk ^. BlockBlockNo >.  val (Just blockNo))
      pure (st_addr ^. StakeAddressId)
    pure $ unValue <$> res

existsDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
existsDelistedPool ph = do
  res <- select $ do
    delistedPool <- from $ table @DelistedPool
    where_ (delistedPool ^. DelistedPoolHashRaw ==. val ph)
    limit 1
    pure (delistedPool ^. DelistedPoolId)
  pure $ not (null res)

existsPoolHash :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
existsPoolHash hsh = do
  res <- select $ do
    poolHash <- from $ table @PoolHash
    where_ (poolHash  ^. PoolHashHashRaw ==. val hsh)
    limit 1
    pure (poolHash ^. PoolHashId)
  pure $ not (null res)

existsPoolHashId :: MonadIO m => PoolHashId -> ReaderT SqlBackend m Bool
existsPoolHashId phid = do
  res <- select $ do
    poolHash <- from $ table @PoolHash
    where_ (poolHash  ^. PoolHashId ==. val phid)
    limit 1
    pure (poolHash ^. PoolHashId)
  pure $ not (null res)

existsPoolMetadataRefId :: MonadIO m => PoolMetadataRefId -> ReaderT SqlBackend m Bool
existsPoolMetadataRefId pmrid = do
  res <- select $ do
    pmr <- from $ table @PoolMetadataRef
    where_ (pmr  ^. PoolMetadataRefId ==. val pmrid)
    limit 1
    pure (pmr  ^. PoolMetadataRefId)
  pure $ not (null res)

-- -----------------------------------------------------------------------------
-- SqlQuery predicates

-- Filter out 'Nothing' from a 'Maybe a'.
isJust :: PersistField a => SqlExpr (Value (Maybe a)) -> SqlExpr (Value Bool)
isJust = not_ . isNothing

-- Returns True if the TxOut has been spent.
{-# INLINABLE txOutSpentB #-}
txOutSpentB :: SqlExpr (Entity TxOut) -> SqlExpr (Value Bool)
txOutSpentB txOut =
  exists $ from (table @TxIn) >>= \ txIn ->
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- A predicate that filters out unspent 'TxOut' entries.
{-# INLINABLE txOutSpentP #-}
txOutSpentP :: SqlExpr (Entity TxOut) -> SqlQuery ()
txOutSpentP txOut =
  where_ . exists $ from (table @TxIn) >>= \ txIn ->
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- A predicate that filters out spent 'TxOut' entries.
{-# INLINABLE txOutUnspentP #-}
txOutUnspentP :: SqlExpr (Entity TxOut) -> SqlQuery ()
txOutUnspentP txOut =
  where_ . notExists  $ from (table @TxIn) >>= \ txIn ->
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- every tx made before or at the snapshot time
txLessEqual :: BlockId -> SqlExpr (ValueList TxId)
txLessEqual blkid =
    subList_select $ from (table @Tx) >>= \ tx -> do
      where_ $ tx ^. TxBlockId `in_` blockLessEqual
      pure $ tx ^. TxId
  where
    -- every block made before or at the snapshot time
    blockLessEqual :: SqlExpr (ValueList BlockId)
    blockLessEqual = subList_select $ from (table @Block) >>= \blk -> do
      where_ $ blk ^. BlockId <=. val blkid
      pure $ blk ^. BlockId

-- | Get the UTxO set after the specified 'BlockNo' has been applied to the chain.
-- Unfortunately the 'sum_' operation above returns a 'PersistRational' so we need
-- to un-wibble it.
unValueSumAda :: Maybe (Value (Maybe Micro)) -> Ada
unValueSumAda mvm =
  case fmap unValue mvm of
    Just (Just x) -> lovelaceToAda x
    _ -> Ada 0

-- -----------------------------------------------------------------------------

entityPair :: Entity a -> (Key a, a)
entityPair e =
  (entityKey e, entityVal e)

maybeToEither :: e -> (a -> b) -> Maybe a -> Either e b
maybeToEither e f =
  maybe (Left e) (Right . f)

unBlockId :: BlockId -> Word64
unBlockId = fromIntegral . unSqlBackendKey . unBlockKey

unTxId :: TxId -> Word64
unTxId = fromIntegral . unSqlBackendKey . unTxKey

unTxInId :: TxInId -> Word64
unTxInId = fromIntegral . unSqlBackendKey . unTxInKey

unTxOutId :: TxOutId -> Word64
unTxOutId = fromIntegral . unSqlBackendKey . unTxOutKey

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)

unValue3 :: (Value a, Value b, Value c) -> (a, b, c)
unValue3 (a, b, c) = (unValue a, unValue b, unValue c)

unValue4 :: (Value a, Value b, Value c, Value d) -> (a, b, c, d)
unValue4 (a, b, c, d) = (unValue a, unValue b, unValue c, unValue d)

unValue5 :: (Value a, Value b, Value c, Value d, Value e) -> (a, b, c, d, e)
unValue5 (a, b, c, d, e) = (unValue a, unValue b, unValue c, unValue d, unValue e)
