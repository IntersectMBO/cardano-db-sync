{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Query
  ( LookupFail (..)
  , queryAddressBalanceAtSlot
  , queryGenesis
  , queryBlock
  , queryBlockCount
  , queryBlockHeight
  , queryBlockId
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
  , queryEpochRewardCount
  , queryFeesUpToBlockNo
  , queryFeesUpToSlotNo
  , queryGenesisSupply
  , queryShelleyGenesisSupply
  , queryLatestBlock
  , queryLatestCachedEpochNo
  , queryLatestEpochNo
  , queryLatestBlockId
  , queryLatestBlockNo
  , queryLatestSlotNo
  , queryMeta
  , queryMultiAssetId
  , queryNetworkName
  , queryPreviousSlotNo
  , querySchemaVersion
  , querySelectCount
  , querySlotHash
  , querySlotNosGreaterThan
  , querySlotNos
  , querySlotUtcTime
  , queryTotalSupply
  , queryTxCount
  , queryTxId
  , queryTxInCount
  , queryTxOutCount
  , queryTxOutValue
  , queryTxOutCredentials
  , queryUtxoAtBlockNo
  , queryUtxoAtSlotNo
  , queryWithdrawalsUpToBlockNo
  , queryAdaPots
  , queryPoolOfflineData
  , queryPoolRegister
  , queryRetiredPools
  , queryUsedTicker
  , queryReservedTicker
  , queryReservedTickers
  , queryDelistedPools
  , queryPoolOfflineFetchError
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
  , unValueSumAda
  ) where


import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.Extra (join, mapMaybeM, whenJust)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Fixed (Micro)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ratio (numerator)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime (..))
import           Data.Tuple.Extra (uncurry3)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto.Legacy (Entity (..), From, InnerJoin (..), LeftOuterJoin (..),
                   PersistField, SqlExpr, SqlQuery, Value (..), ValueList, count, countRows, desc,
                   entityKey, entityVal, exists, from, in_, isNothing, just, limit, max_, min_,
                   notExists, not_, on, orderBy, select, subList_select, sum_, unSqlBackendKey,
                   unValue, val, where_, (&&.), (<=.), (==.), (>.), (>=.), (^.), (||.))
import           Database.Persist.Sql (SqlBackend, selectList)

import           Cardano.Db.Error
import           Cardano.Db.Schema
import           Cardano.Db.Types

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant ^." -}

-- If you squint, these Esqueleto queries almost look like SQL queries.


queryAddressBalanceAtSlot :: MonadIO m => Text -> Word64 -> ReaderT SqlBackend m Ada
queryAddressBalanceAtSlot addr slotNo = do
    eblkId <- select . from $ \blk -> do
                  where_ (blk ^. BlockSlotNo ==. just (val slotNo))
                  pure (blk ^. BlockId)
    maybe (pure 0) (queryAddressBalanceAtBlockId . unValue) (listToMaybe eblkId)
  where
    queryAddressBalanceAtBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
    queryAddressBalanceAtBlockId blkid = do
        -- tx1 refers to the tx of the input spending this output (if it is ever spent)
        -- tx2 refers to the tx of the output
        res <- select . from $ \(txout `LeftOuterJoin` txin `LeftOuterJoin` tx1 `LeftOuterJoin` blk `LeftOuterJoin` tx2) -> do
                  on $ txout ^. TxOutTxId ==. tx2 ^. TxId
                  on $ tx1 ^. TxBlockId ==. blk ^. BlockId
                  on $ txin ^. TxInTxInId ==. tx1 ^. TxId
                  on $ (txout ^. TxOutTxId ==. txin ^. TxInTxOutId) &&. (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
                  where_ $ (txout ^. TxOutTxId `in_` txLessEqual blkid) &&. (isNothing (blk ^. BlockBlockNo) ||. (blk ^. BlockId >. val blkid))
                  where_ (txout ^. TxOutAddress ==. val addr)
                  pure $ sum_ (txout ^. TxOutValue)
        pure $ unValueSumAda (listToMaybe res)

queryGenesis :: MonadIO m => ReaderT SqlBackend m (Either LookupFail BlockId)
queryGenesis = do
  res <- select . from $ \ blk -> do
            where_ (isNothing (blk ^. BlockPreviousId))
            pure $ blk ^. BlockId
  case res of
    [blk] -> pure $ Right (unValue blk)
    _ -> pure $ Left DBMultipleGenesis

-- | Get the 'Block' associated with the given hash.
queryBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail Block)
queryBlock hash = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockHash ==. val hash)
            pure blk
  pure $ maybeToEither (DbLookupBlockHash hash) entityVal (listToMaybe res)


-- | Count the number of blocks in the Block table.
queryBlockCount :: MonadIO m => ReaderT SqlBackend m Word
queryBlockCount = do
  res <- select . from $ \ (_ :: SqlExpr (Entity Block)) -> do
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the 'BlockId' associated with the given hash.
queryBlockId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail BlockId)
queryBlockId hash = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockHash ==. val hash)
            pure $ blk ^. BlockId
  pure $ maybeToEither (DbLookupBlockHash hash) unValue (listToMaybe res)

-- | Get the 'SlotNo' associated with the given hash.
queryBlockSlotNo :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (Maybe Word64))
queryBlockSlotNo hash = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockHash ==. val hash)
            pure $ blk ^. BlockSlotNo
  pure $ maybeToEither (DbLookupBlockHash hash) unValue (listToMaybe res)

queryBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Block)
queryBlockNo blkNo = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockBlockNo ==. just (val blkNo))
            pure blk
  pure $ fmap entityVal (listToMaybe res)

-- | Get the current block height.
queryBlockHeight :: MonadIO m => ReaderT SqlBackend m Word64
queryBlockHeight = do
  res <- select . from $ \ blk -> do
          where_ (isJust $ blk ^. BlockBlockNo)
          orderBy [desc (blk ^. BlockBlockNo)]
          limit 1
          pure (blk ^. BlockBlockNo)
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

-- | Get the latest 'Block' associated with the given hash, skipping any EBBs.
queryMainBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail Block)
queryMainBlock hash = do
    res <- select . from $ \ blk -> do
              where_ (blk ^. BlockHash ==. val hash)
              pure $ blk ^. BlockId
    maybe (pure $ Left (DbLookupBlockHash hash)) queryMainBlockId (unValue <$> listToMaybe res)
  where
    queryMainBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m (Either LookupFail Block)
    queryMainBlockId blkid = do
      res <- select . from $ \ blk -> do
              where_ (isJust (blk ^. BlockBlockNo) &&. blk ^. BlockId <=. val blkid)
              orderBy [desc (blk ^. BlockSlotNo)]
              limit 1
              pure blk
      pure $ maybeToEither (DbLookupBlockId $ unBlockId blkid) entityVal (listToMaybe res)

-- | Get the number of transactions in the specified block.
queryBlockTxCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word64
queryBlockTxCount blkId = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxBlockId ==. val blkId)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryBlocksAfterSlot :: MonadIO m => Word64 -> ReaderT SqlBackend m Int
queryBlocksAfterSlot slotNo = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockSlotNo >. just (val slotNo))
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epcoh entry for the last epoch.
-- When following the chain, this is called for each new block of the current epoch.
queryCalcEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m Epoch
queryCalcEpochEntry epochNum = do
    blkRes <- select . from $ \ blk -> do
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure (countRows, min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    txRes <- select . from $ \ (tx `InnerJoin` blk) -> do
              on (tx ^. TxBlockId ==. blk ^. BlockId)
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
    latest <- select $ from $ \ blk -> do
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
       rows <- select $ from $ \ blk -> do
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
  res <- select . from $ \ (tx `InnerJoin` blk) -> do
            on (tx ^. TxBlockId ==. blk ^. BlockId)
            where_ (blk ^. BlockBlockNo <=. just (val blkNo))
            pure $ sum_ (tx ^. TxDeposit)
  pure $ unValueSumAda (listToMaybe res)

queryEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail Epoch)
queryEpochEntry epochNum = do
    res <- select . from $ \ epoch -> do
              where_ (epoch ^. EpochNo ==. val epochNum)
              pure epoch
    pure $ maybeToEither (DbLookupEpochNo epochNum) entityVal (listToMaybe res)

-- | Get the Epoch number for a given block. Returns '0' for the genesis block
-- even though the DB entry for the genesis block is 'NULL'.
queryEpochNo :: MonadIO m => BlockId -> ReaderT SqlBackend m (Either LookupFail (Maybe Word64))
queryEpochNo blkId = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockId ==. val blkId)
            pure $ blk ^. BlockEpochNo
  pure $ maybeToEither (DbLookupBlockId $ unBlockId blkId) unValue (listToMaybe res)

queryCurrentEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryCurrentEpochNo = do
    res <- select . from $ \ blk -> do
              pure $ max_ (blk ^. BlockEpochNo)
    pure $ join (unValue =<< listToMaybe res)

queryEpochRewardCount :: MonadIO m => Word64 -> ReaderT SqlBackend m Word64
queryEpochRewardCount epochNum = do
  res <- select . from $ \ rwds -> do
            where_ (rwds ^. RewardSpendableEpoch ==. val epochNum)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Get the fees paid in all block from genesis up to and including the specified block.
queryFeesUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToBlockNo blkNo = do
  res <- select . from $ \ (tx `InnerJoin` blk) -> do
            on (tx ^. TxBlockId ==. blk ^. BlockId)
            where_ (blk ^. BlockBlockNo <=. just (val blkNo))
            pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

queryFeesUpToSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToSlotNo slotNo = do
  res <- select . from $ \ (tx `InnerJoin` blk) -> do
            on (tx ^. TxBlockId ==. blk ^. BlockId)
            where_ (isJust $ blk ^. BlockSlotNo)
            where_ (blk ^. BlockSlotNo <=. just (val slotNo))
            pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

-- | Return the total Genesis coin supply.
queryGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryGenesisSupply = do
    res <- select . from $ \ (txOut `InnerJoin` tx `InnerJoin` blk) -> do
                on (tx ^. TxBlockId ==. blk ^. BlockId)
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                where_ (isNothing $ blk ^. BlockPreviousId)
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Return the total Shelley Genesis coin supply. The Shelley Genesis Block
-- is the unique which has a non-null PreviousId, but has null Epoch.
queryShelleyGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryShelleyGenesisSupply = do
    res <- select . from $ \ (txOut `InnerJoin` tx `InnerJoin` blk) -> do
                on (tx ^. TxBlockId ==. blk ^. BlockId)
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                where_ (isJust $ blk ^. BlockPreviousId)
                where_ (isNothing $ blk ^. BlockEpochNo)
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => ReaderT SqlBackend m (Maybe BlockId)
queryLatestBlockId = do
  res <- select $ from $ \ blk -> do
                orderBy [desc (blk ^. BlockSlotNo)]
                limit 1
                pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

-- | Get the 'BlockNo' of the latest block.
queryLatestBlockNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestBlockNo = do
  res <- select $ from $ \ blk -> do
                where_ (isJust $ blk ^. BlockBlockNo)
                orderBy [desc (blk ^. BlockBlockNo)]
                limit 1
                pure $ blk ^. BlockBlockNo
  pure $ listToMaybe (mapMaybe unValue res)

-- | Get the latest block.
queryLatestBlock :: MonadIO m => ReaderT SqlBackend m (Maybe Block)
queryLatestBlock = do
  res <- select $ from $ \ blk -> do
                where_ (isJust $ blk ^. BlockSlotNo)
                orderBy [desc (blk ^. BlockSlotNo)]
                limit 1
                pure blk
  pure $ fmap entityVal (listToMaybe res)

queryLatestCachedEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestCachedEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [desc (epoch ^. EpochNo)]
            limit 1
            pure (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res

queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestEpochNo = do
  res <- select . from $ \ blk -> do
            where_ (isJust $ blk ^. BlockSlotNo)
            orderBy [desc (blk ^. BlockEpochNo)]
            limit 1
            pure (blk ^. BlockEpochNo)
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

-- | Get the latest slot number
queryLatestSlotNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestSlotNo = do
  res <- select . from $ \ blk -> do
            where_ (isJust $ blk ^. BlockSlotNo)
            orderBy [desc (blk ^. BlockSlotNo)]
            limit 1
            pure $ blk ^. BlockSlotNo
  pure $ fromMaybe 0 (unValue =<< listToMaybe res)

-- | Given a 'SlotNo' return the 'SlotNo' of the previous block.
queryPreviousSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Word64)
queryPreviousSlotNo slotNo = do
  res <- select . from $ \ (blk `InnerJoin` pblk) -> do
            on (blk ^. BlockPreviousId ==. just (pblk ^. BlockId))
            where_ (blk ^. BlockSlotNo ==. just (val slotNo))
            pure $ pblk ^. BlockSlotNo
  pure $ unValue =<< listToMaybe res

querySchemaVersion :: MonadIO m => ReaderT SqlBackend m (Maybe SchemaVersion)
querySchemaVersion = do
  res <- select . from $ \ sch -> do
            orderBy [desc (sch ^. SchemaVersionStageOne)]
            limit 1
            pure (sch ^. SchemaVersionStageOne, sch ^. SchemaVersionStageTwo, sch ^. SchemaVersionStageThree)
  pure $ uncurry3 SchemaVersion . unValue3 <$> listToMaybe res

-- | Count the number of rows that match the select with the supplied predicate.
querySelectCount :: (MonadIO m, From table) => (table -> SqlQuery ()) -> ReaderT SqlBackend m Word
querySelectCount predicate = do
  xs <- select . from $ \x -> do
            predicate x
            pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

querySlotHash :: MonadIO m => SlotNo -> ReaderT SqlBackend m [(SlotNo, ByteString)]
querySlotHash slotNo = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockSlotNo ==. just (val $ unSlotNo slotNo))
            pure (blk ^. BlockHash)
  pure $ (\vh -> (slotNo, unValue vh)) <$> res

{-# INLINABLE queryMeta #-}
-- | Get the network metadata.
queryMeta :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Meta)
queryMeta = do
  res <- select . from $ \ (meta :: SqlExpr (Entity Meta)) -> do
            pure meta
  pure $ case res of
            [] -> Left DbMetaEmpty
            [m] -> Right $ entityVal m
            _ -> Left DbMetaMultipleRows

queryMultiAssetId :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe MultiAssetId)
queryMultiAssetId policy assetName = do
  res <- select . from $ \ ma -> do
            where_ (ma ^. MultiAssetPolicy ==. val policy &&. ma ^. MultiAssetName ==. val assetName)
            pure (ma ^. MultiAssetId)
  pure $ unValue <$> listToMaybe res

-- | Get the network name from the Meta table.
queryNetworkName :: MonadIO m => ReaderT SqlBackend m (Maybe Text)
queryNetworkName = do
  res <- select . from $ \ meta -> do
            pure (meta ^. MetaNetworkName)
  pure $ unValue <$> listToMaybe res

querySlotNosGreaterThan :: MonadIO m => Word64 -> ReaderT SqlBackend m [SlotNo]
querySlotNosGreaterThan slotNo = do
  res <- select . from $ \ blk -> do
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
  res <- select . from $ \ blk -> do
            -- Return them in descending order so we can delete the highest numbered
            -- ones first.
            orderBy [desc (blk ^. BlockSlotNo)]
            pure (blk ^. BlockSlotNo)
  pure $ mapMaybe (fmap SlotNo . unValue) res

-- | Calculate the slot time (as UTCTime) for a given slot number.
-- This will fail if the slot is empty.
querySlotUtcTime :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail UTCTime)
querySlotUtcTime slotNo = do
    le <- select . from $ \ blk -> do
            where_ (blk ^. BlockSlotNo ==. just (val slotNo))
            pure (blk ^. BlockTime)
    pure $ maybe (Left $ DbLookupSlotNo slotNo) (Right . unValue) (listToMaybe le)

-- | Get the current total supply of Lovelace. This only returns the on-chain supply which
-- does not include staking rewards that have not yet been withdrawn. Before wihdrawal
-- rewards are part of the ledger state and hence not on chain.
queryTotalSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryTotalSupply = do
    res <- select . from $ \ txOut -> do
                txOutUnspentP txOut
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxCount = do
  res <- select . from $ \ (_ :: SqlExpr (Entity Tx)) -> do
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)


-- | Get the 'TxId' associated with the given hash.
queryTxId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail TxId)
queryTxId hash = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxHash ==. val hash)
            pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityKey (listToMaybe res)

-- | Count the number of transactions in the Tx table.
queryTxInCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxInCount = do
  res <- select . from $ \ (_ :: SqlExpr (Entity TxIn)) -> do
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of transaction outputs in the TxOut table.
queryTxOutCount :: MonadIO m => ReaderT SqlBackend m Word
queryTxOutCount = do
  res <- select . from $ \ (_ :: SqlExpr (Entity TxOut)) -> do
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Give a (tx hash, index) pair, return the TxOut value.
queryTxOutValue :: MonadIO m => (ByteString, Word16) -> ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
queryTxOutValue (hash, index) = do
  res <- select . from $ \ (tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            where_ (txOut ^. TxOutIndex ==. val index
                    &&. tx ^. TxHash ==. val hash
                    )
            pure (txOut ^. TxOutTxId, txOut ^. TxOutValue)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

-- | Give a (tx hash, index) pair, return the TxOut Credentials.
queryTxOutCredentials :: MonadIO m => (ByteString, Word16) -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryTxOutCredentials (hash, index) = do
  res <- select . from $ \ (tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            where_ (txOut ^. TxOutIndex ==. val index
                    &&. tx ^. TxHash ==. val hash
                    )
            pure (txOut ^. TxOutPaymentCred, txOut ^. TxOutAddressHasScript)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

-- | Get the UTxO set after the specified 'BlockId' has been applied to the chain.
-- Not exported because 'BlockId' to 'BlockHash' relationship may not be the same
-- across machines.
queryUtxoAtBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtBlockId blkid = do
    -- tx1 refers to the tx of the input spending this output (if it is ever spent)
    -- tx2 refers to the tx of the output
    outputs <- select . from $ \(txout `LeftOuterJoin` txin `LeftOuterJoin` tx1 `LeftOuterJoin` blk `LeftOuterJoin` tx2) -> do
                  on $ txout ^. TxOutTxId ==. tx2 ^. TxId
                  on $ tx1 ^. TxBlockId ==. blk ^. BlockId
                  on $ txin ^. TxInTxInId ==. tx1 ^. TxId
                  on $ (txout ^. TxOutTxId ==. txin ^. TxInTxOutId) &&. (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
                  where_ $ (txout ^. TxOutTxId `in_` txLessEqual blkid) &&. (isNothing (blk ^. BlockBlockNo) ||. (blk ^. BlockId >. val blkid))
                  pure (txout, tx2 ^. TxHash)
    pure $ map convert outputs
  where
    convert :: (Entity TxOut, Value ByteString) -> (TxOut, ByteString)
    convert (out, hash) = (entityVal out, unValue hash)

queryUtxoAtBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtBlockNo blkNo = do
  eblkId <- select . from $ \blk -> do
                where_ (blk ^. BlockBlockNo ==. just (val blkNo))
                pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId . unValue) (listToMaybe eblkId)

queryUtxoAtSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtSlotNo slotNo = do
  eblkId <- select . from $ \blk -> do
                where_ (blk ^. BlockSlotNo ==. just (val slotNo))
                pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId . unValue) (listToMaybe eblkId)

queryWithdrawalsUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryWithdrawalsUpToBlockNo blkNo = do
  res <- select . from $ \ (tx `InnerJoin` blk `InnerJoin` withDraw) -> do
            on (tx ^. TxId ==. withDraw ^. WithdrawalTxId)
            on (tx ^. TxBlockId ==. blk ^. BlockId)
            where_ (blk ^. BlockBlockNo <=. just (val blkNo))
            pure $ sum_ (withDraw ^. WithdrawalAmount)
  pure $ unValueSumAda (listToMaybe res)

queryAdaPots :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe AdaPots)
queryAdaPots blkId = do
  res <- select . from $ \ adaPots -> do
            where_ (adaPots  ^. AdaPotsBlockId ==. val blkId)
            pure adaPots
  pure $ fmap entityVal (listToMaybe res)

queryUsedTicker :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe Text)
queryUsedTicker poolHash metaHash = do
  res <- select . from $ \(pod `InnerJoin` ph) -> do
            on (ph ^. PoolHashId ==. pod ^. PoolOfflineDataPoolId)
            where_ (ph ^. PoolHashHashRaw ==. val poolHash)
            where_ (pod ^. PoolOfflineDataHash ==. val metaHash)
            pure $ pod ^. PoolOfflineDataTickerName
  pure $ unValue <$> listToMaybe res

queryReservedTicker :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe ByteString)
queryReservedTicker tickerName = do
  res <- select . from $ \ticker -> do
            where_ (ticker ^. ReservedPoolTickerName ==. val tickerName)
            pure $ ticker ^. ReservedPoolTickerPoolHash
  pure $ unValue <$> listToMaybe res

queryReservedTickers :: MonadIO m => ReaderT SqlBackend m [ReservedPoolTicker]
queryReservedTickers =
  fmap entityVal <$> selectList [] []

queryPoolOfflineData :: MonadIO m => ByteString -> ByteString -> ReaderT SqlBackend m (Maybe (Text, ByteString))
queryPoolOfflineData poolHash poolMetadataHash = do
  res <- select . from $ \ (pod `InnerJoin` ph) -> do
            on (pod ^. PoolOfflineDataPoolId ==. ph ^. PoolHashId)
            where_ (ph ^. PoolHashHashRaw ==.  val poolHash)
            where_ (pod  ^. PoolOfflineDataHash ==. val poolMetadataHash)
            limit 1
            pure (pod ^. PoolOfflineDataTickerName, pod ^. PoolOfflineDataBytes)
  pure $ unValue2 <$> listToMaybe res

queryPoolRegister :: MonadIO m => Maybe ByteString -> ReaderT SqlBackend m [PoolCert]
queryPoolRegister mPoolHash = do
    res <- select . from $ \(poolUpdate `InnerJoin` poolHash `InnerJoin` poolMeta `InnerJoin` tx `InnerJoin` blk) -> do
            on (poolUpdate ^. PoolUpdateHashId ==. poolHash ^. PoolHashId)
            on (poolUpdate ^. PoolUpdateMetaId ==.  just (poolMeta ^. PoolMetadataRefId))
            on (poolUpdate ^. PoolUpdateRegisteredTxId ==. tx ^. TxId)
            on (tx ^. TxBlockId ==. blk ^. BlockId)
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
    res <- select . from $ \(retired `InnerJoin` poolHash `InnerJoin` tx `InnerJoin` blk) -> do
            on (retired ^. PoolRetireHashId ==. poolHash ^. PoolHashId)
            on (retired ^. PoolRetireAnnouncedTxId ==. tx ^. TxId)
            on (tx ^. TxBlockId ==. blk ^. BlockId)
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
  res <- select  . from $ \ delistedPool -> do
            pure $ delistedPool ^. DelistedPoolHashRaw
  pure $ unValue <$> res

-- Returns also the metadata hash
queryPoolOfflineFetchError :: MonadIO m => ByteString -> Maybe UTCTime -> ReaderT SqlBackend m [(PoolOfflineFetchError, ByteString)]
queryPoolOfflineFetchError poolHash Nothing = do
    res <- select . from $ \(poolOfflineFetchError `InnerJoin` pooolHash `InnerJoin` poolMetadataRef) -> do
            on (poolOfflineFetchError ^. PoolOfflineFetchErrorPoolId ==. pooolHash ^. PoolHashId)
            on (poolOfflineFetchError ^. PoolOfflineFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)
            where_ (pooolHash ^. PoolHashHashRaw ==. val poolHash)
            orderBy [desc (poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime)]
            limit 10
            pure (poolOfflineFetchError, poolMetadataRef ^. PoolMetadataRefHash)
    pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)

queryPoolOfflineFetchError poolHash (Just fromTime) = do
    res <- select . from $ \(poolOfflineFetchError `InnerJoin` pooolHash `InnerJoin` poolMetadataRef) -> do
            on (poolOfflineFetchError ^. PoolOfflineFetchErrorPoolId ==. pooolHash ^. PoolHashId)
            on (poolOfflineFetchError ^. PoolOfflineFetchErrorPmrId ==. poolMetadataRef ^. PoolMetadataRefId)
            where_ (pooolHash ^. PoolHashHashRaw ==. val poolHash
              &&. poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime >=. val fromTime)
            orderBy [desc (poolOfflineFetchError ^. PoolOfflineFetchErrorFetchTime)]
            limit 10
            pure (poolOfflineFetchError, poolMetadataRef ^. PoolMetadataRefHash)
    pure $ fmap extract res
  where
    extract (fetchErr, metadataHash) = (entityVal fetchErr, unValue metadataHash)

existsDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
existsDelistedPool ph = do
  res <- select  . from $ \delistedPool -> do
            where_ (delistedPool ^. DelistedPoolHashRaw ==. val ph)
            limit 1
            pure (delistedPool ^. DelistedPoolId)
  pure $ not (null res)


existsPoolHash :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
existsPoolHash hsh = do
  res <- select . from $ \ poolHash -> do
            where_ (poolHash  ^. PoolHashHashRaw ==. val hsh)
            limit 1
            pure (poolHash ^. PoolHashId)
  pure $ not (null res)

existsPoolHashId :: MonadIO m => PoolHashId -> ReaderT SqlBackend m Bool
existsPoolHashId phid = do
  res <- select . from $ \ poolHash -> do
            where_ (poolHash  ^. PoolHashId ==. val phid)
            limit 1
            pure (poolHash ^. PoolHashId)
  pure $ not (null res)

existsPoolMetadataRefId :: MonadIO m => PoolMetadataRefId -> ReaderT SqlBackend m Bool
existsPoolMetadataRefId pmrid = do
  res <- select . from $ \ pmr -> do
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
  exists . from $ \ txIn ->
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- A predicate that filters out unspent 'TxOut' entries.
{-# INLINABLE txOutSpentP #-}
txOutSpentP :: SqlExpr (Entity TxOut) -> SqlQuery ()
txOutSpentP txOut =
  where_ . exists . from $ \ txIn -> do
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- A predicate that filters out spent 'TxOut' entries.
{-# INLINABLE txOutUnspentP #-}
txOutUnspentP :: SqlExpr (Entity TxOut) -> SqlQuery ()
txOutUnspentP txOut =
  where_ . notExists . from $ \ txIn -> do
    where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
            &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
            )

-- every tx made before or at the snapshot time
txLessEqual :: BlockId -> SqlExpr (ValueList TxId)
txLessEqual blkid =
    subList_select . from $ \tx -> do
      where_ $ tx ^. TxBlockId `in_` blockLessEqual
      pure $ tx ^. TxId
  where
    -- every block made before or at the snapshot time
    blockLessEqual :: SqlExpr (ValueList BlockId)
    blockLessEqual = subList_select . from $ \blk -> do
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
