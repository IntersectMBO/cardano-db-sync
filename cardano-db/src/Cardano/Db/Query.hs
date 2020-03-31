{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Query
  ( LookupFail (..)
  , queryBlock
  , queryBlockCount
  , queryBlockHeight
  , queryBlockId
  , queryBlockNo
  , queryMainBlock
  , queryBlockTxCount
  , queryCalcEpochEntry
  , queryCheckPoints
  , queryEpochEntry
  , queryEpochNo
  , queryFeesUpToBlockNo
  , queryFeesUpToSlotNo
  , queryGenesisSupply
  , queryIsFullySynced
  , queryLatestBlock
  , queryLatestCachedEpochNo
  , queryLatestEpochNo
  , queryLatestBlockId
  , queryLatestBlockNo
  , queryLatestSlotNo
  , queryMeta
  , queryNetworkName
  , queryPreviousBlockNo
  , querySelectCount
  , querySlotPosixTime
  , querySlotUtcTime
  , queryTotalSupply
  , queryTxCount
  , queryTxId
  , queryTxInCount
  , queryTxOutCount
  , queryTxOutValue
  , queryUtxoAtBlockNo
  , queryUtxoAtSlotNo

  , entityPair
  , epochUtcTime
  , isJust
  , listToMaybe
  , maybeToEither
  , renderLookupFail
  , slotPosixTime
  , slotUtcTime
  , txOutSpentB
  , txOutSpentP
  , txOutUnspentP
  , unBlockId
  , unValue2
  , unValue3
  , unValueSumAda
  ) where


import           Control.Monad (join)
import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Fixed (Micro)
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ratio ((%), numerator)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto (Entity (..), From, InnerJoin (..), LeftOuterJoin (..),
                    PersistField, SqlExpr, SqlQuery, Value (..), ValueList,
                    (^.), (==.), (<=.), (&&.), (||.), (>.),
                    count, countRows, desc, entityKey, entityVal, from, exists,
                    in_, isNothing, just, limit, max_, min_, not_, notExists, on, orderBy,
                    select, subList_select, sum_, unValue, unSqlBackendKey, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Cardano.Db.Error
import           Cardano.Db.Schema
import           Cardano.Db.Types

-- If you squint, these Esqueleto queries almost look like SQL queries.


type ValMay a = Value (Maybe a)

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
  pure $ fromMaybe 0 (join $ unValue <$> listToMaybe res)

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
              where_ $ (isJust (blk ^. BlockBlockNo) &&. blk ^. BlockId <=. val blkid)
              orderBy [desc (blk ^. BlockId)]
              limit 1
              pure blk
      pure $ maybeToEither (DbLookupBlockId $ unBlockId blkid) entityVal (listToMaybe res)

-- | Get the number of transactions in the specified block.
queryBlockTxCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word64
queryBlockTxCount blkId = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxBlock ==. val blkId)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epcoh entry for the last epoch.
-- When following the chain, this is called for each new block of the current epoch.
queryCalcEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail Epoch)
queryCalcEpochEntry epochNum = do
    res <- select . from $ \ (tx `InnerJoin` blk) -> do
              on (tx ^. TxBlock ==. blk ^. BlockId)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure $ (sum_ (tx ^. TxOutSum), count (tx ^. TxOutSum), min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    blks <- select . from $ \ blk -> do
              where_ (isJust $ blk ^. BlockSlotNo)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure countRows
    let blkCount = maybe 0 unValue $ listToMaybe blks
    case listToMaybe res of
      Nothing -> queryEmptyEpoch blkCount
      Just x -> convert blkCount x
  where
    convert :: MonadIO m
            => Word64 -> (ValMay Rational, Value Word64, ValMay UTCTime, ValMay UTCTime)
            -> ReaderT SqlBackend m (Either LookupFail Epoch)
    convert blkCount tuple =
      case tuple of
        (Value (Just outSum), Value txCount, Value (Just start), Value (Just end)) ->
            pure (Right $ Epoch (fromIntegral $ numerator outSum) txCount blkCount epochNum start end)
        _otherwise -> queryEmptyEpoch blkCount

    queryEmptyEpoch :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail Epoch)
    queryEmptyEpoch blkCount = do
      res <- select . from $ \ blk -> do
              where_ (isJust $ blk ^. BlockSlotNo)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure (min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
      case listToMaybe res of
        Nothing -> pure $ Left (DbLookupEpochNo epochNum)
        Just x -> pure $ convert2 blkCount x

    convert2 :: Word64 -> (ValMay UTCTime, ValMay UTCTime) -> Either LookupFail Epoch
    convert2 blkCount tuple =
      case tuple of
        (Value (Just start), Value (Just end)) ->
            Right (Epoch 0 0 blkCount epochNum start end)
        _otherwise -> Left (DbLookupEpochNo epochNum)

queryCheckPoints :: MonadIO m => Word64 -> ReaderT SqlBackend m [(Word64, ByteString)]
queryCheckPoints limitCount = do
    latest <- select $ from $ \ blk -> do
                where_ $ (isJust $ blk ^. BlockSlotNo)
                orderBy [desc (blk ^. BlockId)]
                limit 1
                pure $ (blk ^. BlockSlotNo)
    case join (unValue <$> listToMaybe latest) of
      Nothing -> pure []
      Just slotNo -> mapMaybeM querySpacing (calcSpacing slotNo)
  where
    querySpacing :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe (Word64, ByteString))
    querySpacing blkNo = do
       rows <- select $ from $ \ blk -> do
                  where_ $ (blk ^. BlockSlotNo ==. just (val blkNo))
                  pure $ (blk ^. BlockSlotNo, blk ^. BlockHash)
       pure $ join (convert <$> listToMaybe rows)

    convert :: (Value (Maybe Word64), Value ByteString) -> Maybe (Word64, ByteString)
    convert (va, vb) =
      case (unValue va, unValue vb) of
        (Nothing, _ ) -> Nothing
        (Just a, b) -> Just (a, b)

    calcSpacing :: Word64 -> [Word64]
    calcSpacing end =
      if end > 2 * limitCount
        then [ end, end - end `div` limitCount .. 1 ]
        else [ end, end - 2 .. 1 ]

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

-- | Get the fees paid in all block from genesis up to and including the specified block.
queryFeesUpToBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToBlockNo blkNo = do
  res <- select . from $ \ (tx `InnerJoin` blk) -> do
            on (tx ^. TxBlock ==. blk ^. BlockId)
            where_ (isJust $ blk ^. BlockBlockNo)
            where_ (blk ^. BlockBlockNo <=. just (val blkNo))
            pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

queryFeesUpToSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Ada
queryFeesUpToSlotNo slotNo = do
  res <- select . from $ \ (tx `InnerJoin` blk) -> do
            on (tx ^. TxBlock ==. blk ^. BlockId)
            where_ (isJust $ blk ^. BlockSlotNo)
            where_ (blk ^. BlockSlotNo <=. just (val slotNo))
            pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda (listToMaybe res)

-- | Return the total Genesis coin supply.
queryGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryGenesisSupply = do
    res <- select . from $ \ (txOut `InnerJoin` tx) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                where_ (tx ^. TxBlock ==. val (BlockKey 1))
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

queryIsFullySynced :: MonadIO m => ReaderT SqlBackend m Bool
queryIsFullySynced = do
  emeta <- queryMeta
  mLatestBlockTime <- fmap blockTime <$> queryLatestBlock
  case (emeta, mLatestBlockTime) of
    (Left _, _) -> pure False
    (_, Nothing) -> pure False
    (Right meta, Just latestBlockTime)  -> do
      -- This assumes that the local clock is correct.
      -- This is a a valid assumption as db-sync is connected to a locally running
      -- node and the node needs to have a correct local clock to validate transactions.
      currentTime <- liftIO getCurrentTime
      let psec = diffUTCTime currentTime latestBlockTime
      -- Slot duration is in microseconds, and the difference from current time to
      -- latest block time is in pico seconds.
      pure $ psec < fromRational (2 * fromIntegral (metaSlotDuration meta) % 1000)

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => ReaderT SqlBackend m (Maybe BlockId)
queryLatestBlockId = do
  res <- select $ from $ \ blk -> do
                orderBy [desc (blk ^. BlockId)]
                limit $ 1
                pure $ (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)

-- | Get the 'BlockNo' of the latest block.
queryLatestBlockNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestBlockNo = do
  res <- select $ from $ \ blk -> do
                where_ $ (isJust $ blk ^. BlockBlockNo)
                orderBy [desc (blk ^. BlockBlockNo)]
                limit 1
                pure $ blk ^. BlockBlockNo
  pure $ listToMaybe (catMaybes $ map unValue res)

-- | Get the latest block.
queryLatestBlock :: MonadIO m => ReaderT SqlBackend m (Maybe Block)
queryLatestBlock = do
  res <- select $ from $ \ blk -> do
                orderBy [desc (blk ^. BlockId)]
                limit 1
                pure $ blk
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
            where_ $ (isJust $ blk ^. BlockSlotNo)
            orderBy [desc (blk ^. BlockEpochNo)]
            limit 1
            pure (blk ^. BlockEpochNo)
  pure $ fromMaybe 0 (listToMaybe . catMaybes $ map unValue res)

-- | Given a 'BlockId' return the 'BlockId' of the previous block.
queryPreviousBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Word64)
queryPreviousBlockNo blkNo = do
  res <- select . from $ \ (blk `InnerJoin` pblk) -> do
                on (blk ^. BlockPrevious ==. just (pblk ^. BlockId))
                where_ (blk ^. BlockBlockNo ==. just (val blkNo))
                pure $ (pblk ^. BlockBlockNo)
  pure $ maybe Nothing unValue (listToMaybe res)

-- | Count the number of rows that match the select with the supplied predicate.
querySelectCount :: (MonadIO m, From table) => (table -> SqlQuery ()) -> ReaderT SqlBackend m Word
querySelectCount predicate = do
  xs <- select . from $ \x -> do
            predicate x
            pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

-- | Get the latest slot number
queryLatestSlotNo :: MonadIO m => ReaderT SqlBackend m Word64
queryLatestSlotNo = do
  res <- select . from $ \ blk -> do
            where_ $ (isJust $ blk ^. BlockSlotNo)
            orderBy [desc (blk ^. BlockSlotNo)]
            limit 1
            pure $ blk ^. BlockSlotNo
  pure $ fromMaybe 0 (listToMaybe . catMaybes $ map unValue res)

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

-- | Get the network name from the Meta table.
queryNetworkName :: MonadIO m => ReaderT SqlBackend m (Maybe Text)
queryNetworkName = do
  res <- select . from $ \ meta -> do
            pure (meta ^. MetaNetworkName)
  pure $ join (unValue <$> listToMaybe res)


-- | Calculate the slot time (as UTCTime) for a given slot number. The example here was
-- written as an example, but it would be hoped that this value would be cached in the
-- application or calculated in a VIEW.
querySlotPosixTime :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail POSIXTime)
querySlotPosixTime slotNo =
  runExceptT $ do
    meta <- ExceptT queryMeta
    pure $ slotPosixTime meta slotNo

-- | Calculate the slot time (as UTCTime) for a given slot number. The example here was
-- written as an example, but it would be hoped that this value would be cached in the
-- application or calculated in a VIEW.
querySlotUtcTime :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either LookupFail UTCTime)
querySlotUtcTime slotNo =
  runExceptT $ do
    meta <- ExceptT queryMeta
    pure $ slotUtcTime meta slotNo

-- | Get the current total supply of Lovelace.
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
-- It can return 0 if the output does not exist.
queryTxOutValue :: MonadIO m => (ByteString, Word16) -> ReaderT SqlBackend m (Either LookupFail Word64)
queryTxOutValue (hash, index) = do
  res <- select . from $ \ (tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            where_ (txOut ^. TxOutIndex ==. val index
                    &&. tx ^. TxHash ==. val hash
                    )
            pure $ txOut ^. TxOutValue
  pure $ maybeToEither (DbLookupTxHash hash) unValue (listToMaybe res)

-- | Get the UTxO set after the specified 'BlockId' has been applied to the chain.
-- Not exported because 'BlockId' to 'BlockHash' relationship may not be the same
-- across machines.
queryUtxoAtBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtBlockId blkid = do
    -- tx1 refers to the tx of the input spending this output (if it is ever spent)
    -- tx2 refers to the tx of the output
    outputs <- select . from $ \(txout `LeftOuterJoin` txin `LeftOuterJoin` tx1 `LeftOuterJoin` blk `LeftOuterJoin` tx2) -> do
                  on $ txout ^. TxOutTxId ==. tx2 ^. TxId
                  on $ tx1 ^. TxBlock ==. blk ^. BlockId
                  on $ txin ^. TxInTxInId ==. tx1 ^. TxId
                  on $ (txout ^. TxOutTxId ==. txin ^. TxInTxOutId) &&. (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
                  where_ $ (txout ^. TxOutTxId `in_` txLessEqual blkid) &&. ((isNothing $ blk ^. BlockBlockNo) ||. (blk ^. BlockId >. val blkid))
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
  maybe (pure []) queryUtxoAtBlockId $ fmap unValue (listToMaybe eblkId)

queryUtxoAtSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoAtSlotNo slotNo = do
  eblkId <- select . from $ \blk -> do
                where_ (blk ^. BlockSlotNo ==. just (val slotNo))
                pure (blk ^. BlockId)
  maybe (pure []) queryUtxoAtBlockId $ fmap unValue (listToMaybe eblkId)

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
      where_ $ tx ^. TxBlock `in_` blockLessEqual
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

epochUtcTime :: Meta -> Word64 -> UTCTime
epochUtcTime meta epochNum =
  -- Slot duration is in milliseconds.
  addUTCTime (21.6 * fromIntegral (epochNum * metaSlotDuration meta)) (metaStartTime meta)

maybeToEither :: e -> (a -> b) -> Maybe a -> Either e b
maybeToEither e f =
  maybe (Left e) (Right . f)

slotPosixTime :: Meta -> Word64 -> POSIXTime
slotPosixTime meta =
  utcTimeToPOSIXSeconds . slotUtcTime meta

slotUtcTime :: Meta -> Word64 -> UTCTime
slotUtcTime meta slotNo =
  -- Slot duration is in milliseconds.
  addUTCTime (0.001 * fromIntegral (slotNo * metaSlotDuration meta)) (metaStartTime meta)


unBlockId :: BlockId -> Word64
unBlockId = fromIntegral . unSqlBackendKey . unBlockKey

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)

unValue3 :: (Value a, Value b, Value c) -> (a, b, c)
unValue3 (a, b, c) = (unValue a, unValue b, unValue c)
