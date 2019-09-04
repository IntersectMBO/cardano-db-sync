{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.DB.Query
  ( LookupFail (..)
  , listToMaybe
  , queryBlock
  , queryBlockCount
  , queryBlockId
  , queryBlockIdAndHash
  , queryBlockTxCount
  , queryGenesisSupply
  , queryLatestBlockId
  , queryLatestBlocks
  , queryLatestSlotNo
  , queryMeta
  , queryPreviousBlockId
  , querySelectCount
  , queryTotalSupply
  , queryTxCount
  , queryTxId
  , queryTxInCount
  , queryTxOutCount
  , queryTxOutValue
  , renderLookupFail
  , unValueSumAda
  , maybeToEither
  , entityPair
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Fixed (Micro)
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto (Entity (..), From, InnerJoin (..), PersistField, SqlExpr, SqlQuery, Value,
                    (^.), (==.), (&&.),
                    countRows, desc, entityKey, entityVal, from, limit, not_, notExists, nothing, on, orderBy,
                    select, sum_, unValue, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB.Error
import           Explorer.DB.Schema
import           Explorer.DB.Types

-- If you squint, these Esqueleto queries almost look like SQL queries.



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

-- | Get the 'BlockId' and 'Block' associated with the given hash.
queryBlockIdAndHash :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (BlockId, Block))
queryBlockIdAndHash hash = do
  res <- select . from $ \ blk -> do
            where_ (blk ^. BlockHash ==. val hash)
            pure $ blk
  pure $ maybeToEither (DbLookupBlockHash hash) entityPair (listToMaybe res)

-- | Get the number of transactions in the specified block.
queryBlockTxCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word64
queryBlockTxCount blkId = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxBlock ==. val blkId)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Return the total Genesis coin supply.
queryGenesisSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryGenesisSupply = do
    res <- select . from $ \ (txOut `InnerJoin` tx) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                where_ (tx ^. TxBlock ==. val (BlockKey 1))
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => ReaderT SqlBackend m (Maybe BlockId)
queryLatestBlockId = do
  res <- select $ from $ \ blk -> do
                orderBy [desc (blk ^. BlockId)]
                limit $ 1
                pure $ (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)


-- | Get the last N blocks.
-- This assumes that the block are inserted into the datebase from oldest to
-- newest which is not exlicitly enforced, but should arise automatically
-- from the way blocks are retrieved and inserted into the database.
queryLatestBlocks :: MonadIO m => Int -> ReaderT SqlBackend m [(Word64, ByteString)]
queryLatestBlocks limitCount = do
    eblks <- select $ from $ \ blk -> do
                orderBy [desc (blk ^. BlockId)]
                limit $ fromIntegral limitCount
                pure $ (blk ^. BlockSlotNo, blk ^. BlockHash)
    pure $ catMaybes (map convert eblks)
  where
    convert :: (Value (Maybe Word64), Value ByteString) -> Maybe (Word64, ByteString)
    convert (va, vb) =
      case (unValue va, unValue vb) of
        (Nothing, _ ) -> Nothing
        (Just a, b) -> Just (a, b)

queryPreviousBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe BlockId)
queryPreviousBlockId blkId = do
  res <- select $ from $ \ blk -> do
                where_ (blk ^. BlockId ==. val blkId)
                pure $ (blk ^. BlockPrevious)
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
            where_ (isJust $ blk ^. BlockSlotNo)
            orderBy [desc (blk ^. BlockId)]
            limit 1
            pure (blk ^. BlockSlotNo)
  pure $ fromMaybe 0 (listToMaybe $ mapMaybe unValue res)

queryMeta :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Meta)
queryMeta = do
  res <- select . from $ \ (meta :: SqlExpr (Entity Meta)) -> do
            pure meta
  pure $ case res of
            [] -> Left DbMetaEmpty
            [m] -> Right $ entityVal m
            _ -> Left DbMetaMultipleRows

-- | Get the current total supply of Lovelace.
queryTotalSupply :: MonadIO m => ReaderT SqlBackend m Ada
queryTotalSupply = do
    res <- select . from $ \ txOut -> do
                txOutUnspent txOut
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
queryTxOutValue :: MonadIO m => (ByteString, Word16) -> ReaderT SqlBackend m Word64
queryTxOutValue (hash, index) = do
  res <- select . from $ \ (tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            where_ (txOut ^. TxOutIndex ==. val index
                    &&. tx ^. TxHash ==. val hash
                    )
            pure $ txOut ^. TxOutValue
  pure $ maybe 0 unValue (listToMaybe res)

-- -----------------------------------------------------------------------------
-- SqlQuery predicates

-- Filter out 'Nothing' from a 'Maybe a'.
isJust :: PersistField a => SqlExpr (Value (Maybe a)) -> SqlExpr (Value Bool)
isJust x = not_ (x ==. nothing)

-- A predicate that filters out spent 'TxOut' entries.
txOutUnspent :: SqlExpr (Entity TxOut) -> SqlQuery ()
txOutUnspent txOut =
  where_ $ notExists $ from $ \ txIn -> do
      where_ (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId
              &&. txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex
              )

-- Unfortunately the 'sum_' operation above returns a 'PersistRational' so we need
-- to un-wibble it.
unValueSumAda :: Maybe (Value (Maybe Micro)) -> Ada
unValueSumAda mvm =
  case fmap unValue mvm of
    Just (Just x) -> lovelaceToAda x
    _ -> Ada 0

-- -----------------------------------------------------------------------------

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToEither :: e -> (a -> b) -> Maybe a -> Either e b
maybeToEither e f =
  maybe (Left e) (Right . f)

entityPair :: Entity a -> (Key a, a)
entityPair e =
  (entityKey e, entityVal e)
