{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Version.V13_0.Query (
  queryDatum,
  queryDatumPage,
  queryDatumCount,
  querydatumInfo,
  queryRedeemerData,
  queryRedeemerDataPage,
  queryRedeemerDataCount,
  queryRedeemerDataInfo,
  queryScript,
  queryScriptPage,
  queryScriptCount,
  queryScriptInfo,
  upateDatumBytes,
  upateRedeemerDataBytes,
  updateScriptBytes,
) where

import Cardano.Db.Types (ScriptType (..))
import Cardano.Db.Version.V13_0.Schema
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (
  Entity (..),
  SqlBackend,
  Value,
  asc,
  countRows,
  from,
  innerJoin,
  just,
  limit,
  offset,
  on,
  orderBy,
  select,
  table,
  unValue,
  val,
  where_,
  (==.),
  (^.),
  (||.),
  type (:&) ((:&)),
 )
import Database.Persist ((=.))
import Database.Persist.Class

{- HLINT ignore "Fuse on/on" -}

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
      from
        $ table @Block
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
      from
        $ table @Block
          `innerJoin` table @Tx
        `on` (\(blk :& tx) -> tx ^. TxBlockId ==. blk ^. BlockId)
          `innerJoin` table @RedeemerData
        `on` (\(_blk :& tx :& rdmData) -> rdmData ^. RedeemerDataTxId ==. tx ^. TxId)
          `innerJoin` table @Block
        `on` (\(blk :& _tx :& _rdmData :& prevBlk) -> blk ^. BlockPreviousId ==. just (prevBlk ^. BlockId))
    where_ (rdmData ^. RedeemerDataId ==. val rdmDataId)
    pure (prevBlock ^. BlockHash, prevBlock ^. BlockSlotNo)
  pure $ unValue2 <$> listToMaybe res

queryScriptCount :: MonadIO m => ReaderT SqlBackend m Word64
queryScriptCount = do
  xs <- select $ do
    scr <- from $ table @Script
    where_ (scr ^. ScriptType ==. val PlutusV1 ||. scr ^. ScriptType ==. val PlutusV2)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

queryScript :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe ScriptId)
queryScript hsh = do
  xs <- select $ do
    scr <- from $ table @Script
    where_ (scr ^. ScriptType ==. val PlutusV1 ||. scr ^. ScriptType ==. val PlutusV2)
    where_ (scr ^. ScriptHash ==. val hsh)
    pure (scr ^. ScriptId)
  pure $ unValue <$> listToMaybe xs

queryScriptPage :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity Script]
queryScriptPage ofs lmt =
  select $ do
    scr <- from $ table @Script
    where_ (scr ^. ScriptType ==. val PlutusV1 ||. scr ^. ScriptType ==. val PlutusV2)
    orderBy [asc (scr ^. ScriptId)]
    limit lmt
    offset ofs
    pure scr

queryScriptInfo :: MonadIO m => ScriptId -> ReaderT SqlBackend m (Maybe (ByteString, Maybe Word64))
queryScriptInfo scriptId = do
  res <- select $ do
    (_blk :& _tx :& scr :& prevBlock) <-
      from
        $ table @Block
          `innerJoin` table @Tx
        `on` (\(blk :& tx) -> tx ^. TxBlockId ==. blk ^. BlockId)
          `innerJoin` table @Script
        `on` (\(_blk :& tx :& scr) -> scr ^. ScriptTxId ==. tx ^. TxId)
          `innerJoin` table @Block
        `on` (\(blk :& _tx :& _scr :& prevBlk) -> blk ^. BlockPreviousId ==. just (prevBlk ^. BlockId))
    where_ (scr ^. ScriptType ==. val PlutusV1 ||. scr ^. ScriptType ==. val PlutusV2)
    where_ (scr ^. ScriptId ==. val scriptId)
    pure (prevBlock ^. BlockHash, prevBlock ^. BlockSlotNo)
  pure $ unValue2 <$> listToMaybe res

upateDatumBytes :: MonadIO m => DatumId -> ByteString -> ReaderT SqlBackend m ()
upateDatumBytes datumId bytes = update datumId [DatumBytes =. bytes]

upateRedeemerDataBytes :: MonadIO m => RedeemerDataId -> ByteString -> ReaderT SqlBackend m ()
upateRedeemerDataBytes rdmDataId bytes = update rdmDataId [RedeemerDataBytes =. bytes]

updateScriptBytes :: MonadIO m => ScriptId -> ByteString -> ReaderT SqlBackend m ()
updateScriptBytes scriptId bytes = update scriptId [ScriptBytes =. Just bytes]

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)
