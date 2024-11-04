{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Operations.TxOut.TxOutQuery where

import Cardano.Db.Error (LookupFail (..))
import Cardano.Db.Operations.QueryHelper (isJust, maybeToEither, txLessEqual, unValue2, unValue3, unValueSumAda)
import Cardano.Db.Operations.Types (TxOutFields (..), TxOutIdW (..), TxOutTableType (..), TxOutW (..), UtxoQueryResult (..))
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Db.Types (Ada, DbLovelace (..))
import Cardano.Prelude (Bifunctor (second), ByteString, ReaderT, Text, Word64, listToMaybe, mapMaybe)
import Control.Monad.IO.Class (MonadIO)
import Database.Esqueleto.Experimental (
  Entity (..),
  SqlBackend,
  SqlExpr,
  SqlQuery,
  Value (..),
  countRows,
  from,
  in_,
  innerJoin,
  isNothing,
  just,
  leftJoin,
  notExists,
  on,
  select,
  sum_,
  table,
  val,
  where_,
  (&&.),
  (==.),
  (>.),
  (?.),
  (^.),
  (||.),
  type (:&) ((:&)),
 )

{- HLINT ignore "Fuse on/on" -}
{- HLINT ignore "Redundant ^." -}

-- Some Queries can accept TxOutTableType as a parameter, whilst others that return a TxOut related value can't
-- as they wiil either deal with Core or Variant TxOut/Address types.
-- These types also need to be handled at the call site.

--------------------------------------------------------------------------------
-- queryTxOutValue
--------------------------------------------------------------------------------

-- | Like 'queryTxId' but also return the 'TxOutIdValue' of the transaction output.
queryTxOutValue ::
  MonadIO m =>
  TxOutTableType ->
  (ByteString, Word64) ->
  ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
queryTxOutValue txOutTableType hashIndex =
  case txOutTableType of
    TxOutCore -> queryTxOutValue' @'TxOutCore hashIndex
    TxOutVariantAddress -> queryTxOutValue' @'TxOutVariantAddress hashIndex
  where
    queryTxOutValue' ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      (ByteString, Word64) ->
      ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
    queryTxOutValue' (hash, index) = do
      res <- select $ do
        (tx :& txOut) <-
          from
            $ table @Tx
              `innerJoin` table @(TxOutTable a)
            `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. txOutTxIdField @a)
        where_ (txOut ^. txOutIndexField @a ==. val index &&. tx ^. TxHash ==. val hash)
        pure (txOut ^. txOutTxIdField @a, txOut ^. txOutValueField @a)
      pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

--------------------------------------------------------------------------------
-- queryTxOutId
--------------------------------------------------------------------------------

-- | Like 'queryTxId' but also return the 'TxOutId' of the transaction output.
queryTxOutId ::
  MonadIO m =>
  TxOutTableType ->
  (ByteString, Word64) ->
  ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdW))
queryTxOutId txOutTableType hashIndex =
  case txOutTableType of
    TxOutCore -> wrapTxOutId CTxOutIdW (queryTxOutId' @'TxOutCore hashIndex)
    TxOutVariantAddress -> wrapTxOutId VTxOutIdW (queryTxOutId' @'TxOutVariantAddress hashIndex)
  where
    wrapTxOutId constructor = fmap (fmap (second constructor))

    queryTxOutId' ::
      forall a m.
      (TxOutFields a, MonadIO m) =>
      (ByteString, Word64) ->
      ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdFor a))
    queryTxOutId' (hash, index) = do
      res <- select $ do
        (tx :& txOut) <-
          from
            $ table @Tx
              `innerJoin` table @(TxOutTable a)
            `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. txOutTxIdField @a)
        where_ (txOut ^. txOutIndexField @a ==. val index &&. tx ^. TxHash ==. val hash)
        pure (txOut ^. txOutTxIdField @a, txOut ^. txOutIdField @a)
      pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

--------------------------------------------------------------------------------
-- queryTxOutIdValue
--------------------------------------------------------------------------------

-- | Like 'queryTxOutId' but also return the 'TxOutIdValue'
queryTxOutIdValue ::
  (MonadIO m) =>
  TxOutTableType ->
  (ByteString, Word64) ->
  ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdW, DbLovelace))
queryTxOutIdValue getTxOutTableType hashIndex = do
  case getTxOutTableType of
    TxOutCore -> wrapTxOutId CTxOutIdW (queryTxOutIdValue' @'TxOutCore hashIndex)
    TxOutVariantAddress -> wrapTxOutId VTxOutIdW (queryTxOutIdValue' @'TxOutVariantAddress hashIndex)
  where
    wrapTxOutId constructor =
      fmap (fmap (\(txId, txOutId, lovelace) -> (txId, constructor txOutId, lovelace)))

    queryTxOutIdValue' ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      (ByteString, Word64) ->
      ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdFor a, DbLovelace))
    queryTxOutIdValue' (hash, index) = do
      res <- select $ do
        (tx :& txOut) <-
          from
            $ table @Tx
              `innerJoin` table @(TxOutTable a)
            `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. txOutTxIdField @a)
        where_ (txOut ^. txOutIndexField @a ==. val index &&. tx ^. TxHash ==. val hash)
        pure (txOut ^. txOutTxIdField @a, txOut ^. txOutIdField @a, txOut ^. txOutValueField @a)
      pure $ maybeToEither (DbLookupTxHash hash) unValue3 (listToMaybe res)

--------------------------------------------------------------------------------
-- queryTxOutIdValue
--------------------------------------------------------------------------------

-- | Give a (tx hash, index) pair, return the TxOut Credentials.
queryTxOutCredentials ::
  MonadIO m =>
  TxOutTableType ->
  (ByteString, Word64) ->
  ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryTxOutCredentials txOutTableType (hash, index) =
  case txOutTableType of
    TxOutCore -> queryTxOutCredentialsCore (hash, index)
    TxOutVariantAddress -> queryTxOutCredentialsVariant (hash, index)

queryTxOutCredentialsCore :: MonadIO m => (ByteString, Word64) -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryTxOutCredentialsCore (hash, index) = do
  res <- select $ do
    (tx :& txOut) <-
      from
        $ table @Tx
          `innerJoin` table @C.TxOut
        `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. C.TxOutTxId)
    where_ (txOut ^. C.TxOutIndex ==. val index &&. tx ^. TxHash ==. val hash)
    pure (txOut ^. C.TxOutPaymentCred, txOut ^. C.TxOutAddressHasScript)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

queryTxOutCredentialsVariant :: MonadIO m => (ByteString, Word64) -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryTxOutCredentialsVariant (hash, index) = do
  res <- select $ do
    (tx :& txOut :& address) <-
      from
        $ ( table @Tx
              `innerJoin` table @V.TxOut
              `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. V.TxOutTxId)
          )
          `innerJoin` table @V.Address
        `on` (\((_ :& txOut) :& address) -> txOut ^. V.TxOutAddressId ==. address ^. V.AddressId)
    where_ (txOut ^. V.TxOutIndex ==. val index &&. tx ^. TxHash ==. val hash)
    pure (address ^. V.AddressPaymentCred, address ^. V.AddressHasScript)
  pure $ maybeToEither (DbLookupTxHash hash) unValue2 (listToMaybe res)

--------------------------------------------------------------------------------
-- ADDRESS QUERIES
--------------------------------------------------------------------------------
queryAddressId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe V.AddressId)
queryAddressId addrRaw = do
  res <- select $ do
    addr <- from $ table @V.Address
    where_ (addr ^. V.AddressRaw ==. val addrRaw)
    pure (addr ^. V.AddressId)
  pure $ unValue <$> listToMaybe res

--------------------------------------------------------------------------------
-- queryTotalSupply
--------------------------------------------------------------------------------

-- | Get the current total supply of Lovelace. This only returns the on-chain supply which
-- does not include staking rewards that have not yet been withdrawn. Before wihdrawal
-- rewards are part of the ledger state and hence not on chain.
queryTotalSupply ::
  (MonadIO m) =>
  TxOutTableType ->
  ReaderT SqlBackend m Ada
queryTotalSupply txOutTableType =
  case txOutTableType of
    TxOutCore -> query @'TxOutCore
    TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Ada
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        txOutUnspentP @a txOut
        pure $ sum_ (txOut ^. txOutValueField @a)
      pure $ unValueSumAda (listToMaybe res)

--------------------------------------------------------------------------------
-- queryGenesisSupply
--------------------------------------------------------------------------------

-- | Return the total Genesis coin supply.
queryGenesisSupply ::
  (MonadIO m) =>
  TxOutTableType ->
  ReaderT SqlBackend m Ada
queryGenesisSupply txOutTableType =
  case txOutTableType of
    TxOutCore -> query @'TxOutCore
    TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Ada
    query = do
      res <- select $ do
        (_tx :& txOut :& blk) <-
          from
            $ table @Tx
              `innerJoin` table @(TxOutTable a)
            `on` (\(tx :& txOut) -> tx ^. TxId ==. txOut ^. txOutTxIdField @a)
              `innerJoin` table @Block
            `on` (\(tx :& _txOut :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
        where_ (isNothing $ blk ^. BlockPreviousId)
        pure $ sum_ (txOut ^. txOutValueField @a)
      pure $ unValueSumAda (listToMaybe res)

-- A predicate that filters out spent 'TxOut' entries.
{-# INLINEABLE txOutUnspentP #-}
txOutUnspentP :: forall a. TxOutFields a => SqlExpr (Entity (TxOutTable a)) -> SqlQuery ()
txOutUnspentP txOut =
  where_ . notExists $
    from (table @TxIn) >>= \txIn ->
      where_
        ( txOut
            ^. txOutTxIdField @a
            ==. txIn
              ^. TxInTxOutId
            &&. txOut
              ^. txOutIndexField @a
              ==. txIn
                ^. TxInTxOutIndex
        )

--------------------------------------------------------------------------------
-- queryShelleyGenesisSupply
--------------------------------------------------------------------------------

-- | Return the total Shelley Genesis coin supply. The Shelley Genesis Block
-- is the unique which has a non-null PreviousId, but has null Epoch.
queryShelleyGenesisSupply :: MonadIO m => TxOutTableType -> ReaderT SqlBackend m Ada
queryShelleyGenesisSupply txOutTableType =
  case txOutTableType of
    TxOutCore -> query @'TxOutCore
    TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Ada
    query = do
      res <- select $ do
        (txOut :& _tx :& blk) <-
          from
            $ table @(TxOutTable a)
              `innerJoin` table @Tx
            `on` (\(txOut :& tx) -> tx ^. TxId ==. txOut ^. txOutTxIdField @a)
              `innerJoin` table @Block
            `on` (\(_txOut :& tx :& blk) -> tx ^. TxBlockId ==. blk ^. BlockId)
        where_ (isJust $ blk ^. BlockPreviousId)
        where_ (isNothing $ blk ^. BlockEpochNo)
        pure $ sum_ (txOut ^. txOutValueField @a)
      pure $ unValueSumAda (listToMaybe res)

--------------------------------------------------------------------------------
-- Testing or validating. Queries below are not used in production
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- queryUtxoAtBlockNo
--------------------------------------------------------------------------------
queryUtxoAtBlockNo :: MonadIO m => TxOutTableType -> Word64 -> ReaderT SqlBackend m [UtxoQueryResult]
queryUtxoAtBlockNo txOutTableType blkNo = do
  eblkId <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo ==. just (val blkNo))
    pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId txOutTableType . unValue) (listToMaybe eblkId)

--------------------------------------------------------------------------------
-- queryUtxoAtSlotNo
--------------------------------------------------------------------------------
queryUtxoAtSlotNo :: MonadIO m => TxOutTableType -> Word64 -> ReaderT SqlBackend m [UtxoQueryResult]
queryUtxoAtSlotNo txOutTableType slotNo = do
  eblkId <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockSlotNo ==. just (val slotNo))
    pure (blk ^. BlockId)
  maybe (pure []) (queryUtxoAtBlockId txOutTableType . unValue) (listToMaybe eblkId)

--------------------------------------------------------------------------------
-- queryUtxoAtBlockId
--------------------------------------------------------------------------------
queryUtxoAtBlockId :: MonadIO m => TxOutTableType -> BlockId -> ReaderT SqlBackend m [UtxoQueryResult]
queryUtxoAtBlockId txOutTableType blkid =
  case txOutTableType of
    TxOutCore -> queryUtxoAtBlockIdCore blkid
    TxOutVariantAddress -> queryUtxoAtBlockIdVariant blkid

queryUtxoAtBlockIdCore :: MonadIO m => BlockId -> ReaderT SqlBackend m [UtxoQueryResult]
queryUtxoAtBlockIdCore blkid = do
  outputs <- select $ do
    (txout :& _txin :& _tx1 :& blk :& tx2) <-
      from
        $ table @C.TxOut
          `leftJoin` table @TxIn
        `on` ( \(txout :& txin) ->
                (just (txout ^. C.TxOutTxId) ==. txin ?. TxInTxOutId)
                  &&. (just (txout ^. C.TxOutIndex) ==. txin ?. TxInTxOutIndex)
             )
          `leftJoin` table @Tx
        `on` (\(_txout :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
          `leftJoin` table @Block
        `on` (\(_txout :& _txin :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
          `leftJoin` table @Tx
        `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. C.TxOutTxId) ==. tx2 ?. TxId)

    where_ $
      (txout ^. C.TxOutTxId `in_` txLessEqual blkid)
        &&. (isNothing (blk ?. BlockBlockNo) ||. (blk ?. BlockId >. just (val blkid)))
    pure (txout, txout ^. C.TxOutAddress, tx2 ?. TxHash)
  pure $ mapMaybe convertCore outputs

queryUtxoAtBlockIdVariant :: MonadIO m => BlockId -> ReaderT SqlBackend m [UtxoQueryResult]
queryUtxoAtBlockIdVariant blkid = do
  outputs <- select $ do
    (txout :& _txin :& _tx1 :& blk :& tx2 :& address) <-
      from
        $ table @V.TxOut
          `leftJoin` table @TxIn
        `on` ( \(txout :& txin) ->
                (just (txout ^. V.TxOutTxId) ==. txin ?. TxInTxOutId)
                  &&. (just (txout ^. V.TxOutIndex) ==. txin ?. TxInTxOutIndex)
             )
          `leftJoin` table @Tx
        `on` (\(_txout :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
          `leftJoin` table @Block
        `on` (\(_txout :& _txin :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
          `leftJoin` table @Tx
        `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. V.TxOutTxId) ==. tx2 ?. TxId)
          `innerJoin` table @V.Address
        `on` (\(txout :& _ :& _ :& _ :& _ :& address) -> txout ^. V.TxOutAddressId ==. address ^. V.AddressId)

    where_ $
      (txout ^. V.TxOutTxId `in_` txLessEqual blkid)
        &&. (isNothing (blk ?. BlockBlockNo) ||. (blk ?. BlockId >. just (val blkid)))
    pure (txout, address, tx2 ?. TxHash)
  pure $ mapMaybe convertVariant outputs

convertCore :: (Entity C.TxOut, Value Text, Value (Maybe ByteString)) -> Maybe UtxoQueryResult
convertCore (out, Value address, Value (Just hash')) =
  Just $
    UtxoQueryResult
      { utxoTxOutW = CTxOutW $ entityVal out
      , utxoAddress = address
      , utxoTxHash = hash'
      }
convertCore _ = Nothing

convertVariant :: (Entity V.TxOut, Entity V.Address, Value (Maybe ByteString)) -> Maybe UtxoQueryResult
convertVariant (out, address, Value (Just hash')) =
  Just $
    UtxoQueryResult
      { utxoTxOutW = VTxOutW (entityVal out) (Just (entityVal address))
      , utxoAddress = V.addressAddress $ entityVal address
      , utxoTxHash = hash'
      }
convertVariant _ = Nothing

--------------------------------------------------------------------------------
-- queryAddressBalanceAtSlot
--------------------------------------------------------------------------------
queryAddressBalanceAtSlot :: MonadIO m => TxOutTableType -> Text -> Word64 -> ReaderT SqlBackend m Ada
queryAddressBalanceAtSlot txOutTableType addr slotNo = do
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
      case txOutTableType of
        TxOutCore -> do
          res <- select $ do
            (txout :& _ :& _ :& blk :& _) <-
              from
                $ table @C.TxOut
                  `leftJoin` table @TxIn
                `on` (\(txout :& txin) -> just (txout ^. C.TxOutTxId) ==. txin ?. TxInTxOutId)
                  `leftJoin` table @Tx
                `on` (\(_ :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
                  `leftJoin` table @Block
                `on` (\(_ :& _ :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
                  `leftJoin` table @Tx
                `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. C.TxOutTxId) ==. tx2 ?. TxId)
            where_ $
              (txout ^. C.TxOutTxId `in_` txLessEqual blkid)
                &&. (isNothing (blk ?. BlockBlockNo) ||. (blk ?. BlockId >. just (val blkid)))
            where_ (txout ^. C.TxOutAddress ==. val addr)
            pure $ sum_ (txout ^. C.TxOutValue)
          pure $ unValueSumAda (listToMaybe res)
        TxOutVariantAddress -> do
          res <- select $ do
            (txout :& _ :& _ :& blk :& _ :& address) <-
              from
                $ table @V.TxOut
                  `leftJoin` table @TxIn
                `on` (\(txout :& txin) -> just (txout ^. V.TxOutTxId) ==. txin ?. TxInTxOutId)
                  `leftJoin` table @Tx
                `on` (\(_ :& txin :& tx1) -> txin ?. TxInTxInId ==. tx1 ?. TxId)
                  `leftJoin` table @Block
                `on` (\(_ :& _ :& tx1 :& blk) -> tx1 ?. TxBlockId ==. blk ?. BlockId)
                  `leftJoin` table @Tx
                `on` (\(txout :& _ :& _ :& _ :& tx2) -> just (txout ^. V.TxOutTxId) ==. tx2 ?. TxId)
                  `innerJoin` table @V.Address
                `on` (\(txout :& _ :& _ :& _ :& _ :& address) -> txout ^. V.TxOutAddressId ==. address ^. V.AddressId)
            where_ $
              (txout ^. V.TxOutTxId `in_` txLessEqual blkid)
                &&. (isNothing (blk ?. BlockBlockNo) ||. (blk ?. BlockId >. just (val blkid)))
            where_ (address ^. V.AddressAddress ==. val addr)
            pure $ sum_ (txout ^. V.TxOutValue)
          pure $ unValueSumAda (listToMaybe res)

--------------------------------------------------------------------------------
-- queryScriptOutputs
--------------------------------------------------------------------------------
queryScriptOutputs :: MonadIO m => TxOutTableType -> ReaderT SqlBackend m [TxOutW]
queryScriptOutputs txOutTableType =
  case txOutTableType of
    TxOutCore -> fmap (map CTxOutW) queryScriptOutputsCore
    TxOutVariantAddress -> queryScriptOutputsVariant

queryScriptOutputsCore :: MonadIO m => ReaderT SqlBackend m [C.TxOut]
queryScriptOutputsCore = do
  res <- select $ do
    tx_out <- from $ table @C.TxOut
    where_ (tx_out ^. C.TxOutAddressHasScript ==. val True)
    pure tx_out
  pure $ entityVal <$> res

queryScriptOutputsVariant :: MonadIO m => ReaderT SqlBackend m [TxOutW]
queryScriptOutputsVariant = do
  res <- select $ do
    address <- from $ table @V.Address
    tx_out <- from $ table @V.TxOut
    where_ (address ^. V.AddressHasScript ==. val True)
    where_ (tx_out ^. V.TxOutAddressId ==. address ^. V.AddressId)
    pure (tx_out, address)
  pure $ map (uncurry combineToWrapper) res
  where
    combineToWrapper :: Entity V.TxOut -> Entity V.Address -> TxOutW
    combineToWrapper txOut address =
      VTxOutW (entityVal txOut) (Just (entityVal address))

--------------------------------------------------------------------------------
-- queryAddressOutputs
--------------------------------------------------------------------------------
queryAddressOutputs :: MonadIO m => TxOutTableType -> Text -> ReaderT SqlBackend m DbLovelace
queryAddressOutputs txOutTableType addr = do
  res <- case txOutTableType of
    TxOutCore -> select $ do
      txout <- from $ table @C.TxOut
      where_ (txout ^. C.TxOutAddress ==. val addr)
      pure $ sum_ (txout ^. C.TxOutValue)
    TxOutVariantAddress -> select $ do
      address <- from $ table @V.Address
      txout <- from $ table @V.TxOut
      where_ (address ^. V.AddressAddress ==. val addr)
      where_ (txout ^. V.TxOutAddressId ==. address ^. V.AddressId)
      pure $ sum_ (txout ^. V.TxOutValue)
  pure $ convert (listToMaybe res)
  where
    convert v = case unValue <$> v of
      Just (Just x) -> x
      _otherwise -> DbLovelace 0

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Count the number of transaction outputs in the TxOut table.
queryTxOutCount ::
  MonadIO m =>
  TxOutTableType ->
  ReaderT SqlBackend m Word
queryTxOutCount txOutTableType = do
  case txOutTableType of
    TxOutCore -> query @'TxOutCore
    TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word
    query = do
      res <- select $ from (table @(TxOutTable a)) >> pure countRows
      pure $ maybe 0 unValue (listToMaybe res)

queryTxOutUnspentCount ::
  MonadIO m =>
  TxOutTableType ->
  ReaderT SqlBackend m Word64
queryTxOutUnspentCount txOutTableType =
  case txOutTableType of
    TxOutCore -> query @'TxOutCore
    TxOutVariantAddress -> query @'TxOutVariantAddress
  where
    query ::
      forall (a :: TxOutTableType) m.
      (MonadIO m, TxOutFields a) =>
      ReaderT SqlBackend m Word64
    query = do
      res <- select $ do
        txOut <- from $ table @(TxOutTable a)
        txOutUnspentP @a txOut
        pure countRows
      pure $ maybe 0 unValue (listToMaybe res)
