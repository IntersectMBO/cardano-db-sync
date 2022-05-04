{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Insert.Grouped
    ( BlockGroupedData (..)
    , MissingMaTxOut (..)
    , ExtendedTxOut (..)
    , insertBlockGroupedData
    , resolveTxInputs
    , resolveScriptHash
    ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List as List

import           Cardano.BM.Trace (Trace)

import           Cardano.Db (DbLovelace (..), textShow)
import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Era.Util

import           Cardano.DbSync.Error

import           Database.Persist.Sql (SqlBackend)

-- | Group data within the same block, to insert them together in batches
--
-- important NOTE: Any queries (usually found in 'Cardano.DbSync.Era.Shelley.Query')
-- that touch these 3 tables (tx_out, tx_in, ma_tx_out) need to
-- have a fallback using this in memory structure. This is because
-- these tables are inserted in the db with a delay. 'resolveTxInputs' and
-- 'resolveScriptHash' are examples that fallback to this structure.
--
-- important NOTE: 'MaTxOut' is the only table referencing 'TxOut'. If any
-- other table references it in the future it has to be added here and delay its
-- insertion.
data BlockGroupedData = BlockGroupedData
  { groupedTxIn :: ![DB.TxIn]
  , groupedTxOut :: ![(ExtendedTxOut, [MissingMaTxOut])]
  }

-- | While we collect data, we don't have access yet to the 'TxOutId', since
-- it's inserted to the db later. So it's missing fields compared to DB.MaTxOut.
data MissingMaTxOut = MissingMaTxOut
  { mmtoIdent :: !DB.MultiAssetId
  , mmtoQuantity :: !DB.DbWord64
  }

-- | 'TxOut' with its TxHash. The hash is used to resolve inputs which
-- reference outputs that are not inserted to the db yet.
data ExtendedTxOut = ExtendedTxOut
  { etoTxHash :: !ByteString
  , etoTxOut :: !DB.TxOut
  }

instance Monoid BlockGroupedData where
  mempty = BlockGroupedData [] []

instance Semigroup BlockGroupedData where
  tgd1 <> tgd2 =
    BlockGroupedData (groupedTxIn tgd1 <> groupedTxIn tgd2)
                  (groupedTxOut tgd1 <> groupedTxOut tgd2)

insertBlockGroupedData
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> BlockGroupedData
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertBlockGroupedData _tracer grouped = do
    txOutIds <- lift . DB.insertManyTxOut $ etoTxOut. fst <$> groupedTxOut grouped
    let maTxOuts = concatMap mkmaTxOuts $ zip txOutIds (snd <$> groupedTxOut grouped)
    lift $ DB.insertManyMaTxOut maTxOuts
    lift . DB.insertManyTxIn $ groupedTxIn grouped
  where
    mkmaTxOuts :: (DB.TxOutId, [MissingMaTxOut]) -> [DB.MaTxOut]
    mkmaTxOuts (txOutId, mmtos) = mkmaTxOut txOutId <$> mmtos

    mkmaTxOut :: DB.TxOutId -> MissingMaTxOut -> DB.MaTxOut
    mkmaTxOut txOutId missingMaTx =
      DB.MaTxOut
        { DB.maTxOutIdent = mmtoIdent missingMaTx
        , DB.maTxOutQuantity = mmtoQuantity missingMaTx
        , DB.maTxOutTxOutId = txOutId
        }

-- | If we can't resolve from the db, we fall back to the provided outputs
-- This happens the input consumes an output introduced in the same block.
resolveTxInputs
  :: MonadIO m
  => [ExtendedTxOut]
  -> Generic.TxIn
  -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Generic.TxIn, DB.TxId, DbLovelace)
resolveTxInputs groupedOutputs txIn = fmap convert $ liftLookupFail ("resolveTxInputs " <> textShow txIn <> " ") $ do
    qres <- queryResolveInput txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemory txIn groupedOutputs of
          Nothing -> pure $ Left err
          Just eutxo -> pure $ Right (DB.txOutTxId (etoTxOut eutxo), DB.txOutValue (etoTxOut eutxo))
  where
    convert :: (DB.TxId, DbLovelace) -> (Generic.TxIn, DB.TxId, DbLovelace)
    convert (txId, lovelace) = (txIn, txId, lovelace)

resolveScriptHash
  :: (MonadBaseControl IO m, MonadIO m)
  => [ExtendedTxOut]
  -> Generic.TxIn
  -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
resolveScriptHash groupedOutputs txIn = liftLookupFail "resolveScriptHash" $ do
  qres <- fmap fst <$> queryResolveInputCredentials txIn
  case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemory txIn groupedOutputs of
          Nothing -> pure $ Left err
          Just eutxo -> pure $ Right $ DB.txOutPaymentCred $ etoTxOut eutxo

resolveInMemory :: Generic.TxIn -> [ExtendedTxOut] -> Maybe ExtendedTxOut
resolveInMemory txIn = List.find matches
  where
    matches :: ExtendedTxOut -> Bool
    matches eutxo =
         Generic.txInHash txIn == etoTxHash eutxo
      && Generic.txInIndex txIn == DB.txOutIndex (etoTxOut eutxo)
