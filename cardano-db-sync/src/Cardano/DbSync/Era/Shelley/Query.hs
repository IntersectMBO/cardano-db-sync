{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Query (
  resolveStakeAddress,
  resolveInputTxOutId,
  resolveInputValue,
  resolveInputTxOutIdValue,
  queryResolveInputCredentials,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTxOutVariantType)
import Cardano.DbSync.Api.Types (SyncEnv)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.Prelude hiding (Ptr, from, maybeToEither, on)

resolveStakeAddress :: MonadIO m => ByteString -> DB.DbAction m (Maybe DB.StakeAddressId)
resolveStakeAddress = DB.queryStakeAddress

resolveInputTxOutId :: MonadIO m => SyncEnv -> Generic.TxIn -> DB.DbAction m (DB.TxId, DB.TxOutIdW)
resolveInputTxOutId syncEnv txIn =
  DB.queryTxOutId (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputValue :: MonadIO m => Generic.TxIn -> DB.DbAction m (DB.TxId, DB.DbLovelace)
resolveInputValue txIn =
  DB.queryTxOutValue (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputTxOutIdValue :: MonadIO m => SyncEnv -> Generic.TxIn -> DB.DbAction m (DB.TxId, DB.TxOutIdW, DB.DbLovelace)
resolveInputTxOutIdValue syncEnv txIn =
  DB.queryTxOutIdValue (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials :: MonadIO m => SyncEnv -> Generic.TxIn -> DB.DbAction m (Maybe ByteString)
queryResolveInputCredentials syncEnv txIn = do
  DB.queryTxOutCredentials (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))
