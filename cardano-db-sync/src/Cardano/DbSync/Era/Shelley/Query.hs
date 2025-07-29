{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Query (
  resolveStakeAddress,
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

resolveInputTxOutIdValue ::
  MonadIO m =>
  SyncEnv ->
  Generic.TxIn ->
  DB.DbAction m (Either DB.DbError (DB.TxId, DB.TxOutIdW, DB.DbLovelace))
resolveInputTxOutIdValue syncEnv txIn =
  DB.queryTxOutIdValueEither (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials ::
  MonadIO m =>
  SyncEnv ->
  Generic.TxIn ->
  DB.DbAction m (Maybe ByteString)
queryResolveInputCredentials syncEnv txIn = do
  DB.queryTxOutCredentials (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))
