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
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.Prelude hiding (Ptr, from, maybeToEither, on)

resolveStakeAddress :: ByteString -> ExceptT SyncNodeError DB.DbM (Maybe DB.StakeAddressId)
resolveStakeAddress = lift . DB.queryStakeAddress

resolveInputTxOutIdValue ::
  SyncEnv ->
  Generic.TxIn ->
  ExceptT SyncNodeError DB.DbM (Either DB.DbError (DB.TxId, DB.TxOutIdW, DB.DbLovelace))
resolveInputTxOutIdValue syncEnv txIn =
  lift $ DB.queryTxOutIdValueEither (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials ::
  SyncEnv ->
  Generic.TxIn ->
  ExceptT SyncNodeError DB.DbM (Maybe ByteString)
queryResolveInputCredentials syncEnv txIn = do
  lift $ DB.queryTxOutCredentials (getTxOutVariantType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))
