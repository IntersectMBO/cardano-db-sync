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

import Cardano.Db
import Cardano.DbSync.Api (getTxOutTableType)
import Cardano.DbSync.Api.Types (SyncEnv)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Util
import Cardano.Prelude hiding (Ptr, from, maybeToEither, on)
import Database.Esqueleto.Experimental (
  SqlBackend,
 )

{- HLINT ignore "Fuse on/on" -}

resolveStakeAddress :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail StakeAddressId)
resolveStakeAddress addr = queryStakeAddress addr renderByteArray

resolveInputTxOutId :: MonadIO m => SyncEnv -> Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdW))
resolveInputTxOutId syncEnv txIn =
  queryTxOutId (getTxOutTableType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputValue :: MonadIO m => SyncEnv -> Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
resolveInputValue syncEnv txIn =
  queryTxOutValue (getTxOutTableType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputTxOutIdValue :: MonadIO m => SyncEnv -> Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, TxOutIdW, DbLovelace))
resolveInputTxOutIdValue syncEnv txIn =
  queryTxOutIdValue (getTxOutTableType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials :: MonadIO m => SyncEnv -> Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryResolveInputCredentials syncEnv txIn = do
  queryTxOutCredentials (getTxOutTableType syncEnv) (Generic.toTxHash txIn, fromIntegral (Generic.txInIndex txIn))
