{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Query (
  resolveStakeAddress,
  resolveInputTxId,
  resolveInputTxOutId,
  resolveInputValue,
  resolveInputTxOutIdValue,
  queryResolveInputCredentials,
) where

import Cardano.Db
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Util
import Cardano.Prelude hiding (Ptr, from, maybeToEither, on)
import Database.Esqueleto.Experimental (
  SqlBackend,
 )

{- HLINT ignore "Fuse on/on" -}

resolveStakeAddress :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail StakeAddressId)
resolveStakeAddress addr = queryStakeAddress addr renderByteArray

resolveInputTxOutId :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, TxOutId))
resolveInputTxOutId txIn =
  queryTxOutId (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputValue :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
resolveInputValue txIn =
  queryTxOutValue (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))

resolveInputTxOutIdValue :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, TxOutId, DbLovelace))
resolveInputTxOutIdValue txIn =
  queryTxOutIdValue (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryResolveInputCredentials txIn = do
  queryTxOutCredentials (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))
