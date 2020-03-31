{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Error
  ( DbSyncInvariant (..)
  , DbSyncNodeError (..)
  , annotateInvariantTx
  , bsBase16Encode
  , dbSyncNodeError
  , dbSyncInvariant
  , liftLookupFail
  , renderDbSyncInvariant
  , renderDbSyncNodeError
  ) where

import qualified Cardano.Chain.UTxO as Ledger
import qualified Cardano.Crypto as Crypto

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Db (LookupFail (..), renderLookupFail)
import           Cardano.DbSync.Util

data DbSyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Ledger.Tx !Word64 !Word64

data DbSyncNodeError
  = ENELookup !Text !LookupFail
  | ENEError !Text
  | ENEInvariant !Text !DbSyncInvariant
  | ENEBlockMismatch !Word64 !ByteString !ByteString

annotateInvariantTx :: Ledger.Tx -> DbSyncInvariant -> DbSyncInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

dbSyncNodeError :: Monad m => Text -> ExceptT DbSyncNodeError m a
dbSyncNodeError = left . ENEError

dbSyncInvariant :: Monad m => Text -> DbSyncInvariant -> ExceptT DbSyncNodeError m a
dbSyncInvariant loc = left . ENEInvariant loc

liftLookupFail :: Monad m => Text -> m (Either LookupFail a) -> ExceptT DbSyncNodeError m a
liftLookupFail loc =
  firstExceptT (ENELookup loc) . newExceptT

renderDbSyncInvariant :: DbSyncInvariant -> Text
renderDbSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (unTxHash $ Crypto.hash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

renderDbSyncNodeError :: DbSyncNodeError -> Text
renderDbSyncNodeError ede =
  case ede of
    ENELookup loc lf -> mconcat [ "DB lookup fail in ", loc, ": ", renderLookupFail lf ]
    ENEError t -> "Error: " <> t
    ENEInvariant loc i -> mconcat [ loc, ": " <> renderDbSyncInvariant i ]
    ENEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt
