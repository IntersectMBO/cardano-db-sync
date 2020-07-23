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

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Db (LookupFail (..), renderLookupFail)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Util

data DbSyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Byron.Tx !Word64 !Word64

data DbSyncNodeError
  = NELookup !Text !LookupFail
  | NEError !Text
  | NEInvariant !Text !DbSyncInvariant
  | NEBlockMismatch !Word64 !ByteString !ByteString
  | NEByronConfig !FilePath !Byron.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NECardanoConfig !Text

annotateInvariantTx :: Byron.Tx -> DbSyncInvariant -> DbSyncInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

dbSyncNodeError :: Monad m => Text -> ExceptT DbSyncNodeError m a
dbSyncNodeError = left . NEError

dbSyncInvariant :: Monad m => Text -> DbSyncInvariant -> ExceptT DbSyncNodeError m a
dbSyncInvariant loc = left . NEInvariant loc

liftLookupFail :: Monad m => Text -> m (Either LookupFail a) -> ExceptT DbSyncNodeError m a
liftLookupFail loc =
  firstExceptT (NELookup loc) . newExceptT

renderDbSyncInvariant :: DbSyncInvariant -> Text
renderDbSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (Byron.unTxHash $ Crypto.serializeCborHash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

renderDbSyncNodeError :: DbSyncNodeError -> Text
renderDbSyncNodeError ne =
  case ne of
    NELookup loc lf -> mconcat [ "DB lookup fail in ", loc, ": ", renderLookupFail lf ]
    NEError t -> "Error: " <> t
    NEInvariant loc i -> mconcat [ loc, ": " <> renderDbSyncInvariant i ]
    NEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt
