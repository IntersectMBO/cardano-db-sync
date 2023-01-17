{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Error (
  SyncInvariant (..),
  SyncNodeError (..),
  annotateInvariantTx,
  bsBase16Encode,
  dbSyncNodeError,
  dbSyncInvariant,
  renderSyncInvariant,
  renderSyncNodeError,
) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.DbSync.Util
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data SyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Byron.Tx !Word64 !Word64

data SyncNodeError
  = NEError !Text
  | NEInvariant !Text !SyncInvariant
  | NEBlockMismatch !Word64 !ByteString !ByteString
  | NEIgnoreShelleyInitiation
  | NEByronConfig !FilePath !Byron.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NEAlonzoConfig !FilePath !Text
  | NECardanoConfig !Text

annotateInvariantTx :: Byron.Tx -> SyncInvariant -> SyncInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

dbSyncNodeError :: Monad m => Text -> ExceptT SyncNodeError m a
dbSyncNodeError = left . NEError

dbSyncInvariant :: Monad m => Text -> SyncInvariant -> ExceptT SyncNodeError m a
dbSyncInvariant loc = left . NEInvariant loc

renderSyncInvariant :: SyncInvariant -> Text
renderSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat ["input value ", textShow inval, " < output value ", textShow outval]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx "
        , bsBase16Encode (Byron.unTxHash $ Crypto.serializeCborHash tx)
        , " : input value "
        , textShow inval
        , " < output value "
        , textShow outval
        , "\n"
        , textShow tx
        ]

renderSyncNodeError :: SyncNodeError -> Text
renderSyncNodeError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEInvariant loc i -> mconcat [loc, ": " <> renderSyncInvariant i]
    NEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number "
        , textShow blkNo
        , ", db has "
        , bsBase16Encode hashDb
        , " but chain provided "
        , bsBase16Encode hashBlk
        ]
    NEIgnoreShelleyInitiation ->
      mconcat
        [ "Node configs that don't fork to Shelley directly and initiate"
        , " funds or stakes in Shelley Genesis are not supported."
        ]
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file "
        , textShow fp
        , ": "
        , textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file "
        , textShow fp
        , ": "
        , txt
        ]
    NEAlonzoConfig fp txt ->
      mconcat
        [ "Failed reading Alonzo genesis file "
        , textShow fp
        , ": "
        , txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   "
        , err
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt
