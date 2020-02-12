{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Error
  ( ExplorerInvariant (..)
  , ExplorerNodeError (..)
  , annotateInvariantTx
  , bsBase16Encode
  , explorerError
  , explorerInvariant
  , liftLookupFail
  , renderExplorerInvariant
  , renderExplorerNodeError
  ) where

import qualified Cardano.Chain.UTxO as Ledger
import qualified Cardano.Crypto as Crypto

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Explorer.DB (LookupFail (..), renderLookupFail)
import           Explorer.Node.Util

data ExplorerInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Ledger.Tx !Word64 !Word64

data ExplorerNodeError
  = ENELookup !Text !LookupFail
  | ENEError !Text
  | ENEInvariant !Text !ExplorerInvariant
  | ENEBlockMismatch !Word64 !ByteString !ByteString
  | ENEEpochLookup Word64

annotateInvariantTx :: Ledger.Tx -> ExplorerInvariant -> ExplorerInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

explorerError :: Monad m => Text -> ExceptT ExplorerNodeError m a
explorerError = left . ENEError

explorerInvariant :: Monad m => Text -> ExplorerInvariant -> ExceptT ExplorerNodeError m a
explorerInvariant loc = left . ENEInvariant loc

liftLookupFail :: Monad m => Text -> m (Either LookupFail a) -> ExceptT ExplorerNodeError m a
liftLookupFail loc =
  firstExceptT (ENELookup loc) . newExceptT

renderExplorerInvariant :: ExplorerInvariant -> Text
renderExplorerInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (unTxHash $ Crypto.hash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

renderExplorerNodeError :: ExplorerNodeError -> Text
renderExplorerNodeError ede =
  case ede of
    ENELookup loc lf -> mconcat [ "DB lookup fail in ", loc, ": ", renderLookupFail lf ]
    ENEError t -> "Error: " <> t
    ENEInvariant loc i -> mconcat [ loc, ": " <> renderExplorerInvariant i ]
    ENEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]
    ENEEpochLookup e -> mconcat ["Unable to query epoch number ", textShow e]

-- Lifted from cardano-explorer/src/Explorer/Web/Server/Util.hs
-- Probably should be in cardano-explorer-db
bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt
