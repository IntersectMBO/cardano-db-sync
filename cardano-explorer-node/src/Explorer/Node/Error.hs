{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Error
  ( ExplorerNodeError (..)
  , explorerError
  , liftLookupFail
  , renderExplorerNodeError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import           Data.Text (Text)

import           Explorer.DB (LookupFail (..), renderLookupFail)

data ExplorerNodeError
  = ENELookup !Text !LookupFail
  | ENEError !Text
  | ENEInvariant !Text


explorerError :: Monad m => Text -> ExceptT ExplorerNodeError m a
explorerError = left . ENEError

liftLookupFail :: Monad m => Text -> m (Either LookupFail a) -> ExceptT ExplorerNodeError m a
liftLookupFail loc =
  firstExceptT (ENELookup loc) . newExceptT

renderExplorerNodeError :: ExplorerNodeError -> Text
renderExplorerNodeError ede =
  case ede of
    ENELookup loc lf -> mconcat [ loc, ": ", renderLookupFail lf ]
    ENEError t -> "Error: " <> t
    ENEInvariant t -> "Invariant: " <> t

