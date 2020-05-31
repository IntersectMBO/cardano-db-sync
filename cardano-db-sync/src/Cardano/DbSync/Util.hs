{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Util
  ( liftedLogException
  , logException
  , textShow
  , traverseMEither
  ) where

import           Cardano.Prelude hiding (catch)

import           Cardano.BM.Trace (Trace, logError)

import           Control.Exception.Lifted (SomeException, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Text as Text

-- | Run a function of type `a -> m (Either e ())` over a list and return
-- the first `e` or `()`.
traverseMEither :: Monad m => (a -> m (Either e ())) -> [a] -> m (Either e ())
traverseMEither action xs = do
  case xs of
    [] -> pure $ Right ()
    (y:ys) ->
      action y >>= either (pure . Left) (const $ traverseMEither action ys)


-- | Needed when debugging disappearing exceptions.
liftedLogException :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
liftedLogException tracer txt action =
    action `catch` logger
  where
    logger :: MonadIO m => SomeException -> m a
    logger e =
      liftIO $ do
        putStrLn $ "Caught exception: txt " ++ show e
        logError tracer $ txt <> textShow e
        throwIO e

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action =
    action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e

textShow :: Show a => a -> Text
textShow = Text.pack . show
