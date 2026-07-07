{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Tracer.Transformers
  ( Counting(..)
  , Folding(..)
  , counting
  , fanning
  , folding
  ) where

import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IORef (IORef, atomicModifyIORef', newIORef)

import           Control.Tracer

-- | A pure tracer combinator that allows to decide a further tracer to use,
--   based on the message being processed.
fanning
  :: forall m a
  . Monad m
  => (a -> Tracer m a) -> Tracer m a
fanning fan = mkTracer $ \x -> traceWith (fan x) x

newtype Counting a = Counting Int

-- | A stateful tracer transformer that substitutes messages with
--   a monotonically incrementing occurence count.
counting
  :: forall m a . (MonadIO m)
  => Tracer m (Counting a) -> m (Tracer m a)
counting tr =
  mkCountingTracer <$> liftIO (newIORef 0)
 where
    mkCountingTracer :: IORef Int -> Tracer m a
    mkCountingTracer ctrref = mkTracer $ \_ -> do
      ctr <- liftIO $ atomicModifyIORef' ctrref $ \n -> join (,) (n + 1)
      traceWith tr (Counting ctr)

newtype Folding a f = Folding f

-- | A generalised trace transformer that provides evolving state,
--   defined as a strict left fold.
folding
  :: forall m f a
  .  (MonadIO m)
  => (f -> a -> f) -> f -> Tracer m (Folding a f) -> m (Tracer m a)
folding cata initial tr =
  mkFoldingTracer <$> liftIO (newIORef initial)
 where
    mkFoldingTracer :: IORef f -> Tracer m a
    mkFoldingTracer ref = mkTracer $ \a -> do
      x' <- liftIO $ atomicModifyIORef' ref $ \x -> join (,) (cata x a)
      traceWith tr (Folding x')
