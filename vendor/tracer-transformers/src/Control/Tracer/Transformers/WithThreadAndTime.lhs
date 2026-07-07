\label{code:Control.Tracer.Transformers.WithThreadAndTime}

%if style == newcode
\begin{code}
{-|
Module: WithThreadAndTime

Observing events with annotations of thread id and time.
-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Control.Tracer.Transformers.WithThreadAndTime
    (
    -- * transformer
      WithThreadAndTime (..)
    , threadAndTimeTracer
    ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Time.Clock.System (SystemTime, getSystemTime)

import           Control.Tracer (Tracer, mkTracer, traceWith)

\end{code}
%endif

\begin{code}
-- | Add some operational context, time and thread
data WithThreadAndTime a
  = WithThreadAndTime { occurredAt   :: !SystemTime
                      , withinThread :: !ThreadId
                      , event        :: !a
                      }
-- ^ note that this could, for example, be an instance of 'ToJSON' or
-- 'Generic' or similar to project it into a more general framework

instance (Show a) => Show (WithThreadAndTime a) where
  show (WithThreadAndTime {occurredAt, withinThread, event})
    = show occurredAt ++ "[" ++ show withinThread ++ "]: " ++ show event

-- | Add the time and thread to a trace observation
threadAndTimeTracer :: (MonadIO m) => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = mkTracer $ \s -> do
   !now <- liftIO getSystemTime
   !tid <- liftIO myThreadId
   traceWith tr $ WithThreadAndTime now tid s

\end{code}
