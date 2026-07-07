{-|
Module: Control.Tracer.Legacy

Compatibility shims for symbols that existed in contra-tracer < 0.2
(from the iohk-monitoring-framework repository) but were dropped in the
contra-tracer 0.2 rewrite. Vendored here so that iohk-monitoring can
build against contra-tracer >= 0.2.1.
-}
module Control.Tracer.Legacy
    ( showTracing
    , condTracing
    , condTracingM
    ) where

import           Control.Monad (when)
import           Control.Tracer (Tracer, contramap, mkTracer, traceWith)

-- | Transform a traced value to a showable instance.
showTracing :: (Show a, Monad m) => Tracer m String -> Tracer m a
showTracing = contramap show

-- | Conditional tracing, the condition being defined at trace creation time.
condTracing :: Monad m => (a -> Bool) -> Tracer m a -> Tracer m a
condTracing active tr = mkTracer $ \s ->
    when (active s) (traceWith tr s)

-- | Conditional tracing, the condition being evaluated at trace time.
condTracingM :: Monad m => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = mkTracer $ \s -> do
    active <- activeP
    when (active s) (traceWith tr s)
