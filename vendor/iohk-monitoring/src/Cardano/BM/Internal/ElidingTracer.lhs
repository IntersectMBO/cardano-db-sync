
\subsection{Cardano.BM.Internal.ElidingTracer}
\label{code:Cardano.BM.Internal.ElidingTracer}

This module is marked \emph{internal} as its use is risky and should only be included
if one has tested the implemented instances thoroughly.

Disclaimer: Use at your own risk. Eliding of messages can result in the irrevocable removal
of traced values.

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}

module Cardano.BM.Internal.ElidingTracer
    (
      ElidingTracer (..)
    , defaultelidedreporting
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import           Control.Monad (when)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Aggregated (Measurable(PureI))
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Trace (traceNamedObject)

\end{code}
%endif

\subsubsection{Tracer transformer for eliding messages}
\label{code:ElidingTracer}\index{ElidingTracer}

The eliding tracer transformer depends on two predicates to decide on which
observable type eliding messages is active (\ref{doelide}), and whether two messages can be
considered equivalent and thus be elided (\ref{isEquivalent}).
\begin{code}

class ElidingTracer a where
\end{code}

This predicate is |True| for message types for which eliding is enabled. Needs to be
overwritten in instances of |ElidingTracer|.
\label{code:doelide}\index{ElidingTracer!doelide}
\begin{code}
  doelide :: a -> Bool
\end{code}

The predicate to determine if two messages are |equivalent|. This needs to be
overwritten in instances of |ElidingTracer|.
\label{code:isEquivalent}\index{ElidingTracer!isEquivalent}
\begin{code}
  isEquivalent :: a -> a -> Bool
\end{code}

Create a new state |MVar|.
\label{code:newstate}\index{ElidingTracer!newstate}
\begin{code}
  newstate :: IO (MVar (Maybe a, Integer))
  default newstate :: IO (MVar (Maybe a, Integer))
  newstate = newMVar (Nothing, 0)
\end{code}

Internal state transitions.
\label{code:starteliding}\index{ElidingTracer!starteliding}
\label{code:conteliding}\index{ElidingTracer!conteliding}
\label{code:stopeliding}\index{ElidingTracer!stopeliding}
\label{code:reportelided}\index{ElidingTracer!reportelided}
\begin{code}
  starteliding :: (ToObject t, Transformable t IO a)
               => TracingVerbosity -> Trace IO t
               -> a -> IO (Maybe a, Integer)
  default starteliding :: (ToObject t, Transformable t IO a)
                       => TracingVerbosity -> Trace IO t
                       -> a -> IO (Maybe a, Integer)
  starteliding tverb tr ev = do
    traceWith (toLogObject' tverb tr) ev
    return (Just ev, 0)

  conteliding :: (ToObject t, Transformable t IO a)
              => TracingVerbosity -> Trace IO t
              -> a -> (Maybe a, Integer) -> IO (Maybe a, Integer)
  default conteliding :: Transformable t IO a
                      => TracingVerbosity -> Trace IO t
                      -> a -> (Maybe a, Integer) -> IO (Maybe a, Integer)
  conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding _tverb _tr ev (_old, count) = return (Just ev, count + 1)

  stopeliding :: (ToObject t, Transformable t IO a)
              => TracingVerbosity -> Trace IO t
              -> a -> (Maybe a, Integer) -> IO (Maybe a, Integer)
  default stopeliding :: (ToObject t, Transformable t IO a)
                      => TracingVerbosity -> Trace IO t
                      -> a -> (Maybe a, Integer) -> IO (Maybe a, Integer)
  stopeliding tverb tr ev (Nothing, _count) = do
    traceWith (toLogObject' tverb tr) ev
    return (Nothing, 0)
  stopeliding tverb tr ev (Just ev0, count) = do
    when (count > 1) $  -- report the number of elided messages
      reportelided tverb tr ev0 count
    when (count > 0) $  -- output last elided message
      traceWith (toLogObject' tverb tr) ev0
    traceWith (toLogObject' tverb tr) ev
    return (Nothing, 0)

  reportelided :: (ToObject t, Transformable t IO a)
               => TracingVerbosity -> Trace IO t
               -> a -> Integer -> IO ()
  default reportelided :: (ToObject t, Transformable t IO a)
                       => TracingVerbosity -> Trace IO t
                       -> a -> Integer -> IO ()
  reportelided = defaultelidedreporting

\end{code}

The transformer from a Tracer IO emph{a} to |Trace IO t| contains
the main logic of eliding messages.
\label{code:elideToLogObject}\index{ElidingTracer!elideToLogObject}
\begin{code}
  elideToLogObject
      :: (ToObject t, Transformable t IO a)
      => TracingVerbosity -> MVar (Maybe a, Integer)
      -> Trace IO t -> Tracer IO a
  default elideToLogObject
      :: (ToObject t, Transformable t IO a)
      => TracingVerbosity -> MVar (Maybe a, Integer)
      -> Trace IO t -> Tracer IO a
  elideToLogObject tverb mvar tr = mkTracer $ \ev ->
    modifyMVar_ mvar $ \s@(old, _count) ->
    if doelide ev
      then
        case old of
          Nothing -> starteliding tverb tr ev
          Just ev0 ->
            if ev `isEquivalent` ev0
              then
                conteliding tverb tr ev s >>= \case
                  (Nothing, _) -> stopeliding tverb tr ev s
                  newpair -> return newpair
              else
                stopeliding tverb tr ev s
      else
        stopeliding tverb tr ev s

\end{code}

\subsubsection{Tracing a summary messages after eliding}
\label{code:defaultelidedreporting}\index{defaultelidedreporting}

This is the default implementation of tracing a summary message after eliding stopped.
It simply outputs the internal counter.
In cases where this state is used for other purposes than counting messages, the
\ref{reportelided} function should be implemented in the \ref{ElidingTracer} instance.

\begin{code}
defaultelidedreporting :: (ToObject t, Transformable t IO a)
                       => TracingVerbosity -> Trace IO t
                       -> a -> Integer -> IO ()
defaultelidedreporting _tverb tr ev0 count = do
    meta <- mkLOMeta (getSeverityAnnotation ev0) (getPrivacyAnnotation ev0)
    traceNamedObject tr (meta, LogValue "before next, messages elided" (PureI $ toInteger (count - 1)))

\end{code}
