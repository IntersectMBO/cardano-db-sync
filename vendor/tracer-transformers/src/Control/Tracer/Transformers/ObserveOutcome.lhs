\label{code:Control.Tracer.Transformers.ObserveOutcome}

%if style == newcode
\begin{code}
{-|
Module: ObserveOutcome

Observing events with annotations of thread id and time.
-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Control.Tracer.Transformers.ObserveOutcome
    (
    -- * transformer
      Outcome (..)
    , OutcomeEnhancedTracer
    , OutcomeFidelity (..)
    , OutcomeProgressionStatus (..)
    , mkOutcomeExtractor
    ) where


import           Control.Monad.IO.Class (MonadIO (..))

import           Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

import           Control.Exception.Safe (MonadMask)
import qualified Control.Exception.Safe as CES

import           Control.Tracer (Tracer, mkTracer, traceWith)

\end{code}
%endif

\begin{code}
-- transformer of traces that have the structure of an 'Outcome'
-- (beginning and possible end)

-- the distinct stages
data OutcomeProgressionStatus
  = OutcomeStarts
  | OutcomeOther
  | OutcomeEnds
  deriving (Eq)

-- constructing an outcome from a sequence of observables
class (Monad m) => Outcome m a where
  type IntermediateValue a
  type OutcomeMetric a

  classifyObservable     :: a
                         -> m OutcomeProgressionStatus
  captureObservableValue :: a
                         -> m (IntermediateValue a)
  computeOutcomeMetric   :: a
                         -> IntermediateValue a
                         -> IntermediateValue a
                         -> m (OutcomeMetric a)

-- | The Maybe (OutcomeMetric a) captures the 'DeltaQ-ness' of the
--   nature of outcomes, may / may not complete.
type OutcomeEnhancedTracer m a
  = Tracer m (Either a (OutcomeFidelity (OutcomeMetric a)))

-- | Also need to know that observables happened in the "right way"

data OutcomeFidelity a
  = EndsBeforeStarted
  | StartsBeforeEnds a
  | ProgressedNormally a
--  might have "timeout" and/or Failureprogression?
  deriving (Show)

-- | Custom function for @MVar@, relying on two pretty standard
-- constraints.
liftedModifyMVar_ :: (MonadIO m, MonadMask m) => MVar a -> (a -> m a) -> m ()
liftedModifyMVar_ m io =
  CES.mask $ \restore -> do
    a  <- liftIO $ takeMVar m
    a' <- restore (io a) `CES.onException` (liftIO $ putMVar m a)
    liftIO $ putMVar m a'

-- | Generic Trace transformer. It could be written to take
--   an initial argument, but restricting the scope of that
--   per-invocation state seems more appropriate (for the
--   moment). That may be of use if\/when explict management of
--   timeout was required and\/or non-termination of the outcome at
--   the end of a run was of interest.
mkOutcomeExtractor
    :: forall m a. (MonadIO m, MonadMask m, Outcome m a)
    => m (OutcomeEnhancedTracer m a -> Tracer m a)
mkOutcomeExtractor = do
    -- We always instantiate a new MVar for the outcome.
    maybeInterValue <- liftIO $ newMVar Nothing
    pure $ traceOutcomes maybeInterValue
  where
    traceOutcomes
        :: MVar (Maybe (IntermediateValue a))
        -> OutcomeEnhancedTracer m a
        -> Tracer m a
    traceOutcomes maybeInterValue tr = mkTracer $ \a -> do
      classifedObservable <- classifyObservable a
      case classifedObservable of
        OutcomeOther    -> traceWith tr $ Left a
        outcome         -> liftedModifyMVar_ maybeInterValue $ \observedResult -> case observedResult of
                Nothing   -> outcomeWithoutValue outcome a
                (Just b)  -> outcomeWithValue outcome a b
      pure ()
       where
        -- If we don't have any intermediate values and the outcome is
        -- @OutcomeStarts@, then we set the initial value inside the MVar.
        outcomeWithoutValue :: OutcomeProgressionStatus -> a -> m (Maybe (IntermediateValue a))
        outcomeWithoutValue OutcomeStarts a = do
            !z <- captureObservableValue a
            traceWith tr $ Left a
            return $ Just z
        outcomeWithoutValue _otherwise a = do -- Outcome ends
            traceWith tr $ Left a
            traceWith tr $ Right EndsBeforeStarted
            -- We remove what we had been measuring, this should
            -- probably be an error as well.
            return $ Nothing

        -- If we do have any intermediate values and the outcome is
        -- @OutcomeStarts@, then we set the initial value inside the MVar.
        outcomeWithValue :: OutcomeProgressionStatus -> a -> IntermediateValue a -> m (Maybe (IntermediateValue a))
        outcomeWithValue OutcomeEnds a b = do
            !z <- captureObservableValue a
            traceWith tr $ Left a
            v <- computeOutcomeMetric a b z
            traceWith tr $ Right (ProgressedNormally v)
            return $ Nothing
        outcomeWithValue _otherwise a b = do -- OutcomeStarts, this could be ignored since it "resets".
            !z <- captureObservableValue a
            traceWith tr $ Left a
            v <- computeOutcomeMetric a b z
            traceWith tr $ Right (StartsBeforeEnds v) -- Probably some error.
            return $ Just z

\end{code}
