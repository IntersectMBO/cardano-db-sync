
\subsection{Cardano.BM.Observer.Monadic}
\label{code:Cardano.BM.Observer.Monadic}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-@ LIQUID "--prune-unsorted" @-}

module Cardano.BM.Observer.Monadic
    (
      bracketObserveIO
    , bracketObserveM
    , bracketObserveX
      -- * observing functions
    , observeOpen
    , observeClose
    ) where

import           Control.Exception.Safe (MonadCatch, SomeException, catch, throwM)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.IO (stderr)

import           Cardano.BM.Data.Counter (CounterState (..), diffCounters)
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta,
                     PrivacyAnnotation(Confidential), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.SubTrace (SubTrace (Neutral, NoTrace))
import           Cardano.BM.Trace (Trace, traceNamedObject)
\end{code}
%endif

\subsubsection{Monadic.bracketObserverIO}
Observes an |IO| action. The subtrace type is found in the configuration with
the passed-in name.
\newline
\par\noindent
Microbenchmarking steps:
\newline
\par
1. Create a |trace| which will have been configured
   to observe things besides logging.

\begin{spec}
        import qualified Cardano.BM.Configuration.Model as CM
        . . .
        c <- config
        trace <- setupTrace (Right c) "demo-playground"
            where
                config :: IO CM.Configuration
                config = do
                    c <- CM.empty
                    CM.setMinSeverity c Debug
                    CM.setSetupBackends c [KatipBK, AggregationBK]
                    CM.setDefaultBackends c [KatipBK, AggregationBK]
                    CM.setSetupScribes c [ ScribeDefinition {
                                              scName = "stdout"
                                            , scKind = StdoutSK
                                            , scRotation = Nothing
                                            }
                                    ]
                    CM.setDefaultScribes c ["StdoutSK::stdout"]

                    return c
\end{spec}

2. |c| is the |Configuration| of |trace|. In order to
   enable the collection and processing of measurements
   (min, max, mean, std-dev) |AggregationBK| is needed.

\begin{spec}
        CM.setDefaultBackends c [KatipBK, AggregationBK]
\end{spec}
in a configuration file (YAML) means

\begin{spec}
        defaultBackends:
          - KatipBK
          - AggregationBK
\end{spec}

3. Set the measurements that you want to take by changing
   the configuration of the |trace| using |setSubTrace|,
   in order to declare the namespace where we want to
   enable the particular measurements and the list with
   the kind of measurements.

\begin{spec}
        CM.setSubTrace
            config
            "submit-tx"
            (Just $ ObservableTraceSelf observablesSet)
          where
            observablesSet = [MonotonicClock, MemoryStats]
\end{spec}

4. Find an action to measure. e.g.:

\begin{spec}
        runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())
\end{spec}

    and use |bracketObserveIO|. e.g.:


\begin{spec}
        bracketObserveIO trace "submit-tx" $
            runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())
\end{spec}

\begin{code}
bracketObserveIO :: Config.Configuration -> Trace IO a -> Severity -> Text -> IO t -> IO t
bracketObserveIO config trace severity name action = do
    subTrace <- fromMaybe Neutral <$> Config.findSubTrace config name
    bracketObserveIO' subTrace severity trace action
  where
    bracketObserveIO' :: SubTrace -> Severity -> Trace IO a -> IO t -> IO t
    bracketObserveIO' NoTrace _ _ act = act
    bracketObserveIO' subtrace sev logTrace act = do
        mCountersid <- observeOpen subtrace sev logTrace

        -- run action; if an exception is caught it will be logged and rethrown.
        t <- act `catch` (\(e :: SomeException) -> (Text.hPutStrLn stderr (textShow e) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                Text.hPutStrLn stderr ("ObserveOpen: " <> textShow openException)
            Right countersid -> do
                    res <- observeClose subtrace sev logTrace countersid []
                    case res of
                        Left ex -> Text.hPutStrLn stderr ("ObserveClose: " <> textShow ex)
                        _ -> pure ()
        pure t

\end{code}

\subsubsection{Monadic.bracketObserverM}
Observes a |MonadIO m => m| action.
\begin{code}
bracketObserveM :: (MonadCatch m, MonadIO m) => Config.Configuration -> Trace m a -> Severity -> Text -> m t -> m t
bracketObserveM config trace severity name action = do
    subTrace <- liftIO $ fromMaybe Neutral <$> Config.findSubTrace config name
    bracketObserveM' subTrace severity trace action
  where
    bracketObserveM' :: (MonadCatch m, MonadIO m) => SubTrace -> Severity -> Trace m a -> m t -> m t
    bracketObserveM' NoTrace _ _ act = act
    bracketObserveM' subtrace sev logTrace act = do
        mCountersid <- observeOpen subtrace sev logTrace

        -- run action; if an exception is caught it will be logged and rethrown.
        t <- act `catch` (\(e :: SomeException) -> liftIO (Text.hPutStrLn stderr (textShow e) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                liftIO $ Text.hPutStrLn stderr ("ObserveOpen: " <> textShow openException)
            Right countersid -> do
                    res <- observeClose subtrace sev logTrace countersid []
                    case res of
                        Left ex -> liftIO (Text.hPutStrLn stderr ("ObserveClose: " <> textShow ex))
                        _ -> pure ()
        pure t

\end{code}

\subsubsection{Monadic.bracketObserver}
Observes a |MonadIO m => m| action. This observer bracket does not interfere
on exceptions.
\begin{code}
bracketObserveX :: (MonadIO m) => Config.Configuration -> Trace m a -> Severity -> Text -> m t -> m t
bracketObserveX config trace severity name action = do
    subTrace <- liftIO $ fromMaybe Neutral <$> Config.findSubTrace config name
    bracketObserveX' subTrace severity trace action
  where
    bracketObserveX' :: (MonadIO m) => SubTrace -> Severity -> Trace m a -> m t -> m t
    bracketObserveX' NoTrace _ _ act = act
    bracketObserveX' subtrace sev logTrace act = do
        countersid <- observeOpen0 subtrace sev logTrace

        -- run action
        t <- act

        observeClose0 subtrace sev logTrace countersid []

        pure t

\end{code}

\subsubsection{observerOpen}\label{observeOpen}
\begin{code}
observeOpen :: (MonadCatch m, MonadIO m) => SubTrace -> Severity -> Trace m a -> m (Either SomeException CounterState)
observeOpen subtrace severity logTrace = (do
    state <- observeOpen0 subtrace severity logTrace
    return (Right state)) `catch` (return . Left)

observeOpen0 :: (MonadIO m) => SubTrace -> Severity -> Trace m a -> m CounterState
observeOpen0 subtrace severity logTrace = do
    -- take measurement
    counters <- liftIO $ readCounters subtrace
    let state = CounterState counters
    if counters == []
    then return ()
    else do
        -- send opening message to Trace
        meta <- mkLOMeta severity Confidential
        traceNamedObject logTrace (meta, ObserveOpen state)
    return state

\end{code}

\subsubsection{observeClose}\label{observeClose}
\begin{code}
observeClose
    :: (MonadCatch m, MonadIO m) => SubTrace -> Severity -> Trace m a
    -> CounterState -> [(LOMeta, LOContent a)]
    -> m (Either SomeException ())
observeClose subtrace sev logTrace initState logObjects = (do
    observeClose0 subtrace sev logTrace initState logObjects
    return (Right ())) `catch` (return . Left)

observeClose0 :: (MonadIO m) => SubTrace -> Severity -> Trace m a
    -> CounterState -> [(LOMeta, LOContent a)]
    -> m ()
observeClose0 subtrace sev logTrace initState logObjects = do
    let initialCounters = csCounters initState

    -- take measurement
    counters <- liftIO $ readCounters subtrace
    if counters == []
    then return ()
    else do
        mle <- mkLOMeta sev Confidential
        -- send closing message to Trace
        traceNamedObject logTrace $
            (mle, ObserveClose (CounterState counters))
        -- send diff message to Trace
        traceNamedObject logTrace $
            (mle, ObserveDiff (CounterState (diffCounters initialCounters counters)))
    -- trace the messages gathered from inside the action
    forM_ logObjects $ traceNamedObject logTrace
    return ()

\end{code}

\subsubsection{textShow}\label{textShow}
\begin{code}
-- The text package version 2.1.2 and later has the function Text.show, but we cannot
-- use that because ghc-8.10 cannot build text-2.1.2. As soon as ghc8.10 can be 
-- dropped, we should switch to `Text.show`.
textShow :: Show a => a -> Text
textShow = Text.pack . show
\end{code}
