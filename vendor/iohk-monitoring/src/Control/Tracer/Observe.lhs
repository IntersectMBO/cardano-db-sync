
\label{code:Control.Tracer.Observe}

%if style == newcode
\begin{code}
{-|
Module: Control.Tracer.Observe

Functions useful for observing and measuring actions.
-}
{-# LANGUAGE LambdaCase        #-}

module Control.Tracer.Observe
    (
    -- * observing
      ObserveIndicator (..)
    , Observable (..)
    , matchObservations
    ) where

import           Control.Tracer (Tracer, mkTracer, traceWith)

\end{code}
%endif

\subsection{Examples}
Observe the duration of an action using the timedBracketObserve:

\begin{spec}
data AddSub a = Add a
              | Sub a
              deriving Show

type Time = Word64

example :: IO ()
example = do
    let -- a Tracer handling the observations
        trObserve :: Tracer IO (Observable Time Time Time)
        trObserve = showTracing stdoutTracer
        -- a transformer which enriches observations with time measurement
        transform :: Tracer IO (Observable Time Time Time) -> Tracer IO ObserveIndicator
        transform trace = mkTracer $ \observeIndicator -> do
            now <- getMonotonicTimeNSec
            case observeIndicator of
                ObserveBefore -> traceWith trace $ OStart now
                ObserveAfter  -> traceWith trace $ OEnd   now Nothing

    beforeMVarAdd <- newMVar Nothing
    beforeMVarSub <- newMVar Nothing

    let trObserve'  = transform $ matchObservations
                                    (readMVar beforeMVarAdd)
                                    (\x -> modifyMVar_ beforeMVarAdd (const $ return $ Just x))
                                    (flip (-))
                                    trObserve
        trObserve'' = transform $ matchObservations
                                    (readMVar beforeMVarSub)
                                    (\x -> modifyMVar_ beforeMVarSub (const $ return $ Just x))
                                    (flip (-))
                                    trObserve

    -- observe add
    traceWith trObserve' ObserveBefore
    _ <- actionAdd tr
    traceWith trObserve' ObserveAfter

    -- observe sub
    traceWith trObserve'' ObserveBefore
    _ <- actionSub tr
    traceWith trObserve'' ObserveAfter

  where
    tr :: Tracer IO (AddSub Int)
    tr = showTracing stdoutTracer
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd trace = do
        let res = 1+2
        traceWith trace $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub trace = do
        let res = 1-2
        traceWith trace $ Sub res
        return res

instance Show (Observable Time Time Time) where
  show (OStart time)     = "OStart " ++ show time
  show (OEnd time mTime) = "OEnd "   ++ show time ++ ", ODiff " ++ show mTime

\end{spec}

\subsection{Observe}
\subsubsection{ObserveIndicator}\label{code:ObserveIndicator}\index{ObserveIndicator}
Data structure that indicates the beginning and the end of an observation.
\begin{code}
data ObserveIndicator = ObserveBefore | ObserveAfter
                      deriving Show

\end{code}

\subsubsection{Observable}\label{code:Observable}\index{Observable}
Data structure which holds the observation along with the indicator
of the observation.
\begin{code}
data Observable s e d = OStart s
                      | OEnd e (Maybe d)
                      --         ^^ holds the difference between start and end

\end{code}

\subsubsection{matchObservations}\label{code:matchObservations}\index{matchObservations}
Match start and end of observations.
\begin{code}
matchObservations
    :: Monad m
    => m (Maybe s)
    -> (s -> m ())
    -> (s -> e -> d)
    -> Tracer m (Observable s e d)
    -> Tracer m (Observable s e d)
matchObservations getStart putStart f tr = mkTracer $ \case
    obs@(OStart s) -> do
        putStart s
        traceWith tr obs
    (OEnd e _) -> do
        before <- getStart
        traceWith tr $ OEnd e $ fmap ((flip f) e) before

\end{code}

\subsubsection{matchObservationsState}\label{code:matchObservationsState}\index{matchObservationsState}
Match start and end of observations using a |MonadState|.
\begin{spec}
matchObservationsState
    :: MonadState (Maybe s) m
    => (s -> e -> d)
    -> Tracer m (Observable s e d)
    -> Tracer m (Observable s e d)
matchObservationsState f tr = mkTracer $ \case
    obs@(OStart s) -> do
        put $ Just s
        traceWith tr obs
    (OEnd e _) -> do
        before <- get
        traceWith tr $ OEnd e $ fmap ((flip f) e) before

exampleState :: IO ()
exampleState = evalStateT exampleS Nothing

exampleS :: StateT (Maybe Time) IO ()
exampleS = do
    let -- a Tracer handling the observations
        trObserve :: Tracer (StateT (Maybe Time) IO) (Observable Time Time Time)
        trObserve = showTracing stdoutTracer
        -- a transformer which enriches observations with time measurement
        transform
            :: Tracer (StateT (Maybe Time) IO) (Observable Time Time Time)
            -> Tracer (StateT (Maybe Time) IO) ObserveIndicator
        transform trace = mkTracer $ \observeIndicator -> do
            now <- liftIO $ getMonotonicTimeNSec
            case observeIndicator of
                ObserveBefore -> traceWith trace $ OStart now
                ObserveAfter  -> traceWith trace $ OEnd   now Nothing

    let trObserve'  = transform $ matchObservationsState (flip (-)) trObserve
        trObserve'' = transform $ matchObservationsState (flip (-)) trObserve

    -- observe add
    traceWith trObserve' ObserveBefore
    _ <- liftIO $ actionAdd tr
    traceWith trObserve' ObserveAfter

    -- observe sub
    traceWith trObserve'' ObserveBefore
    _ <- liftIO $ actionSub tr
    traceWith trObserve'' ObserveAfter

  where
    tr :: Tracer IO (AddSub Int)
    tr = showTracing stdoutTracer
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd trace = do
        let res = 1+2
        traceWith trace $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub trace = do
        let res = 1-2
        traceWith trace $ Sub res
        return res

\end{spec}
