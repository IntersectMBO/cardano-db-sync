
\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-@ LIQUID "--prune-unsorted" @-}

module Cardano.BM.Tracer
    (
      Tracer (..)
    -- * observing
    , bracketObserve
    -- * examples
    , example
    , exampleWithChoose
    ) where

import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Contravariant.Divisible (Divisible (..),
                     Decidable (..))
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Control.Tracer (Tracer (..), mkTracer, nullTracer,
                                     stdoutTracer, traceWith)
import           Control.Tracer.Legacy (showTracing)
import           Control.Tracer.Observe (Observable (..), ObserveIndicator (..))

\end{code}
%endif

\subsubsection{Divisible and Decidable instances of |Tracer|}
A |Divisible| contravariant functor is the contravariant analogue of
|Applicative|. A |Divisible| contravariant functor has the ability to
be composed "beside" another contravariant. It gives a way to combine
two contravariant functors that focus on different parts of a
structure.
(see \url{https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant-Divisible.html#g:1})

\begin{code}
instance Monad m => Divisible (Tracer m) where
  divide  :: (a -> (b, c)) -> Tracer m b -> Tracer m c -> Tracer m a
  divide f tb tc = mkTracer $ \a -> case f a of
    (b, c) -> traceWith tb b *> traceWith tc c

  conquer :: Tracer m a
  conquer = nullTracer

\end{code}
A |Decidable| contravariant functor is the contravariant analogue of
|Alternative|. Noting the superclass constraint that the
contravariant functor must also be |Divisible|, a |Decidable| functor
has the ability to "fan out" input, under the intuition that
contravariant functors consume input. It chooses the appropriate
contravariant functor for a data structure that is an alternative
choice (sum) of two different parts.
(see \url{https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant-Divisible.html#g:2})

\begin{code}
instance Monad m => Decidable (Tracer m) where
  lose :: (a -> Void) -> Tracer m a
  lose _ = nullTracer

  choose :: (a -> Either b c) -> Tracer m b -> Tracer m c -> Tracer m a
  choose f tb tc = mkTracer $ either (traceWith tb) (traceWith tc) . f

\end{code}

\subsubsection{bracketObserve}\label{code:bracketObserve}\index{bracketObserve}
Indicates the beginning and the end of an action.
|matchObservations| can be used if we want a |Tracer| which
produces the difference between the starting and the ending
observations of the action.
\begin{code}
bracketObserve :: forall m s e b d . Monad m
               => (m s, m e, Tracer m (Observable s e d))
               -> m b
               -> m b
bracketObserve (getStart, getEnd, tr) action = do

    let transform :: Tracer m (Observable s e d) -> Tracer m ObserveIndicator
        transform trace =  mkTracer $ \case
            ObserveBefore -> do
                start <- getStart
                traceWith trace $ OStart start
            ObserveAfter -> do
                end <- getEnd
                traceWith trace $ OEnd end Nothing

        tr' = transform tr

    traceWith tr' ObserveBefore
    res <- action
    traceWith tr' ObserveAfter

    return res

\end{code}

\subsubsection{example}
\begin{code}
data AddSub a = Add a
              | Sub a
              deriving Show

type Time = Word64

type ObservableS t = Observable t t t

example :: IO Int
example = do
    let trInt :: Tracer IO (AddSub Int)
        trInt = showTracing stdoutTracer
        trObserve :: Tracer IO (ObservableS Time)
        trObserve = showTracing stdoutTracer

    _ <- bracketObserve (getMonotonicTimeNSec, getMonotonicTimeNSec, trObserve) (actionAdd trInt)
    bracketObserve (getMonotonicTimeNSec, getMonotonicTimeNSec, trObserve) (actionSub trInt)

  where
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd tr = do
        let res = 1+2
        traceWith tr $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub tr = do
        let res = 1-2
        traceWith tr $ Sub res
        return res

exampleWithChoose :: IO Int
exampleWithChoose = do
    let trInt :: Tracer IO (AddSub Int)
        trInt = showTracing stdoutTracer
        trObserve :: Tracer IO (ObservableS (AddSub Time))
        trObserve = showTracing stdoutTracer

        trace :: Tracer IO (Either (ObservableS (AddSub Time)) (AddSub Int))
        trace = choose id trObserve trInt

        bracketObserve' (getTime, tr) = bracketObserve (getTime, getTime, tr)

    _ <- bracketObserve' (Add <$> getMonotonicTimeNSec, contramap Left trace) $ actionAdd $ contramap Right trace
    bracketObserve' (Sub <$> getMonotonicTimeNSec, contramap Left trace) $ actionSub $ contramap Right trace

  where
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd tr = do
        let res = 1+2
        traceWith tr $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub tr = do
        let res = 1-2
        traceWith tr $ Sub res
        return res

instance Show (ObservableS Time) where
  show (OStart time)     = "OStart " ++ show time
  show (OEnd time mTime) = "OEnd "   ++ show time ++ ", ODiff " ++ show mTime

instance Show (ObservableS (AddSub Time)) where
  show (OStart a)   = "OStart " ++ show a
  show (OEnd   a b) = "OEnd "   ++ show a ++ ", ODiff "  ++ show b

\end{code}
