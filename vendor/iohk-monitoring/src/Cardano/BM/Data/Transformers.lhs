\subsection{Cardano.BM.Data.Transformers}
\label{code:Cardano.BM.Data.Transformers}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Transformers
  ( liftCounting
  , liftFolding
  , setHostname
  )
  where

import           Data.Text (Text)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..),
                     LogObject (..), LoggerName)
import           Cardano.BM.Data.Tracer (Tracer (..), mkTracer, traceWith)
import           Cardano.BM.Data.Trace

import           Control.Tracer.Transformers
\end{code}

\subsubsection{Transformer for counting events}
\label{code:liftCounting}
\index{liftCounting}
Lift a 'Counting' tracer into a 'Trace' of 'PureI' messages.
\begin{code}

liftCounting
  :: forall m a
  .  Monad m
  => LOMeta -> LoggerName -> Text -> Trace m a
  -> Tracer m (Counting (LoggerName, LogObject a))
liftCounting meta name desc tr = mkTracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Counting (LoggerName, LogObject a) -> m ()
   traceIncrement t (Counting n) =
    --  traceWith t . LogObject name meta . LogValue desc . PureI $ fromIntegral n
     traceWith t $ (name, LogObject name meta . LogValue desc . PureI $ fromIntegral n)

\end{code}

\subsubsection{Transformer for state folding}
\label{code:liftFolding}
\index{liftFolding}
Lift a 'Trace' tracer into a 'Trace' of 'PureI' messages,
thereby specialising it to 'Integral'.
\begin{code}

liftFolding
  :: forall m f a
  .  (Monad m, Integral f) -- TODO:  generalise
  => LOMeta -> LoggerName -> Text -> Trace m a
  -> Tracer m (Folding (LoggerName, LogObject a) f)
liftFolding meta name desc tr = mkTracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Folding (LoggerName, LogObject a) f -> m ()
   traceIncrement t (Folding f) =
     traceWith t $ (name, LogObject name meta . LogValue desc . PureI $ fromIntegral f)

\end{code}

\subsubsection{Transformer for setting hostname annotation}
\label{code:setHostname}
\index{setHostname}
The hostname annotation of the |LogObject| can be altered.
\begin{code}
setHostname :: Monad m => Text -> Trace m a -> Trace m a
setHostname hn tr = mkTracer $ \(ctx, lo@(LogObject _ln meta _lc)) ->
    traceWith tr (ctx, lo { loMeta = meta { hostname = hn }})

\end{code}
