\subsection{Cardano.BM.Backend.LogBuffer}
\label{module:Cardano.BM.Backend.LogBuffer}

%if style == newcode
\begin{code}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.BM.Backend.LogBuffer
    ( LogBuffer
    , readBuffer
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                     newMVar)
import           Data.Aeson (FromJSON)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import           System.IO (stderr)

import           Cardano.BM.Data.Backend (BackendKind (LogBufferBK),
                     IsBackend (..), IsEffectuator (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LoggerName,
                     LogObject (..))

\end{code}
%endif

\subsubsection{Structure of LogBuffer}\label{code:LogBuffer}\index{LogBuffer}
\begin{code}
newtype LogBuffer a = LogBuffer
    { getLogBuf :: LogBufferMVar a }

type LogBufferMVar a = MVar (LogBufferInternal a)

data LogBufferInternal a = LogBufferInternal
    { logBuffer :: !(LogBufferMap a)
    }

\end{code}

\subsubsection{Relation from log context name to log item}
We keep the latest |LogObject| from a log context in a |HashMap|.
\begin{code}
type LogBufferMap a = HM.HashMap LoggerName (LogObject a)

\end{code}

\subsubsection{Read out the latest |LogObject|s}
Returns a list of the maps keys and values.
And, resets the map.
\begin{code}
readBuffer :: LogBuffer a -> IO [(LoggerName, LogObject a)]
readBuffer buffer =
    modifyMVar (getLogBuf buffer) $ \currentBuffer -> do
        let !l = HM.toList $ logBuffer currentBuffer
        return (LogBufferInternal HM.empty, l)

\end{code}

\subsubsection{LogBuffer is an effectuator}\index{LogBuffer!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for log buffering.
\begin{code}
instance IsEffectuator LogBuffer a where
    effectuate buffer lo@(LogObject loname _lometa (LogValue lvname _lvalue)) =
        modifyMVar_ (getLogBuf buffer) $ \currentBuffer ->
            return $! LogBufferInternal $ HM.insert ("#buffered." <> loname <> "." <> lvname) lo $ logBuffer currentBuffer
    effectuate buffer lo@(LogObject loname _lometa _logitem) =
        modifyMVar_ (getLogBuf buffer) $ \currentBuffer ->
            return $! LogBufferInternal $ HM.insert ("#buffered." <> loname) lo $ logBuffer currentBuffer

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: overflow in LogBuffer, dropping log items!"

\end{code}

\subsubsection{|LogBuffer| implements |Backend| functions}\index{LogBuffer!instance of IsBackend}

|LogBuffer| is an |IsBackend|
\begin{code}
instance FromJSON a => IsBackend LogBuffer a where
    bekind _ = LogBufferBK

    realize _ =
        let emptyBuffer = LogBufferInternal HM.empty
        in
        LogBuffer <$> newMVar emptyBuffer

    unrealize _ = return ()

\end{code}
