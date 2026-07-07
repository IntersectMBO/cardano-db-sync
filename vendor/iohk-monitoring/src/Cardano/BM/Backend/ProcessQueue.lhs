
\subsection{Cardano.BM.Backend.ProcessQueue}
\label{module:Cardano.BM.Backend.ProcessQueue}


%if style == newcode
\begin{code}

module Cardano.BM.Backend.ProcessQueue
    ( processQueue
    ) where

import           Control.Concurrent.STM (atomically, retry)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (when)

import           Cardano.BM.Data.LogItem

\end{code}
%endif


\subsubsection{Read TBQueue in batches}
\begin{code}
processQueue
    :: TBQ.TBQueue (Maybe (LogObject a))
    -> (LogObject a -> b -> IO b)  -- processing function
    -> b                           -- initial state
    -> (b -> IO ())                -- termination function
    -> IO ()
processQueue tbqueue proc state terminate = do
    items <- atomically $ do
                list <- TBQ.flushTBQueue tbqueue
                when (null list) retry
                return list
    processItems state items
    where
    processItems s []             = processQueue tbqueue proc s terminate
    processItems s ((Just lo):is) = proc lo s >>= \s' -> processItems s' is
    processItems s (Nothing  :_ ) = terminate s

\end{code}
