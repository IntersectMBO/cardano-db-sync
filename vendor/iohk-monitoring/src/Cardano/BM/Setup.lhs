
\subsection{Cardano.BM.Setup}
\label{code:Cardano.BM.Setup}

\begin{figure}[ht]
\centering{
  \includegraphics[scale=0.54]{SetupProcedure.pdf}
}
\caption{Setup procedure}
\end{figure}

%if style == newcode
\begin{code}

module Cardano.BM.Setup
    (
      setupTrace
    , setupTrace_
    , shutdown
    , withTrace
    ) where

import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.Tracer (ToObject)
import qualified Cardano.BM.Backend.Switchboard as Switchboard
import           Cardano.BM.Trace (Trace, appendName, natTrace)

\end{code}
%endif

\subsubsection{setupTrace}\label{code:setupTrace}\index{setupTrace}
Setup a new |Trace| with either a given |Configuration|
or a |FilePath| to a configuration file. After all tracing operations have ended;
|shutdownTrace| must be called.
\begin{code}

setupTrace :: (MonadIO m, ToJSON a, FromJSON a, ToObject a) => Either FilePath Config.Configuration -> Text -> m (Trace m a)
setupTrace (Left cfgFile) name = do
    c <- liftIO $ Config.setup cfgFile
    fst <$> setupTrace_ c name
setupTrace (Right c) name = fst <$> setupTrace_ c name

setupTrace_ :: (MonadIO m, ToJSON a, FromJSON a, ToObject a) => Config.Configuration -> Text -> m (Trace m a, Switchboard.Switchboard a)
setupTrace_ c name = do
    sb <- liftIO $ Switchboard.realize c

    let tr = appendName name $ natTrace liftIO (Switchboard.mainTraceConditionally c sb)
    return (tr,sb)

\end{code}

\subsubsection{shutdown}\label{code:shutdown}\index{shutdown}
Shut down the Switchboard and all the |Trace|s related to it.
\begin{code}
shutdown :: (ToJSON a, FromJSON a, ToObject a) => Switchboard.Switchboard a -> IO ()
shutdown = Switchboard.unrealize

\end{code}

\subsubsection{withTrace}\label{code:withTrace}\index{withTrace}
Setup a |Trace| from |Configuration| and pass it to the action. At the end,
shutdown all the components and close the trace.
\begin{code}
withTrace :: (MonadIO m, MonadMask m, ToJSON a, FromJSON a, ToObject a) =>  Config.Configuration -> Text -> (Trace m a -> m t) -> m t
withTrace cfg name action =
    bracket
        (setupTrace_ cfg name)              -- aquire
        (\(_,sb) -> liftIO $ shutdown sb)   -- release
        (\(tr,_) -> action tr)              -- action

\end{code}
