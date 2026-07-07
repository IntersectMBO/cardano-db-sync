
\subsection{Cardano.BM.Backend.Switchboard}
\label{code:Cardano.BM.Backend.Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.BM.Backend.Switchboard
    (
      Switchboard(..)
    , SwitchboardInternal (..)
    , setSbEKGServer
    , getSbEKGServer
    , mainTraceConditionally
    , readLogBuffer
    , effectuate
    , realize
    , unrealize
    , waitForTermination
    -- * integrate external backend
    , addUserDefinedBackend
    , addExternalBackend
    , addExternalScribe
    -- * testing
    --, realizeSwitchboard
    --, unrealizeSwitchboard
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, modifyMVar_,
                     putMVar, readMVar, tryReadMVar, withMVar)
import           Control.Concurrent.STM (atomically, retry)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM, fromException)
import           Control.Exception (SomeException(..))
import           Control.Monad (forM_, when)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Conc (labelThread, myThreadId)
import           GHC.IO.Exception (BlockedIndefinitelyOnSTM)
import qualified Katip as K
import           System.IO (stderr)
import           System.Remote.Monitoring.Wai (Server)

import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Configuration.Model (getBackends,
                     getSetupBackends, setSeverity, setMinSeverity)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace (SubTrace (..))
import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Data.Tracer (mkTracer)
import qualified Cardano.BM.Backend.Log
import qualified Cardano.BM.Backend.LogBuffer

\end{code}
%endif

\subsubsection{Switchboard}\label{code:Switchboard}\index{Switchboard}

We are using an |MVar| because we spawn a set of backends that may try to send messages to
the switchboard before it is completely setup.

\begin{code}

type SwitchboardMVar a = MVar (SwitchboardInternal a)

newtype Switchboard a = Switchboard
    { getSB :: SwitchboardMVar a
    }

data SwitchboardInternal a = SwitchboardInternal
    { sbQueue     :: TBQ.TBQueue (LogObject a)
    , sbDispatch  :: Async.Async ()
    , sbLogBuffer :: !(Cardano.BM.Backend.LogBuffer.LogBuffer a)
    , sbLogBE     :: !(Cardano.BM.Backend.Log.Log a)
    , sbBackends  :: NamedBackends a
    , sbRunning   :: !SwitchboardStatus
    , sbEKGServer :: Maybe Server
    }

setSbEKGServer :: Maybe Server -> Switchboard a -> IO ()
setSbEKGServer condSrv (Switchboard sbv) =
    modifyMVar_ sbv (\sb -> pure $ sb {sbEKGServer = condSrv})

getSbEKGServer :: Switchboard a -> IO (Maybe Server)
getSbEKGServer (Switchboard sbv) = do
    sbi <- readMVar sbv
    pure (sbEKGServer sbi)

type NamedBackends a = [(BackendKind, Backend a)]

data SwitchboardStatus
    = SwitchboardRunning
    | SwitchboardStopped
    deriving (Eq, Show)

\end{code}

\subsubsection{Trace that forwards to the |Switchboard|}
\label{code:mainTraceConditionally}\index{mainTraceConditionally}
Every |Trace| ends in the |Switchboard| which then takes care of
dispatching the messages to the selected backends.
\\
This |Tracer| will forward all messages unconditionally to the |Switchboard|.
(currently disabled)
\begin{spec}
mainTrace :: IsEffectuator eff a => eff a -> Tracer IO (LogObject a)
mainTrace = Tracer . effectuate

\end{spec}

This |Tracer| will apply to every message the severity filter as defined in the |Configuration|.
\begin{code}
mainTraceConditionally :: IsEffectuator eff a => Configuration -> eff a -> Trace IO a
mainTraceConditionally config eff = mkTracer $ \(ctxname,item) -> do
    mayItem <- Config.testSubTrace config ctxname item
    case mayItem of
        Just itemF@(LogObject _loname meta _) -> do
            passSevFilter <- Config.testSeverity config ctxname meta
            when passSevFilter $
                -- pass to backend and insert name
                effectuate eff itemF { loName = ctxname }
        Nothing -> pure ()

\end{code}

\subsubsection{Process incoming messages}\index{Switchboard!instance of IsEffectuator}
Incoming messages are put into the queue, and then processed by the dispatcher.
The switchboard will never block when processing incoming messages
("eager receiver").
\newline
The queue is initialized and the message dispatcher launched.

\begin{code}
instance IsEffectuator Switchboard a where
    effectuate switchboard item = do
        let writequeue :: TBQ.TBQueue (LogObject a) -> LogObject a -> IO ()
            writequeue q i = do
                    nocapacity <- atomically $ TBQ.isFullTBQueue q
                    if nocapacity
                    then handleOverflow switchboard
                    else atomically $ TBQ.writeTBQueue q i

        sb <- readMVar (getSB switchboard)

        if (sbRunning sb) == SwitchboardRunning
            then writequeue (sbQueue sb) item
            else TIO.hPutStrLn stderr "Error: Switchboard is not running, dropping log items!"

    handleOverflow _ = TIO.hPutStrLn stderr "Error: Switchboard's queue full, dropping log items!"

\end{code}

\subsubsection{|Switchboard| implements |Backend| functions}\index{Switchboard!instance of IsBackend}

|Switchboard| is an |IsBackend|
\begin{code}
instance (FromJSON a, ToJSON a) => IsBackend Switchboard a where
    bekind _ = SwitchboardBK

    realize cfg = realizeSwitchboard cfg
    unrealize switchboard = unrealizeSwitchboard switchboard


realizeSwitchboard :: (FromJSON a, ToJSON a) => Configuration -> IO (Switchboard a)
realizeSwitchboard cfg = do
    -- we setup |LogBuffer| explicitly so we can access it as a |Backend| and as |LogBuffer|
    logbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize cfg
    katipBE :: Cardano.BM.Backend.Log.Log a <- Cardano.BM.Backend.Log.realize cfg
    let spawnDispatcher :: Switchboard a -> TBQ.TBQueue (LogObject a) -> IO (Async.Async ())
        spawnDispatcher switchboard queue =

            let sendMessage nli befilter = do
                    let name = case nli of
                            LogObject loname _ (LogValue valueName _) ->
                                loname <> "." <> valueName
                            LogObject loname _ _ -> loname
                    selectedBackends <- getBackends cfg name
                    let selBEs = befilter selectedBackends
                    withMVar (getSB switchboard) $ \sb ->
                        forM_ (sbBackends sb) $ \(bek, be) ->
                            when (bek `elem` selBEs) (bEffectuate be nli)

                qProc = do
                    -- read complete queue at once and process items
                    nlis <- atomically $ do
                                  r <- TBQ.flushTBQueue queue
                                  when (null r) retry
                                  return r

                    let processItem nli@(LogObject loname _ loitem) = do
                            Config.findSubTrace cfg loname >>= \case
                                Just (TeeTrace sndName) ->
                                    atomically $ TBQ.writeTBQueue queue $ nli{ loName = loname <> "." <> sndName }
                                _ -> return ()

                            case loitem of
                                KillPill -> do
                                    -- each of the backends will be terminated sequentially
                                    withMVar (getSB switchboard) $ \sb ->
                                        forM_ (sbBackends sb) ( \(_, be) -> bUnrealize be )
                                    -- all backends have terminated
                                    return False
                                (AggregatedMessage _) -> do
                                    sendMessage nli (filter (/= AggregationBK))
                                    return True
                                (MonitoringEffect (MonitorAlert _)) -> do
                                    sendMessage nli (filter (/= MonitoringBK))
                                    return True
                                (MonitoringEffect (MonitorAlterGlobalSeverity sev)) -> do
                                    setMinSeverity cfg sev
                                    return True
                                (MonitoringEffect (MonitorAlterSeverity loggerName sev)) -> do
                                    setSeverity cfg loggerName (Just sev)
                                    return True
                                (Command (DumpBufferedTo bk)) -> do
                                    msgs <- Cardano.BM.Backend.LogBuffer.readBuffer logbuf
                                    forM_ msgs (\(lonm, lobj) -> sendMessage (lobj {loName = lonm}) (const [bk]))
                                    return True
                                _ -> do
                                    sendMessage nli id
                                    return True

                    res <- mapM processItem nlis
                    when (and res) $ qProc
            in
            Async.async $ do
              myThreadId >>= flip labelThread "Switchboard dispatcher (iohk-monitoring)"
              qProc

#ifdef PERFORMANCE_TEST_QUEUE
    let qSize = 1000000
#else
    let qSize = 2048
#endif
    q <- atomically $ TBQ.newTBQueue qSize
    sbref <- newEmptyMVar

    let sb :: Switchboard a = Switchboard sbref

    backends <- getSetupBackends cfg
    bs0 <- setupBackends backends cfg sb
    bs1 <- return (LogBufferBK, MkBackend
                        { bEffectuate = Cardano.BM.Backend.LogBuffer.effectuate logbuf
                        , bUnrealize = Cardano.BM.Backend.LogBuffer.unrealize logbuf
                        })
    bs2 <- return (KatipBK, MkBackend
                        { bEffectuate = Cardano.BM.Backend.Log.effectuate katipBE
                        , bUnrealize = Cardano.BM.Backend.Log.unrealize katipBE
                        })

    let bs = bs2 : bs1 : bs0
    dispatcher <- spawnDispatcher sb q
    -- link the given Async to the current thread, such that if the Async
    -- raises an exception, that exception will be re-thrown in the current
    -- thread, wrapped in ExceptionInLinkedThread.
    Async.linkOnly (not . isBlockedIndefinitelyOnSTM) dispatcher

    -- Modify the internal state of the switchboard, the switchboard
    -- is now running.
    putMVar sbref $ SwitchboardInternal
        { sbQueue = q
        , sbDispatch = dispatcher
        , sbLogBuffer = logbuf
        , sbLogBE = katipBE
        , sbBackends = bs
        , sbRunning = SwitchboardRunning
        , sbEKGServer = Nothing
        }
    return sb



unrealizeSwitchboard :: Switchboard a -> IO ()
unrealizeSwitchboard switchboard = do
    -- Here we are doing a modification to send the "kill pill"
    -- to the queue and we are waiting for the dispather to exit.
    -- At the end, either return the result or throw an exception.
    dispatcher <- withMVar (getSB switchboard) $ \sb -> do
        let dispatcher  = sbDispatch sb
        let queue       = sbQueue sb

        -- Create terminating item, the "kill pill".
        lo <- LogObject <$> pure "kill.switchboard"
                        <*> (mkLOMeta Warning Confidential)
                        <*> pure KillPill

        -- Send terminating item to the queue.
        atomically $ TBQ.writeTBQueue queue lo

        -- Return the dispatcher.
        return dispatcher

    -- Wait for the dispatcher to exit.
    res <- Async.waitCatch dispatcher

    -- Either raise an exception or return the result.
    either throwM return res

    -- Modify the state in the end so we signal that the switchboard is shut down.
    _ <- withMVar (getSB switchboard) (\sb -> return $ sb { sbRunning = SwitchboardStopped })

    pure ()



isBlockedIndefinitelyOnSTM :: SomeException -> Bool
isBlockedIndefinitelyOnSTM e =
  isJust (fromException e :: Maybe BlockedIndefinitelyOnSTM)

\end{code}

\subsubsection{Integrate with external backend}\label{code:addUserDefinedBackend}\index{addUserDefinedBackend}
\begin{code}
addUserDefinedBackend :: Switchboard a -> Backend a -> Text -> IO ()
addUserDefinedBackend switchboard be name =
    modifyMVar_ (getSB switchboard) $ \sb ->
        return $ sb { sbBackends = (UserDefinedBK name, be) : sbBackends sb }
\end{code}

\subsubsection{Integrate with external backend}\label{code:addExternalBackend}\index{addExternalBackend}
\begin{code}
addExternalBackend :: Switchboard a -> Backend a -> BackendKind -> IO ()
addExternalBackend switchboard be bk =
    modifyMVar_ (getSB switchboard) $ \sb ->
        return $ sb { sbBackends = (bk, be) : sbBackends sb }

\end{code}

\subsubsection{Integrate with external \emph{katip} scribe}\label{code:addExternalScribe}\index{addExternalScribe}
\begin{code}
addExternalScribe :: Switchboard a -> K.Scribe -> Text -> IO ()
addExternalScribe switchboard sc name =
    withMVar (getSB switchboard) $ \sb ->
        Cardano.BM.Backend.Log.registerScribe (sbLogBE sb) sc name

\end{code}

\subsubsection{Waiting for the switchboard to terminate}\label{code:waitForTermination}\index{waitForTermination}
\begin{code}
waitForTermination :: Switchboard a -> IO ()
waitForTermination switchboard =
    tryReadMVar (getSB switchboard) >>= \case
        Nothing -> return ()
        Just sb -> Async.waitCatch  (sbDispatch sb) >> return ()

\end{code}

\subsubsection{Reading the buffered log messages}\label{code:readLogBuffer}\index{readLogBuffer}
\begin{code}
readLogBuffer :: Switchboard a -> IO [(LoggerName, LogObject a)]
readLogBuffer switchboard = do
    sb <- readMVar (getSB switchboard)
    Cardano.BM.Backend.LogBuffer.readBuffer (sbLogBuffer sb)

\end{code}

\subsubsection{Realizing the backends according to configuration}
\label{code:setupBackends}\index{Switchboard!setupBackends}
\begin{code}
setupBackends :: (FromJSON a, ToJSON a)
              => [BackendKind]
              -> Configuration
              -> Switchboard a
              -> IO [(BackendKind, Backend a)]
setupBackends bes c sb = setupBackendsAcc bes []
  where
    setupBackendsAcc [] acc = return acc
    setupBackendsAcc (bk : r) acc = do
        setupBackend' bk c sb >>= \case
            Nothing -> setupBackendsAcc r acc
            Just be -> setupBackendsAcc r ((bk,be) : acc)

setupBackend' :: (FromJSON a , ToJSON a) => BackendKind -> Configuration -> Switchboard a -> IO (Maybe (Backend a))
setupBackend' SwitchboardBK _ _ = fail "cannot instantiate a further Switchboard"
setupBackend' (UserDefinedBK _) _ _ = fail "cannot instantiate an user-defined backend"
setupBackend' MonitoringBK _ _ = return Nothing
setupBackend' AggregationBK _ _ = return Nothing
setupBackend' EditorBK _ _ = return Nothing
setupBackend' GraylogBK _ _ = return Nothing
setupBackend' EKGViewBK _ _ = return Nothing
setupBackend' KatipBK _ _ = return Nothing
setupBackend' LogBufferBK _ _ = return Nothing
setupBackend' TraceAcceptorBK _ _ = return Nothing
setupBackend' TraceForwarderBK _ _ = return Nothing

\end{code}
