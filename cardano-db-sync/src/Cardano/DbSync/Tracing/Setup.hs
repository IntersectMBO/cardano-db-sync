{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

-- | Initialisation of the @trace-dispatcher@ tracers used by db-sync.
--
-- This is the replacement for the old @iohk-monitoring@ @setupTrace@:
-- it creates the stdout tracer, and optionally (when a @cardano-tracer@
-- socket was given on the command line) the forwarding tracers, the EKG
-- metrics store and the @NodeInfo@ data point.
module Cardano.DbSync.Tracing.Setup (
  DbSyncTracers (..),
  mkDbSyncTracers,
  mkStdoutTracer,
  stdoutOnlyTracers,
  toTracer,
) where

import Cardano.Client.Subscription (MuxTrace, SubscriptionTrace)
import qualified Cardano.Db as DB
import Cardano.DbSync.Tracing.Network ()
import Cardano.DbSync.Types (CardanoBlock)
import Cardano.Logging (
  BackendConfig (..),
  ConfigOption (..),
  FormatLogging (..),
  ForwarderMode (..),
  HowToConnect (..),
  Trace,
  TraceConfig (..),
  configureTracers,
  dataPointTracer,
  defaultForwarder,
  ekgTracer,
  emptyConfigReflection,
  forwardTracer,
  mkCardanoTracer,
  mkDataPointTracer,
  standardTracer,
  traceTracerInfo,
  traceWith,
 )
import Cardano.Logging.Resources ()
import Cardano.Logging.Types.NodeInfo (NodeInfo (..))
import Cardano.Network.NodeToClient (
  ConnectionId,
  Handshake,
  IOManager,
  LocalAddress,
  TraceSendRecv,
 )
import qualified Cardano.Network.NodeToClient.Version as Network
import Cardano.Node.Tracing.NodeInfo ()
import Cardano.Node.Tracing.Tracers.NodeToClient ()
import Cardano.Node.Tracing.Tracers.Resources (startResourceTracer)
import qualified Codec.CBOR.Term as CBOR
import Control.Applicative ((<|>))
import Control.DeepSeq (deepseq)
import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Version (showVersion)
import qualified Network.Mux as Mux
import Network.Mux.Tracing ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Point, Tip)
import qualified Ouroboros.Network.Driver.Stateful as Stateful
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Ouroboros.Network.Tracing ()
import Paths_cardano_db_sync (version)
import qualified System.Metrics as EKG
import Trace.Forward.Forwarding (InitForwardingConfig (..), initForwardingDelayed)
import Trace.Forward.Utils.TraceObject (writeToSink)
import "contra-tracer" Control.Tracer (Tracer (..))

-- | All tracers used by db-sync.
data DbSyncTracers = DbSyncTracers
  { dstTracer :: !(Trace IO DB.LogMessage)
  -- ^ The main tracer for all db-sync log messages.
  , dstMuxTracer :: !(Trace IO (Mux.WithBearer (ConnectionId LocalAddress) MuxTrace))
  , dstHandshakeTracer ::
      !( Trace
           IO
           ( Mux.WithBearer
               (ConnectionId LocalAddress)
               (TraceSendRecv (Handshake Network.NodeToClientVersion CBOR.Term))
           )
       )
  , dstSubscriptionTracer :: !(Trace IO (SubscriptionTrace ()))
  , dstChainSyncTracer ::
      !(Trace IO (TraceSendRecv (ChainSync CardanoBlock (Point CardanoBlock) (Tip CardanoBlock))))
  , dstStateQueryTracer ::
      !( Trace
           IO
           ( Stateful.TraceSendRecv
               (LocalStateQuery.LocalStateQuery CardanoBlock (Point CardanoBlock) (Query CardanoBlock))
               LocalStateQuery.State
           )
       )
  }

-- | Convert a trace-dispatcher 'Trace' into a plain contra-tracer 'Tracer'.
toTracer :: Trace IO a -> Tracer IO a
toTracer tr = Tracer (traceWith tr)

-- | Create all db-sync tracers.
--
-- When the first argument is 'False' (@EnableLogging: False@ in the config)
-- all returned tracers are no-ops.
--
-- When a @cardano-tracer@ socket path is given, a forwarding connection
-- (initiator mode) is established: log messages, EKG metrics and the
-- @NodeInfo@ data point are served to @cardano-tracer@ over that socket.
mkDbSyncTracers ::
  IOManager ->
  -- | Is logging enabled?
  Bool ->
  TraceConfig ->
  -- | Socket path of a cardano-tracer to forward to.
  Maybe FilePath ->
  NetworkMagic ->
  -- | Node name used when forwarding (when the config has no @TraceOptionNodeName@).
  Text ->
  -- | System start time (for the @NodeInfo@ data point).
  UTCTime ->
  IO DbSyncTracers
mkDbSyncTracers iomgr enableLogging traceConfig mTracerSocket networkMagic fallbackName systemStart
  | not enableLogging =
      pure
        DbSyncTracers
          { dstTracer = mempty
          , dstMuxTracer = mempty
          , dstHandshakeTracer = mempty
          , dstSubscriptionTracer = mempty
          , dstChainSyncTracer = mempty
          , dstStateQueryTracer = mempty
          }
  | otherwise = do
      let trConfig =
            addForwarderBackend mTracerSocket $
              traceConfig {tcNodeName = tcNodeName traceConfig <|> Just fallbackName}

      ekgStore <- EKG.newStore
      EKG.registerGcMetrics ekgStore
      ekgTrace <- ekgTracer trConfig ekgStore

      stdoutTrace <- standardTracer

      -- Initialise forwarding only when a cardano-tracer socket was given.
      (fwdTracer, dpTracer, kickoffForwarder) <-
        case mTracerSocket of
          Nothing -> pure (mempty, mempty, pure ())
          Just socketPath -> do
            let initForwConf =
                  InitForwardingWith
                    { initNetworkMagic = networkMagic
                    , initEKGStore = Just ekgStore
                    , initOnForwardInterruption = Nothing
                    , initOnQueueOverflow = Nothing
                    , initHowToConnect = LocalPipe socketPath
                    , initForwarderMode = Initiator
                    }
                forwardingConf = fromMaybe defaultForwarder (tcForwarder trConfig)
            (forwardSink, dpStore, kickoff) <-
              initForwardingDelayed iomgr forwardingConf initForwConf
            pure (forwardTracer (writeToSink forwardSink), dataPointTracer dpStore, kickoff)

      configReflection <- emptyConfigReflection

      mainTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["DBSync"]
      configureTracers configReflection trConfig [mainTracer]

      muxTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["Mux"]
      configureTracers configReflection trConfig [muxTracer]

      handshakeTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["Handshake"]
      configureTracers configReflection trConfig [handshakeTracer]

      subscriptionTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["Subscription"]
      configureTracers configReflection trConfig [subscriptionTracer]

      chainSyncTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["ChainSync"]
      configureTracers configReflection trConfig [chainSyncTracer]

      stateQueryTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["LocalStateQuery"]
      configureTracers configReflection trConfig [stateQueryTracer]

      traceTracerInfo stdoutTrace fwdTracer configReflection

      -- Publish the NodeInfo data point (queried by cardano-tracer) and
      -- then start the forwarding connection.
      whenJust' mTracerSocket $ do
        nodeInfoTracer <- mkDataPointTracer dpTracer
        nodeInfo <- mkNodeInfo (fromMaybe fallbackName (tcNodeName trConfig)) systemStart
        nodeInfo `deepseq` traceWith nodeInfoTracer nodeInfo
      kickoffForwarder

      -- Periodic resource stats (only when configured via TraceOptionResourceFrequency).
      whenJust (tcResourceFrequency trConfig) $ \frequency ->
        when (frequency > 0) $ do
          resourcesTracer <- mkCardanoTracer stdoutTrace fwdTracer (Just ekgTrace) ["Resources"]
          configureTracers configReflection trConfig [resourcesTracer]
          startResourceTracer (toTracer resourcesTracer) frequency

      pure
        DbSyncTracers
          { dstTracer = mainTracer
          , dstMuxTracer = muxTracer
          , dstHandshakeTracer = handshakeTracer
          , dstSubscriptionTracer = subscriptionTracer
          , dstChainSyncTracer = chainSyncTracer
          , dstStateQueryTracer = stateQueryTracer
          }
  where
    whenJust' :: Maybe a -> IO () -> IO ()
    whenJust' m act = maybe (pure ()) (const act) m

-- | A simple stdout-only tracer for db-sync messages. Used by tests and
-- the smash server, where no forwarding is needed.
mkStdoutTracer :: Bool -> TraceConfig -> Text -> IO (Trace IO DB.LogMessage)
mkStdoutTracer enableLogging traceConfig name
  | not enableLogging = pure mempty
  | otherwise = do
      stdoutTrace <- standardTracer
      configReflection <- emptyConfigReflection
      tracer <- mkCardanoTracer stdoutTrace mempty Nothing [name]
      configureTracers configReflection traceConfig [tracer]
      pure tracer

-- | Tracers that only emit the main db-sync messages and silence the
-- network protocol tracers. Used by the tests.
stdoutOnlyTracers :: Trace IO DB.LogMessage -> DbSyncTracers
stdoutOnlyTracers tracer =
  DbSyncTracers
    { dstTracer = tracer
    , dstMuxTracer = mempty
    , dstHandshakeTracer = mempty
    , dstSubscriptionTracer = mempty
    , dstChainSyncTracer = mempty
    , dstStateQueryTracer = mempty
    }

-- | When a forwarding socket is given, make sure the 'Forwarder' backend is
-- active, otherwise nothing would be sent over the connection. The backend
-- is added to the root namespace unless the configuration already mentions
-- the 'Forwarder' backend anywhere.
addForwarderBackend :: Maybe FilePath -> TraceConfig -> TraceConfig
addForwarderBackend mSocket tc =
  case mSocket of
    Nothing -> tc
    Just _
      | forwarderConfigured -> tc
      | otherwise -> tc {tcOptions = Map.alter addToRoot [] (tcOptions tc)}
  where
    forwarderConfigured =
      any (any hasForwarder) (Map.elems (tcOptions tc))

    hasForwarder :: ConfigOption -> Bool
    hasForwarder (ConfBackend backends) = Forwarder `elem` backends
    hasForwarder _ = False

    addToRoot :: Maybe [ConfigOption] -> Maybe [ConfigOption]
    addToRoot Nothing = Just [ConfBackend [Stdout HumanFormatUncoloured, Forwarder]]
    addToRoot (Just opts) = Just (map addBackend opts <> newBackend opts)
      where
        addBackend (ConfBackend backends) = ConfBackend (backends <> [Forwarder])
        addBackend other = other
        newBackend os =
          [ ConfBackend [Stdout HumanFormatUncoloured, Forwarder]
          | not (any isBackend os)
          ]
        isBackend ConfBackend {} = True
        isBackend _ = False

mkNodeInfo :: Text -> UTCTime -> IO NodeInfo
mkNodeInfo name systemStart = do
  now <- getCurrentTime
  pure $
    NodeInfo
      { niName = name
      , niProtocol = "cardano"
      , niVersion = Text.pack (showVersion version)
      , niCommit = DB.gitRev
      , niStartTime = now
      , niSystemStartTime = systemStart
      }
