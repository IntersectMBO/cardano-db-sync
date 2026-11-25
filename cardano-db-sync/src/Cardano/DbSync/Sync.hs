{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Sync (
  ConfigFile (..),
  SyncCommand (..),
  SyncNodeParams (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  SocketPath (..),
  MetricSetters (..),
  SyncEnv (..),
  configureLogging,
  runSyncNodeClient,
) where

import Cardano.Client.Subscription (Decision (..), SubscriptionParams (..), SubscriptionTracers (..), subscribe)
import qualified Cardano.Db as DB
import Cardano.Db.Log (LogMessage, logInfo)
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), LedgerEnv (..), SyncEnv (..), envLedgerEnv, envNetworkMagic, envOptions, soptOnlyFix)
import Cardano.DbSync.Config
import Cardano.DbSync.DbEvent
import Cardano.DbSync.Fix.ProtoMinor (ProtoMinorFixPlan, chainSyncClientFixProtoMinor, planProtoMinorFix)
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Metrics
import Cardano.DbSync.Tracing.Setup (DbSyncTracers (..), toTracer)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Logging (Trace)
import Cardano.Prelude hiding (Meta, Nat, (%))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar, newTVarIO, readTVarIO, writeTVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Network.TypedProtocol.Peer (N (..), Nat (..))
import "contra-tracer" Control.Tracer (nullTracer)

import Cardano.Network.NodeToClient (
  IOManager,
  LocalAddress,
  NodeToClientProtocols (..),
  localSnocket,
  localStateQueryPeerNull,
  localTxMonitorPeerNull,
  localTxSubmissionPeerNull,
 )
import qualified Cardano.Network.NodeToClient.Version as Network
import qualified Network.Mux as Mux
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (configCodec)
import Ouroboros.Consensus.Network.NodeToClient (
  Codecs' (..),
  cChainSyncCodec,
  cStateQueryCodec,
  cTxSubmissionCodec,
  clientCodecs,
 )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion, supportedNodeToClientVersions)
import Ouroboros.Network.Block (
  BlockNo (..),
  Point (..),
  Tip (..),
  blockNo,
  genesisPoint,
  getTipBlockNo,
 )
import Ouroboros.Network.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Network.Mux (MiniProtocolCb (..), RunMiniProtocol (..), RunMiniProtocolWithMinimalCtx)
import qualified Ouroboros.Network.Mux as Mux
import Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (
  ChainSyncClientPipelined (..),
  ClientPipelinedStIdle (..),
  ClientPipelinedStIntersect (..),
  ClientStNext (..),
  chainSyncClientPeerPipelined,
  recvMsgIntersectFound,
  recvMsgIntersectNotFound,
  recvMsgRollBackward,
  recvMsgRollForward,
 )
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (
  MkPipelineDecision,
  PipelineDecision (..),
  pipelineDecisionLowHighMark,
  runPipelineDecision,
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Client (localStateQueryClientPeer)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Ouroboros.Network.Snocket as Snocket
import System.Exit (exitSuccess)

-- | Raised to stop the subscription loop after a @--only-fix@ run, which must not be
-- reconnected. 'runSyncNodeClient' turns it into a clean exit.
data OnlyFixDone = OnlyFixDone
  deriving (Show)

instance Exception OnlyFixDone

runSyncNodeClient ::
  MetricSetters ->
  SyncEnv ->
  IOManager ->
  DbSyncTracers ->
  ThreadChannels ->
  SocketPath ->
  IO ()
runSyncNodeClient metricsSetters syncEnv iomgr tracers tc (SocketPath socketPath) = do
  logInfo (dstTracer tracers) $ "Connecting to node via " <> textShow socketPath
  onlyFixDoneVar <- newTVarIO False
  void $
    subscribe
      (localSnocket iomgr)
      (envNetworkMagic syncEnv)
      (supportedNodeToClientVersions (Proxy @CardanoBlock))
      subscriptionTracers
      subscriptionParams
      (dbSyncProtocols onlyFixDoneVar syncEnv metricsSetters tracers tc codecConfig)
  -- 'subscribe' swallows the exception it was given a 'Decision' for, so the exit is made
  -- here, on the way out of this thread: 'mapConcurrently_' then cancels its siblings and
  -- re-raises in the main thread, which is the only thread the RTS exits for.
  onlyFixDone <- readTVarIO onlyFixDoneVar
  when onlyFixDone $ do
    logInfo (dstTracer tracers) "Exiting after the one-off repair, as --only-fix was given."
    exitSuccess
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec $ getTopLevelConfig syncEnv

    --    nonExperimentalVersions =
    --      filter (\a -> Just a <= snd (lastReleasedNodeVersion Proxy)) $ supportedNodeToClientVersions (Proxy @CardanoBlock)

    subscriptionParams =
      SubscriptionParams
        { spAddress = Snocket.localAddressFromPath socketPath
        , spReconnectionDelay = Nothing
        , spCompleteCb = \case
            Left e
              | Just AsyncCancelled <- fromException e -> Abort
              -- A finished --only-fix run must not be reconnected; the caller exits.
              | Just OnlyFixDone <- fromException e -> Abort
              | otherwise -> Reconnect
            Right _ -> Reconnect
        }

    subscriptionTracers =
      SubscriptionTracers
        { stMuxTracer = toTracer (dstMuxTracer tracers)
        , stHandshakeTracer = toTracer (dstHandshakeTracer tracers)
        , stSubscriptionTracer = toTracer (dstSubscriptionTracer tracers)
        , stMuxChannelTracer = nullTracer
        , stMuxBearerTracer = nullTracer
        }

dbSyncProtocols ::
  StrictTVar IO Bool ->
  SyncEnv ->
  MetricSetters ->
  DbSyncTracers ->
  ThreadChannels ->
  CodecConfig CardanoBlock ->
  Network.NodeToClientVersion ->
  BlockNodeToClientVersion CardanoBlock ->
  NodeToClientProtocols 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
dbSyncProtocols onlyFixDoneVar syncEnv metricsSetters tracers tc codecConfig version bversion =
  NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncPtcl
    , localTxSubmissionProtocol = dummylocalTxSubmit
    , localStateQueryProtocol = localStateQuery
    , localTxMonitorProtocol =
        InitiatorProtocolOnly $
          Mux.mkMiniProtocolCbFromPeer $
            const
              (nullTracer, cTxMonitorCodec codecs, localTxMonitorPeerNull)
    }
  where
    codecs = clientCodecs codecConfig bversion version

    tracer :: Trace IO LogMessage
    tracer = getTrace syncEnv

    localChainSyncPtcl :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $
      MiniProtocolCb $ \_ctx channel -> liftIO $ do
        -- The exit request is raised outside 'logException', so a --only-fix run that did
        -- what it was asked does not log an error on its way out.
        result <- logException tracer "ChainSyncWithBlocksPtcl: " $ do
          logInfo tracer "Starting ChainSync client"
          setConsistentLevel syncEnv Unchecked

          -- A pending one-off repair takes the connection for itself, because the
          -- chainsync protocol cannot be restarted on the same channel once a client
          -- is done. The repair ends the connection and the next one syncs normally.
          mFixPlan <- protoMinorFixPlan
          case mFixPlan of
            Just (dbEnv, plan) -> do
              void $
                runPeer
                  (toTracer (dstChainSyncTracer tracers))
                  (cChainSyncCodec codecs)
                  channel
                  (chainSyncClientPeer (chainSyncClientFixProtoMinor syncEnv dbEnv plan))
              afterRepair "Finished the block.proto_minor repair"
            -- Nothing to repair, which --only-fix still has to be told about: it promises to
            -- exit rather than carry on syncing, whether or not there was work to do.
            Nothing
              | soptOnlyFix (envOptions syncEnv) ->
                  afterRepair "No block.proto_minor repair was pending"
              | otherwise -> do
                  (latestPoints, currentTip) <- waitRestartState tc
                  let (inMemory, onDisk) = List.span snd latestPoints
                  logInfo tracer $
                    mconcat
                      [ "Suggesting intersection points from memory: "
                      , textShow (fst <$> inMemory)
                      , " and from disk: "
                      , textShow (fst <$> onDisk)
                      ]
                  void $
                    runPipelinedPeer
                      (toTracer (dstChainSyncTracer tracers))
                      (cChainSyncCodec codecs)
                      channel
                      ( chainSyncClientPeerPipelined $
                          chainSyncClient metricsSetters tracer (fst <$> latestPoints) currentTip tc
                      )
                  atomically $ writeDbEventQueue tc DbFinish
                  -- We should return leftover bytes returned by 'runPipelinedPeer', but
                  -- client application do not care about them (it's only important if one
                  -- would like to restart a protocol on the same mux and thus bearer).
                  pure (Right ((), Nothing))
        either throwIO pure result

    -- | End the connection the repair took: either hand over to syncing on the next one, or
    -- stop the subscription so the caller can exit.
    afterRepair what
      | soptOnlyFix (envOptions syncEnv) = do
          logInfo tracer $ what <> " and --only-fix was given, so exiting without syncing."
          atomically $ writeTVar onlyFixDoneVar True
          pure (Left OnlyFixDone)
      | otherwise = do
          logInfo tracer $ what <> ", reconnecting to resume syncing."
          pure (Right ((), Nothing))

    protoMinorFixPlan :: IO (Maybe (DB.DbEnv, ProtoMinorFixPlan))
    protoMinorFixPlan =
      case envDbEnv syncEnv of
        Nothing -> pure Nothing
        Just dbEnv -> fmap ((,) dbEnv) <$> planProtoMinorFix syncEnv dbEnv

    dummylocalTxSubmit :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    dummylocalTxSubmit =
      InitiatorProtocolOnly $
        Mux.mkMiniProtocolCbFromPeer $
          const
            ( nullTracer
            , cTxSubmissionCodec codecs
            , localTxSubmissionPeerNull
            )

    localStateQuery :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    localStateQuery =
      case envLedgerEnv syncEnv of
        HasLedger _ ->
          InitiatorProtocolOnly $
            Mux.mkMiniProtocolCbFromPeerSt $
              const
                ( nullTracer
                , cStateQueryCodec codecs
                , LocalStateQuery.StateIdle
                , localStateQueryPeerNull
                )
        NoLedger nle ->
          InitiatorProtocolOnly $
            Mux.mkMiniProtocolCbFromPeerSt $
              const
                ( toTracer (dstStateQueryTracer tracers)
                , cStateQueryCodec codecs
                , LocalStateQuery.StateIdle
                , localStateQueryClientPeer $ localStateQueryHandler nle
                )

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
-- When an intersect with the node is found, we are sure that the next message
-- will trigger the 'recvMsgRollBackward', so this is where we actually handle
-- any necessary rollback. This means that at this point, the 'currentTip' may not
-- be correct. This is not an issue, because we only use it for performance reasons
-- in the pipeline policy.
chainSyncClient ::
  MetricSetters ->
  Trace IO LogMessage ->
  [Point CardanoBlock] ->
  WithOrigin BlockNo ->
  ThreadChannels ->
  ChainSyncClientPipelined CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClient metricsSetters trce latestPoints currentTip tc = do
  ChainSyncClientPipelined $ pure $ clientPipelinedStIdle currentTip latestPoints
  where
    clientPipelinedStIdle ::
      WithOrigin BlockNo ->
      [CardanoPoint] ->
      ClientPipelinedStIdle 'Z CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    clientPipelinedStIdle clientTip points =
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null points then [genesisPoint] else points)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \_hdr tip -> pure $ goTip policy Zero clientTip tip Nothing
          , recvMsgIntersectNotFound = \tip -> pure $ goTip policy Zero clientTip tip Nothing
          }

    policy :: MkPipelineDecision
    policy = pipelineDecisionLowHighMark 1 50

    goTip ::
      MkPipelineDecision ->
      Nat n ->
      WithOrigin BlockNo ->
      Tip CardanoBlock ->
      Maybe [CardanoPoint] ->
      ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    goTip mkPipelineDecision n clientTip serverTip =
      go mkPipelineDecision n clientTip (getTipBlockNo serverTip)

    go ::
      MkPipelineDecision ->
      Nat n ->
      WithOrigin BlockNo ->
      WithOrigin BlockNo ->
      Maybe [CardanoPoint] ->
      ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    go mkPipelineDecision n clientTip serverTip mPoint =
      case (mPoint, n, runPipelineDecision mkPipelineDecision n clientTip serverTip) of
        (Just points, _, _) -> drainThePipe n $ clientPipelinedStIdle clientTip points
        (_, _Zero, (Request, mkPipelineDecision')) ->
          SendMsgRequestNext (pure ()) clientStNext
          where
            clientStNext = mkClientStNext $ goTip mkPipelineDecision' n
        (_, _, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (pure ())
            (go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
        (_, Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined (pure ()) $ go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
            (mkClientStNext $ goTip mkPipelineDecision' n')
        (_, Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext $ goTip mkPipelineDecision' n')

    mkClientStNext ::
      ( WithOrigin BlockNo ->
        Tip CardanoBlock ->
        Maybe [CardanoPoint] ->
        ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
      ) ->
      ClientStNext n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
            logException trce "recvMsgRollForward: " $ do
              setNodeBlockHeight metricsSetters (getTipBlockNo tip)

              newSize <- atomically $ do
                writeDbEventQueue tc $ mkDbApply blk
                lengthDbEventQueue tc

              setDbQueueLength metricsSetters newSize

              pure $ finish (At (blockNo blk)) tip Nothing
        , recvMsgRollBackward = \point tip ->
            logException trce "recvMsgRollBackward: " $ do
              -- This will get the current tip rather than what we roll back to
              -- but will only be incorrect for a short time span.
              (mPoints, newTip) <- waitRollback tc point tip
              pure $ finish newTip tip mPoints
        }

drainThePipe ::
  Nat n ->
  ClientPipelinedStIdle 'Z CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO () ->
  ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
drainThePipe n0 client = go n0
  where
    go ::
      forall n'.
      Nat n' ->
      ClientPipelinedStIdle n' CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    go n =
      case n of
        Zero -> client
        Succ n' ->
          CollectResponse Nothing $
            ClientStNext
              { recvMsgRollForward = \_hdr _tip -> pure $ go n'
              , recvMsgRollBackward = \_pt _tip -> pure $ go n'
              }
