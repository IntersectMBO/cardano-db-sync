{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Cardano.BM.Data.Tracer (ToLogObject (..))
import Cardano.BM.Trace (Trace, appendName, logInfo)
import qualified Cardano.BM.Trace as Logging
import Cardano.Client.Subscription (Decision (..), MuxTrace, SubscriptionParams (..), SubscriptionTrace, SubscriptionTracers (..), subscribe)
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), LedgerEnv (..), SyncEnv (..), envLedgerEnv, envNetworkMagic, envOptions)
import Cardano.DbSync.Config
import Cardano.DbSync.DbEvent
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Metrics
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (Meta, Nat, (%))
import Cardano.Slotting.Slot (WithOrigin (..))
import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Tracer (Tracer)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Network.Mux as Mux
import Network.TypedProtocol.Peer (N (..), Nat (..))

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
import Ouroboros.Network.Driver.Simple (runPipelinedPeer)
import Ouroboros.Network.Mux (MiniProtocolCb (..), RunMiniProtocol (..), RunMiniProtocolWithMinimalCtx)
import qualified Ouroboros.Network.Mux as Mux
import Ouroboros.Network.NodeToClient (
  ConnectionId,
  Handshake,
  IOManager,
  LocalAddress,
  NodeToClientProtocols (..),
  TraceSendRecv,
  localSnocket,
  localStateQueryPeerNull,
  localTxMonitorPeerNull,
  localTxSubmissionPeerNull,
 )
import qualified Ouroboros.Network.NodeToClient.Version as Network
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
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.LocalStateQuery.Client (localStateQueryClientPeer)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Ouroboros.Network.Snocket as Snocket

runSyncNodeClient ::
  MetricSetters ->
  SyncEnv ->
  IOManager ->
  Trace IO Text ->
  ThreadChannels ->
  SocketPath ->
  IO ()
runSyncNodeClient metricsSetters syncEnv iomgr trce tc (SocketPath socketPath) = do
  logInfo trce $ "Connecting to node via " <> textShow socketPath
  void $
    subscribe
      (localSnocket iomgr)
      (envNetworkMagic syncEnv)
      (supportedNodeToClientVersions (Proxy @CardanoBlock))
      subscriptionTracers
      subscriptionParams
      (dbSyncProtocols syncEnv metricsSetters tc codecConfig)
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
            Left e ->
              case fromException e of
                Just AsyncCancelled -> Abort
                _other -> Reconnect
            Right _ -> Reconnect
        }

    subscriptionTracers =
      SubscriptionTracers
        { stMuxTracer = muxTracer
        , stHandshakeTracer = handshakeTracer
        , stSubscriptionTracer = subscriptionTracer
        , stMuxChannelTracer = Logging.nullTracer
        , stMuxBearerTracer = Logging.nullTracer
        }

    muxTracer :: Tracer IO (Mux.WithBearer (ConnectionId LocalAddress) MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    subscriptionTracer :: Tracer IO (SubscriptionTrace ())
    subscriptionTracer = toLogObject $ appendName "Subscription" trce

    handshakeTracer ::
      Tracer
        IO
        ( Mux.WithBearer
            (ConnectionId LocalAddress)
            (TraceSendRecv (Handshake Network.NodeToClientVersion CBOR.Term))
        )
    handshakeTracer = toLogObject $ appendName "Handshake" trce

dbSyncProtocols ::
  SyncEnv ->
  MetricSetters ->
  ThreadChannels ->
  CodecConfig CardanoBlock ->
  Network.NodeToClientVersion ->
  BlockNodeToClientVersion CardanoBlock ->
  NodeToClientProtocols 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
dbSyncProtocols syncEnv metricsSetters tc codecConfig version bversion =
  NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncPtcl
    , localTxSubmissionProtocol = dummylocalTxSubmit
    , localStateQueryProtocol = localStateQuery
    , localTxMonitorProtocol =
        InitiatorProtocolOnly $
          Mux.mkMiniProtocolCbFromPeer $
            const
              (Logging.nullTracer, cTxMonitorCodec codecs, localTxMonitorPeerNull)
    }
  where
    codecs = clientCodecs codecConfig bversion version

    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync CardanoBlock (Point CardanoBlock) (Tip CardanoBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" tracer

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    localChainSyncPtcl :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $
      MiniProtocolCb $ \_ctx channel ->
        liftIO . logException tracer "ChainSyncWithBlocksPtcl: " $ do
          logInfo tracer "Starting ChainSync client"
          setConsistentLevel syncEnv Unchecked

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
              localChainSyncTracer
              (cChainSyncCodec codecs)
              channel
              ( chainSyncClientPeerPipelined $
                  chainSyncClient metricsSetters tracer (fst <$> latestPoints) currentTip tc
              )
          atomically $ writeDbEventQueue tc DbFinish
          -- We should return leftover bytes returned by 'runPipelinedPeer', but
          -- client application do not care about them (it's only important if one
          -- would like to restart a protocol on the same mux and thus bearer).
          pure ((), Nothing)

    dummylocalTxSubmit :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    dummylocalTxSubmit =
      InitiatorProtocolOnly $
        Mux.mkMiniProtocolCbFromPeer $
          const
            ( Logging.nullTracer
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
                ( Logging.nullTracer
                , cStateQueryCodec codecs
                , LocalStateQuery.StateIdle
                , localStateQueryPeerNull
                )
        NoLedger nle ->
          InitiatorProtocolOnly $
            Mux.mkMiniProtocolCbFromPeerSt $
              const
                ( contramap (Text.pack . show) . toLogObject $ appendName "local-state-query" tracer
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
  Trace IO Text ->
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
        (Just points, _, _) ->
          -- When re-intersecting after rollback failure, reset clientTip to Origin
          -- if falling back to genesis, otherwise keep current clientTip
          let newClientTip = if points == [genesisPoint] then Origin else clientTip
           in drainThePipe n $ clientPipelinedStIdle newClientTip points
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
