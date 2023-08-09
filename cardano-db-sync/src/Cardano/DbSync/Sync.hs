{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
  nullMetricSetters,
  SyncEnv (..),
  configureLogging,
  runSyncNodeClient,
) where

import Cardano.BM.Data.Tracer (ToLogObject (..), ToObject)
import Cardano.BM.Trace (Trace, appendName, logInfo, logWarning)
import qualified Cardano.BM.Trace as Logging
import Cardano.Client.Subscription (subscribe)
import Cardano.Db (runDbIohkLogging)
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), FixesRan (..), LedgerEnv (..), SyncEnv (..), SyncOptions (..), envConnString, envLedgerEnv, envNetworkMagic, envOptions)
import Cardano.DbSync.Config
import Cardano.DbSync.Database
import Cardano.DbSync.DbAction
import Cardano.DbSync.Fix.PlutusDataBytes
import Cardano.DbSync.Fix.PlutusScripts
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Metrics
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (Meta, Nat, (%))
import Cardano.Slotting.Slot (WithOrigin (..))
import qualified Codec.CBOR.Term as CBOR
import Control.Tracer (Tracer)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import qualified Data.Text as Text
import Database.Persist.Postgresql (SqlBackend)
import Network.Mux (MuxTrace, WithMuxBearer)
import Network.Mux.Types (MuxMode (..))
import Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))
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
import Ouroboros.Consensus.Node.ErrorPolicy
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion, supportedNodeToClientVersions)
import Ouroboros.Network.Block (
  BlockNo (..),
  Point (..),
  Tip (..),
  blockNo,
  genesisPoint,
  getTipBlockNo,
 )
import Ouroboros.Network.Driver (runPeer)
import Ouroboros.Network.Driver.Simple (runPipelinedPeer)
import Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import Ouroboros.Network.NodeToClient (
  ClientSubscriptionParams (..),
  ConnectionId,
  ErrorPolicyTrace (..),
  Handshake,
  IOManager,
  LocalAddress,
  NetworkSubscriptionTracers (..),
  NodeToClientProtocols (..),
  TraceSendRecv,
  WithAddr (..),
  localSnocket,
  localStateQueryPeerNull,
  localTxMonitorPeerNull,
  localTxSubmissionPeerNull,
  networkErrorPolicies,
 )
import qualified Ouroboros.Network.NodeToClient.Version as Network
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient)
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Client
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
import qualified Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Subscription (SubscriptionTrace)

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
      networkSubscriptionTracers
      clientSubscriptionParams
      (dbSyncProtocols syncEnv metricsSetters tc codecConfig)
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec $ getTopLevelConfig syncEnv

    --    nonExperimentalVersions =
    --      filter (\a -> Just a <= snd (lastReleasedNodeVersion Proxy)) $ supportedNodeToClientVersions (Proxy @CardanoBlock)

    clientSubscriptionParams =
      ClientSubscriptionParams
        { cspAddress = Snocket.localAddressFromPath socketPath
        , cspConnectionAttemptDelay = Nothing
        , cspErrorPolicies = networkErrorPolicies <> consensusErrorPolicy (Proxy @CardanoBlock)
        }

    networkSubscriptionTracers =
      NetworkSubscriptionTracers
        { nsMuxTracer = muxTracer
        , nsHandshakeTracer = handshakeTracer
        , nsErrorPolicyTracer = errorPolicyTracer
        , nsSubscriptionTracer = subscriptionTracer
        }

    errorPolicyTracer :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    errorPolicyTracer = toLogObject $ appendName "ErrorPolicy" trce

    muxTracer :: (Show peer, ToObject peer) => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    subscriptionTracer :: Tracer IO (Identity (SubscriptionTrace LocalAddress))
    subscriptionTracer = toLogObject $ appendName "Subscription" trce

    handshakeTracer ::
      Tracer
        IO
        ( WithMuxBearer
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
  ConnectionId LocalAddress ->
  NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols syncEnv metricsSetters tc codecConfig version bversion _connectionId =
  NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncPtcl
    , localTxSubmissionProtocol = dummylocalTxSubmit
    , localStateQueryProtocol = localStateQuery
    , localTxMonitorProtocol =
        InitiatorProtocolOnly $ MuxPeer Logging.nullTracer (cTxMonitorCodec codecs) localTxMonitorPeerNull
    }
  where
    codecs = clientCodecs codecConfig bversion version

    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync CardanoBlock (Point CardanoBlock) (Tip CardanoBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" tracer

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    backend :: SqlBackend
    backend = envBackend syncEnv

    initAction channel = do
      fr <- getIsSyncFixed syncEnv
      let skipFix = soptSkipFix $ envOptions syncEnv
      let onlyFix = soptOnlyFix $ envOptions syncEnv
      if noneFixed fr && (onlyFix || not skipFix)
        then do
          fd <- runDbIohkLogging backend tracer $ getWrongPlutusData tracer
          unless (nullData fd) $
            void $
              runPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                ( Client.chainSyncClientPeer $
                    chainSyncClientFixData backend tracer fd
                )
          if onlyFix
            then do
              setIsFixed syncEnv DataFixRan
            else setIsFixedAndMigrate syncEnv DataFixRan
          pure False
        else
          if isDataFixed fr && (onlyFix || not skipFix)
            then do
              ls <- runDbIohkLogging backend tracer $ getWrongPlutusScripts tracer
              unless (nullPlutusScripts ls) $
                void $
                  runPeer
                    localChainSyncTracer
                    (cChainSyncCodec codecs)
                    channel
                    ( Client.chainSyncClientPeer $
                        chainSyncClientFixScripts backend tracer ls
                    )
              when onlyFix $ panic "All Good! This error is only thrown to exit db-sync"
              setIsFixed syncEnv AllFixRan
              pure False
            else do
              when skipFix $ setIsFixedAndMigrate syncEnv AllFixRan
              pure True

    localChainSyncPtcl :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $
      MuxPeerRaw $ \channel ->
        liftIO . logException tracer "ChainSyncWithBlocksPtcl: " $ do
          isInitComplete <- runAndSetDone tc $ initAction channel
          when isInitComplete $ do
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
            atomically $ writeDbActionQueue tc DbFinish
            -- We should return leftover bytes returned by 'runPipelinedPeer', but
            -- client application do not care about them (it's only important if one
            -- would like to restart a protocol on the same mux and thus bearer).
            pure ()
          pure ((), Nothing)

    dummylocalTxSubmit :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    dummylocalTxSubmit =
      InitiatorProtocolOnly $
        MuxPeer
          Logging.nullTracer
          (cTxSubmissionCodec codecs)
          localTxSubmissionPeerNull

    localStateQuery :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localStateQuery =
      case envLedgerEnv syncEnv of
        HasLedger _ ->
          InitiatorProtocolOnly $
            MuxPeer
              Logging.nullTracer
              (cStateQueryCodec codecs)
              localStateQueryPeerNull
        NoLedger nle ->
          InitiatorProtocolOnly $
            MuxPeer
              (contramap (Text.pack . show) . toLogObject $ appendName "local-state-query" tracer)
              (cStateQueryCodec codecs)
              (localStateQueryClientPeer $ localStateQueryHandler nle)

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
        (Just points, _, _) -> drainThePipe n $ clientPipelinedStIdle clientTip points
        (_, _Zero, (Request, mkPipelineDecision')) ->
          SendMsgRequestNext clientStNext (pure clientStNext)
          where
            clientStNext = mkClientStNext $ goTip mkPipelineDecision' n
        (_, _, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
        (_, Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip Nothing)
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
                writeDbActionQueue tc $ mkDbApply blk
                lengthDbActionQueue tc

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

chainSyncClientFixData ::
  SqlBackend -> Trace IO Text -> FixData -> ChainSyncClient CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClientFixData backend tracer fixData = Client.ChainSyncClient $ do
  liftIO $ logInfo tracer "Starting chainsync to fix Plutus Data. This will update database values in tables datum and redeemer_data."
  clientStIdle True (sizeFixData fixData) fixData
  where
    updateSizeAndLog :: Int -> Int -> IO Int
    updateSizeAndLog lastSize currentSize = do
      let diffSize = lastSize - currentSize
      if lastSize >= currentSize && diffSize >= 200_000
        then do
          liftIO $ logInfo tracer $ mconcat ["Fixed ", textShow (sizeFixData fixData - currentSize), " Plutus Data"]
          pure currentSize
        else pure lastSize

    clientStIdle :: Bool -> Int -> FixData -> IO (Client.ClientStIdle CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ())
    clientStIdle shouldLog lastSize fds = do
      case spanFDOnNextPoint fds of
        Nothing -> do
          liftIO $ logInfo tracer "Finished chainsync to fix Plutus Data."
          pure $ Client.SendMsgDone ()
        Just (point, fdOnPoint, fdRest) -> do
          when shouldLog $
            liftIO $
              logInfo tracer $
                mconcat ["Starting fixing Plutus Data ", textShow point]
          newLastSize <- liftIO $ updateSizeAndLog lastSize (sizeFixData fds)
          let clientStIntersect =
                Client.ClientStIntersect
                  { Client.recvMsgIntersectFound = \_pnt _tip ->
                      Client.ChainSyncClient $
                        pure $
                          Client.SendMsgRequestNext (clientStNext newLastSize fdOnPoint fdRest) (pure $ clientStNext newLastSize fdOnPoint fdRest)
                  , Client.recvMsgIntersectNotFound = \tip -> Client.ChainSyncClient $ do
                      liftIO $
                        logWarning tracer $
                          mconcat
                            [ "Node can't find block "
                            , textShow point
                            , ". It's probably behind, at "
                            , textShow tip
                            , ". Sleeping for 3 mins and retrying.."
                            ]
                      threadDelay $ 180 * 1_000_000
                      pure $ Client.SendMsgFindIntersect [point] clientStIntersect
                  }
          pure $ Client.SendMsgFindIntersect [point] clientStIntersect

    clientStNext :: Int -> FixData -> FixData -> Client.ClientStNext CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    clientStNext lastSize fdOnPoint fdRest =
      Client.ClientStNext
        { Client.recvMsgRollForward = \blk _tip -> Client.ChainSyncClient $ do
            runDbIohkLogging backend tracer $ fixPlutusData tracer blk fdOnPoint
            clientStIdle False lastSize fdRest
        , Client.recvMsgRollBackward = \_point _tip ->
            Client.ChainSyncClient $
              pure $
                Client.SendMsgRequestNext (clientStNext lastSize fdOnPoint fdRest) (pure $ clientStNext lastSize fdOnPoint fdRest)
        }

chainSyncClientFixScripts ::
  SqlBackend -> Trace IO Text -> FixPlutusScripts -> ChainSyncClient CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClientFixScripts backend tracer fps = Client.ChainSyncClient $ do
  liftIO $ logInfo tracer "Starting chainsync to fix Plutus Scripts. This will update database values in tables script."
  clientStIdle True (sizeFixPlutusScripts fps) fps
  where
    updateSizeAndLog :: Int -> Int -> IO Int
    updateSizeAndLog lastSize currentSize = do
      let diffSize = lastSize - currentSize
      if lastSize >= currentSize && diffSize >= 200_000
        then do
          liftIO $ logInfo tracer $ mconcat ["Fixed ", textShow (sizeFixPlutusScripts fps - currentSize), " Plutus Scripts"]
          pure currentSize
        else pure lastSize

    clientStIdle :: Bool -> Int -> FixPlutusScripts -> IO (Client.ClientStIdle CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ())
    clientStIdle shouldLog lastSize fps' = do
      case spanFPSOnNextPoint fps' of
        Nothing -> do
          liftIO $ logInfo tracer "Finished chainsync to fix Plutus Scripts."
          pure $ Client.SendMsgDone ()
        Just (point, fpsOnPoint, fpsRest) -> do
          when shouldLog $
            liftIO $
              logInfo tracer $
                mconcat ["Starting fixing Plutus Scripts ", textShow point]
          newLastSize <- liftIO $ updateSizeAndLog lastSize (sizeFixPlutusScripts fps')
          let clientStIntersect =
                Client.ClientStIntersect
                  { Client.recvMsgIntersectFound = \_pnt _tip ->
                      Client.ChainSyncClient $
                        pure $
                          Client.SendMsgRequestNext (clientStNext newLastSize fpsOnPoint fpsRest) (pure $ clientStNext newLastSize fpsOnPoint fpsRest)
                  , Client.recvMsgIntersectNotFound = \tip -> Client.ChainSyncClient $ do
                      liftIO $
                        logWarning tracer $
                          mconcat
                            [ "Node can't find block "
                            , textShow point
                            , ". It's probably behind, at "
                            , textShow tip
                            , ". Sleeping for 3 mins and retrying.."
                            ]
                      threadDelay $ 180 * 1_000_000
                      pure $ Client.SendMsgFindIntersect [point] clientStIntersect
                  }
          pure $ Client.SendMsgFindIntersect [point] clientStIntersect

    clientStNext :: Int -> FixPlutusScripts -> FixPlutusScripts -> Client.ClientStNext CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    clientStNext lastSize fpsOnPoint fpsRest =
      Client.ClientStNext
        { Client.recvMsgRollForward = \blk _tip -> Client.ChainSyncClient $ do
            runDbIohkLogging backend tracer $ fixPlutusScripts tracer blk fpsOnPoint
            clientStIdle False lastSize fpsRest
        , Client.recvMsgRollBackward = \_point _tip ->
            Client.ChainSyncClient $
              pure $
                Client.SendMsgRequestNext (clientStNext lastSize fpsOnPoint fpsRest) (pure $ clientStNext lastSize fpsOnPoint fpsRest)
        }
