{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Sync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)

  , MetricSetters (..)
  , nullMetricSetters
  , SyncEnv (..)

  , configureLogging
  , runSyncNode
  ) where

import           Cardano.Prelude hiding (Meta, Nat, option, (%))

import           Control.Tracer (Tracer)

import           Cardano.BM.Data.Tracer (ToLogObject (..), ToObject)
import           Cardano.BM.Trace (Trace, appendName, logInfo)
import qualified Cardano.BM.Trace as Logging

import           Cardano.Client.Subscription (subscribe)
import qualified Cardano.Crypto as Crypto

import           Cardano.Slotting.Slot (EpochNo (..), WithOrigin (..))

import           Cardano.DbSync.Api
import           Cardano.DbSync.Config
import           Cardano.DbSync.Database
import           Cardano.DbSync.DbAction
import           Cardano.DbSync.Epoch
import           Cardano.DbSync.Era
import           Cardano.DbSync.Error
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Tracing.ToObjectOrphans ()
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Trans.Except.Exit (orDie)

import qualified Data.ByteString.Lazy as BSL

import           Database.Persist.Sql (SqlBackend)

import           Network.Mux (MuxTrace, WithMuxBearer)
import           Network.Mux.Types (MuxMode (..))

import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))
import           Ouroboros.Network.Driver.Simple (runPipelinedPeer)

import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Config (configCodec)
import qualified Ouroboros.Consensus.HardFork.Simple as HardFork
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, Codecs' (..),
                   cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus

import           Ouroboros.Network.Block (BlockNo (..), Point (..), Tip (..), blockNo, genesisPoint,
                   getTipBlockNo)
import           Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..), ConnectionId,
                   ErrorPolicyTrace (..), Handshake, IOManager, LocalAddress,
                   NetworkSubscriptionTracers (..), NodeToClientProtocols (..), TraceSendRecv,
                   WithAddr (..), localSnocket, localStateQueryPeerNull, localTxMonitorPeerNull,
                   localTxSubmissionPeerNull, networkErrorPolicies)
import qualified Ouroboros.Network.NodeToClient.Version as Network

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (..), ClientPipelinedStIdle (..),
                   ClientPipelinedStIntersect (..), ClientStNext (..), chainSyncClientPeerPipelined,
                   recvMsgIntersectFound, recvMsgIntersectNotFound, recvMsgRollBackward,
                   recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (MkPipelineDecision,
                   PipelineDecision (..), pipelineDecisionLowHighMark, runPipelineDecision)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription (SubscriptionTrace)

import           System.Directory (createDirectoryIfMissing)


runSyncNode
    :: MetricSetters
    -> Trace IO Text
    -> SqlBackend
    -> IOManager
    -> Bool
    -> Word64
    -> Word64
    -> SyncNodeParams
    -> IO ()
runSyncNode metricsSetters trce backend iomgr aop snEveryFollowing snEveryLagging enp = do

    let configFile = enpConfigFile enp
    enc <- readSyncNodeConfig configFile

    createDirectoryIfMissing True (unLedgerStateDir $ enpLedgerStateDir enp)

    logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile enc)
    logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile enc)
    logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile enc)

    orDie renderSyncNodeError $ do
      genCfg <- readCardanoGenesisConfig enc
      logProtocolMagicId trce $ genesisProtocolMagicId genCfg

      -- If the DB is empty it will be inserted, otherwise it will be validated (to make
      -- sure we are on the right chain).
      insertValidateGenesisDist trce backend (dncNetworkName enc) genCfg (useShelleyInit enc)

      case genCfg of
          GenesisCardano {} -> do
            syncEnv <- ExceptT $ mkSyncEnvFromConfig trce backend
              (SyncOptions (enpExtended enp) aop snEveryFollowing snEveryLagging)
              (enpLedgerStateDir enp) genCfg
            liftIO $ epochStartup syncEnv
            liftIO $ runSyncNodeClient metricsSetters syncEnv iomgr trce (enpSocketPath enp)
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        HardFork.TriggerHardForkAtEpoch (EpochNo 0) -> True
        _ -> False

-- -------------------------------------------------------------------------------------------------

runSyncNodeClient
    :: MetricSetters
    -> SyncEnv
    -> IOManager
    -> Trace IO Text
    -> SocketPath
    -> IO ()
runSyncNodeClient metricsSetters env iomgr trce (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $ subscribe
    (localSnocket iomgr)
    codecConfig
    (envNetworkMagic env)
    networkSubscriptionTracers
    clientSubscriptionParams
    (dbSyncProtocols trce env metricsSetters)
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec $ Consensus.pInfoConfig (leProtocolInfo $ envLedger env)

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

    handshakeTracer :: Tracer IO (WithMuxBearer
                          (ConnectionId LocalAddress)
                          (TraceSendRecv (Handshake Network.NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

dbSyncProtocols
    :: Trace IO Text -> SyncEnv -> MetricSetters
    -> Network.NodeToClientVersion -> ClientCodecs CardanoBlock IO
    -> ConnectionId LocalAddress
    -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce env metricsSetters _version codecs _connectionId =
    NodeToClientProtocols
      { localChainSyncProtocol = localChainSyncPtcl
      , localTxSubmissionProtocol = dummylocalTxSubmit
      , localStateQueryProtocol = localStateQuery
      , localTxMonitorProtocol =
          InitiatorProtocolOnly $ MuxPeer Logging.nullTracer (cTxMonitorCodec codecs) localTxMonitorPeerNull
      }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync CardanoBlock(Point CardanoBlock) (Tip CardanoBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

    localChainSyncPtcl :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
      liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
        logInfo trce "Starting chainSyncClient"

        -- The Db thread is not forked at this point, so we can use
        -- the connection here. A connection cannot be used concurrently by many
        -- threads
        latestPoints <- getLatestPoints env
        currentTip <- getCurrentTipBlockNo env
        logDbState env
        -- communication channel between datalayer thread and chainsync-client thread
        actionQueue <- newDbActionQueue

        race_
            (race
                (runDbThread env metricsSetters actionQueue)
                (runOfflineFetchThread trce (envLedger env))
                )
            (runPipelinedPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                (chainSyncClientPeerPipelined
                    $ chainSyncClient metricsSetters trce latestPoints currentTip actionQueue)
            )

        atomically $ writeDbActionQueue actionQueue DbFinish
        -- We should return leftover bytes returned by 'runPipelinedPeer', but
        -- client application do not care about them (it's only important if one
        -- would like to restart a protocol on the same mux and thus bearer).
        pure ((), Nothing)

    dummylocalTxSubmit :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    dummylocalTxSubmit = InitiatorProtocolOnly $ MuxPeer
        Logging.nullTracer
        (cTxSubmissionCodec codecs)
        localTxSubmissionPeerNull

    localStateQuery :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localStateQuery =
      InitiatorProtocolOnly $ MuxPeer
        Logging.nullTracer
        (cStateQueryCodec codecs)
        localStateQueryPeerNull

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
chainSyncClient
    :: MetricSetters
    -> Trace IO Text
    -> [Point CardanoBlock]
    -> WithOrigin BlockNo
    -> DbActionQueue
    -> ChainSyncClientPipelined CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClient metricsSetters trce latestPoints currentTip actionQueue = do
    ChainSyncClientPipelined $ pure $ clientPipelinedStIdle currentTip latestPoints
  where
    clientPipelinedStIdle
        :: WithOrigin BlockNo -> [CardanoPoint]
        -> ClientPipelinedStIdle 'Z CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    clientPipelinedStIdle clintTip points =
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null points then [genesisPoint] else points)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \ _hdr tip -> pure $ go policy Zero clintTip (getTipBlockNo tip) Nothing
          , recvMsgIntersectNotFound = \tip -> pure $ goTip policy Zero clintTip tip Nothing
          }

    policy :: MkPipelineDecision
    policy = pipelineDecisionLowHighMark 1 50

    goTip :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> Tip CardanoBlock -> Maybe [CardanoPoint]
          -> ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    goTip mkPipelineDecision n clientTip serverTip mPoint =
      go mkPipelineDecision n clientTip (getTipBlockNo serverTip) mPoint

    go :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo -> Maybe [CardanoPoint]
        -> ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
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

    mkClientStNext
        :: (WithOrigin BlockNo -> Tip CardanoBlock -> Maybe [CardanoPoint]
        -> ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ())
        -> ClientStNext n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
              logException trce "recvMsgRollForward: " $ do

                setNodeBlockHeight metricsSetters (getTipBlockNo tip)

                newSize <- atomically $ do
                                writeDbActionQueue actionQueue $ mkDbApply blk
                                lengthDbActionQueue actionQueue

                setDbQueueLength metricsSetters newSize

                pure $ finish (At (blockNo blk)) tip Nothing
        , recvMsgRollBackward = \point tip ->
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                (mPoints, newTip) <- waitRollback actionQueue point
                pure $ finish newTip tip mPoints
        }

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.unProtocolMagicId pm)
    ]

drainThePipe
    :: Nat n -> ClientPipelinedStIdle 'Z CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    -> ClientPipelinedStIdle  n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
drainThePipe n0 client = go n0
  where
    go :: forall n'. Nat n'
       -> ClientPipelinedStIdle n' CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    go n =
      case n of
        Zero -> client
        Succ n' ->
          CollectResponse Nothing $
            ClientStNext
              { recvMsgRollForward  = \_hdr _tip -> pure $ go n'
              , recvMsgRollBackward = \_pt  _tip -> pure $ go n'
              }
