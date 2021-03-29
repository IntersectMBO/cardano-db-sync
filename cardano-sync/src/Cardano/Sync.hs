{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncNodePlugin (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)

  , SyncDataLayer (..)
  , MetricSetters (..)
  , nullMetricSetters
  , Block (..)
  , SyncEnv (..)

  , configureLogging
  , runSyncNode
  ) where

import           Cardano.Prelude hiding (Meta, Nat, option, (%))

import           Control.Tracer (Tracer)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName, logError, logInfo)
import qualified Cardano.BM.Trace as Logging

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Client.Subscription (subscribe)
import qualified Cardano.Crypto as Crypto

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import           Cardano.Sync.Api
import           Cardano.Sync.Config
import           Cardano.Sync.Database (DbAction (..), DbActionQueue, lengthDbActionQueue,
                   mkDbApply, mkDbRollback, newDbActionQueue, runDbStartup, writeDbActionQueue)
import           Cardano.Sync.Error
import           Cardano.Sync.Metrics
import           Cardano.Sync.Plugin (SyncNodePlugin (..))
import           Cardano.Sync.Tracing.ToObjectOrphans ()
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Trans.Except.Exit (orDie)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import           Network.Mux (MuxTrace, WithMuxBearer)
import           Network.Mux.Types (MuxMode (..))

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero))
import           Ouroboros.Network.Driver.Simple (runPipelinedPeer)

import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.Byron.Ledger.Config (mkByronCodecConfig)
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block (CodecConfig (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, cChainSyncCodec,
                   cStateQueryCodec, cTxSubmissionCodec)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (ShelleyCodecConfig))

import           Ouroboros.Network.Block (BlockNo (..), Point (..), Tip (..), blockNo, genesisPoint,
                   getTipBlockNo)
import           Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..), ConnectionId,
                   ErrorPolicyTrace (..), Handshake, IOManager, LocalAddress,
                   NetworkSubscriptionTracers (..), NodeToClientProtocols (..), TraceSendRecv,
                   WithAddr (..), localSnocket, localStateQueryPeerNull, localTxSubmissionPeerNull,
                   networkErrorPolicies, withIOManager)
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


type InsertValidateGenesisFunction
    = Trace IO Text
    -> NetworkName
    -> GenesisConfig
    -> ExceptT SyncNodeError IO ()

type RunDBThreadFunction
    =  Trace IO Text
    -> SyncEnv
    -> MetricSetters
    -> SyncNodePlugin
    -> DbActionQueue
    -> IO ()

runSyncNode
    :: SyncDataLayer
    -> MetricSetters
    -> Trace IO Text
    -> SyncNodePlugin
    -> SyncNodeParams
    -> InsertValidateGenesisFunction
    -> RunDBThreadFunction
    -> IO ()
runSyncNode dataLayer metricsSetters trce plugin enp insertValidateGenesisDist runDBThreadFunction =
  withIOManager $ \iomgr -> do

    let configFile = enpConfigFile enp
    enc <- readSyncNodeConfig configFile

    createDirectoryIfMissing True (unLedgerStateDir $ enpLedgerStateDir enp)

    logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile enc)
    logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile enc)

    orDie renderSyncNodeError $ do
      genCfg <- readCardanoGenesisConfig enc
      logProtocolMagicId trce $ genesisProtocolMagicId genCfg

      -- If the DB is empty it will be inserted, otherwise it will be validated (to make
      -- sure we are on the right chain).
      insertValidateGenesisDist trce (dncNetworkName enc) genCfg

        -- Must run plugin startup after the genesis distribution has been inserted/validate.
      liftIO $ runDbStartup trce plugin
      case genCfg of
          GenesisCardano _ bCfg _sCfg -> do
            syncEnv <- ExceptT $ mkSyncEnvFromConfig dataLayer (enpLedgerStateDir enp) genCfg
            liftIO $ runSyncNodeNodeClient metricsSetters syncEnv iomgr trce plugin
                        runDBThreadFunction (cardanoCodecConfig bCfg) (enpSocketPath enp)
  where
    cardanoCodecConfig :: Byron.Config -> CodecConfig CardanoBlock
    cardanoCodecConfig cfg =
      CardanoCodecConfig
        (mkByronCodecConfig cfg)
        ShelleyCodecConfig
        ShelleyCodecConfig -- Allegra
        ShelleyCodecConfig -- Mary

-- -------------------------------------------------------------------------------------------------

runSyncNodeNodeClient
    :: MetricSetters
    -> SyncEnv
    -> IOManager
    -> Trace IO Text
    -> SyncNodePlugin
    -> RunDBThreadFunction
    -> CodecConfig CardanoBlock
    -> SocketPath
    -> IO ()
runSyncNodeNodeClient metricsSetters env iomgr trce plugin runDBThreadFunction codecConfig (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $ subscribe
    (localSnocket iomgr socketPath)
    codecConfig
    (envNetworkMagic env)
    networkSubscriptionTracers
    clientSubscriptionParams
    (dbSyncProtocols trce env metricsSetters plugin runDBThreadFunction)
  where
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

    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    subscriptionTracer :: Tracer IO (Identity (SubscriptionTrace LocalAddress))
    subscriptionTracer = toLogObject $ appendName "Subscription" trce

    handshakeTracer :: Tracer IO (WithMuxBearer
                          (ConnectionId LocalAddress)
                          (TraceSendRecv (Handshake Network.NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

dbSyncProtocols
    :: Trace IO Text
    -> SyncEnv
    -> MetricSetters
    -> SyncNodePlugin
    -> RunDBThreadFunction
    -> Network.NodeToClientVersion
    -> ClientCodecs CardanoBlock IO
    -> ConnectionId LocalAddress
    -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce env metricsSetters plugin runDBThreadFunction version codecs _connectionId =
    NodeToClientProtocols
      { localChainSyncProtocol = localChainSyncPtcl
      , localTxSubmissionProtocol = dummylocalTxSubmit
      , localStateQueryProtocol = localStateQuery
      }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync CardanoBlock(Point CardanoBlock) (Tip CardanoBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

    localChainSyncPtcl :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
      liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
        logInfo trce "Starting chainSyncClient"

        when (version < minVersion) $ do
          logError trce versionErrorMsg
          throwIO $ ErrorCall (Text.unpack versionErrorMsg)

        latestPoints <- getLatestPoints env
        currentTip <- getCurrentTipBlockNo (envDataLayer env)
        logDbState (envDataLayer env) trce
        actionQueue <- newDbActionQueue

        race_
            (runDBThreadFunction trce env metricsSetters plugin actionQueue)
            (runPipelinedPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                (chainSyncClientPeerPipelined
                    $ chainSyncClient (envDataLayer env) metricsSetters trce latestPoints currentTip actionQueue)
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

    versionErrorMsg :: Text
    versionErrorMsg = Text.concat
        [ "The cardano-node version is too old. Please upgrade to a compatible "
        , "cardano-node version. The db-sync requires a node that supports "
        , textShow minVersion
        , ", the one in use only supports "
        , textShow version
        ]

    minVersion :: Network.NodeToClientVersion
    minVersion = Network.NodeToClientV_8

logDbState :: SyncDataLayer -> Trace IO Text -> IO ()
logDbState dataLayer trce = do
    let getLatestBlock = sdlGetLatestBlock dataLayer
    mblk <- getLatestBlock
    case mblk of
      Nothing -> logInfo trce "Cardano.Db is empty"
      Just block ->
          logInfo trce $ Text.concat
                  [ "Cardano.Db tip is at "
                  , showTip block
                  ]
  where
    showTip :: Block -> Text
    showTip blk =
      mconcat
        [ "slot ", textShow (unSlotNo $ bSlotNo blk)
        , ", block ", textShow (unBlockNo $ bBlockNo blk)
        ]

getCurrentTipBlockNo :: SyncDataLayer -> IO (WithOrigin BlockNo)
getCurrentTipBlockNo dataLayer = do
    maybeTip <- sdlGetLatestBlock dataLayer
    case maybeTip of
      Just tip -> pure $ At (bBlockNo tip)
      Nothing -> pure Origin

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
chainSyncClient
    :: SyncDataLayer
    -> MetricSetters
    -> Trace IO Text
    -> [Point CardanoBlock]
    -> WithOrigin BlockNo
    -> DbActionQueue
    -> ChainSyncClientPipelined CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClient dataLayer metricsSetters trce latestPoints currentTip actionQueue = do
    ChainSyncClientPipelined $ pure $
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null latestPoints then [genesisPoint] else latestPoints)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \ _hdr tip -> pure $ go policy Zero currentTip (getTipBlockNo tip)
          , recvMsgIntersectNotFound = pure . go policy Zero currentTip . getTipBlockNo
          }
  where
    policy = pipelineDecisionLowHighMark 1000 10000

    go :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo
        -> ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
    go mkPipelineDecision n clientTip serverTip =
      case (n, runPipelineDecision mkPipelineDecision n clientTip serverTip) of
        (_Zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext clientStNext (pure clientStNext)
          where
            clientStNext = mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n clientBlockNo (getTipBlockNo newServerTip)
        (_, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (go mkPipelineDecision' (Succ n) clientTip serverTip)
        (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip)
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (getTipBlockNo newServerTip))
        (Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (getTipBlockNo newServerTip))

    mkClientStNext :: (WithOrigin BlockNo -> Tip CardanoBlock
                    -> ClientPipelinedStIdle n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO a)
                    -> ClientStNext n CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO a
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
              logException trce "recvMsgRollForward: " $ do

                setNodeBlockHeight metricsSetters (getTipBlockNo tip)

                newSize <- atomically $ do
                                writeDbActionQueue actionQueue $ mkDbApply blk
                                lengthDbActionQueue actionQueue

                setDbQueueLength metricsSetters newSize

                pure $ finish (At (blockNo blk)) tip
        , recvMsgRollBackward = \point tip ->
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                atomically $ writeDbActionQueue actionQueue (mkDbRollback point)
                newTip <- getCurrentTipBlockNo dataLayer
                pure $ finish newTip tip
        }

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.unProtocolMagicId pm)
    ]
