{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync
  ( ConfigFile (..)
  , DbSyncNodeParams (..)
  , DbSyncNodePlugin (..)
  , GenesisFile (..)
  , GenesisHash (..)
  , NetworkName (..)
  , SocketPath (..)

  , defDbSyncNodePlugin
  , runDbSyncNode
  ) where

import           Cardano.Binary (unAnnotated)

import           Control.Tracer (Tracer)

import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName, logInfo)
import qualified Cardano.BM.Trace as Logging

import           Cardano.Client.Subscription (subscribe)
import qualified Cardano.Crypto as Crypto

import           Cardano.Db (LogFileDir (..))
import qualified Cardano.Db as DB
import           Cardano.DbSync.Config
import           Cardano.DbSync.Database
import           Cardano.DbSync.Era
import           Cardano.DbSync.Error
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin (DbSyncNodePlugin (..))
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Default.Rollback (unsafeRollback)
import           Cardano.DbSync.Tracing.ToObjectOrphans ()
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude hiding (atomically, option, (%), Nat)

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import qualified Codec.CBOR.Term as CBOR
import           Control.Monad.Class.MonadSTM.Strict (atomically)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Exit (orDie)

import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Network.Socket (SockAddr (..))
import           Network.Mux (MuxTrace, WithMuxBearer)
import           Network.Mux.Types (MuxMode (..))

import           Ouroboros.Network.Driver.Simple (runPipelinedPeer)
import           Network.TypedProtocol.Pipelined (Nat(Zero, Succ))

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs,
                    cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Network.NodeToClient.Version as Network

import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Point (..),
                    Tip, genesisPoint, getTipBlockNo, blockNo)
import           Ouroboros.Network.Mux (MuxPeer (..),  RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (IOManager, ClientSubscriptionParams (..),
                    ConnectionId, ErrorPolicyTrace (..), Handshake, LocalAddress,
                    NetworkSubscriptionTracers (..), NodeToClientProtocols (..),
                    TraceSendRecv, WithAddr (..), localSnocket, localStateQueryPeerNull,
                    localTxSubmissionPeerNull, networkErrorPolicies, withIOManager)

import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Network.Point (withOrigin)

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ChainSyncClientPipelined (..),
                    ClientPipelinedStIdle (..), ClientPipelinedStIntersect (..), ClientStNext (..),
                    chainSyncClientPeerPipelined, recvMsgIntersectFound, recvMsgIntersectNotFound,
                    recvMsgRollBackward, recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionLowHighMark,
                        PipelineDecision (..), runPipelineDecision, MkPipelineDecision)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription (SubscriptionTrace)

import           Prelude (String)
import qualified Prelude

import qualified Shelley.Spec.Ledger.Genesis as Shelley

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show


runDbSyncNode :: DbSyncNodePlugin -> DbSyncNodeParams -> IO ()
runDbSyncNode plugin enp =
  withIOManager $ \ iomgr -> do
    DB.runMigrations Prelude.id True (enpMigrationDir enp) (Just $ LogFileDir "/tmp")

    enc <- readDbSyncNodeConfig (unConfigFile $ enpConfigFile enp)

    trce <- if not (encEnableLogging enc)
              then pure Logging.nullTracer
              else liftIO $ Logging.setupTrace (Right $ encLoggingConfig enc) "db-sync-node"

    -- For testing and debugging.
    case enpMaybeRollback enp of
      Just slotNo -> void $ unsafeRollback trce slotNo
      Nothing -> pure ()

    orDie renderDbSyncNodeError $ do
      genCfg <- readGenesisConfig enp enc
      logProtocolMagic trce $ genesisProtocolMagic genCfg

      -- If the DB is empty it will be inserted, otherwise it will be validated (to make
      -- sure we are on the right chain).
      insertValidateGenesisDist trce (encNetworkName enc) genCfg

      liftIO $ do
        -- Must run plugin startup after the genesis distribution has been inserted/validate.
        runDbStartup trce plugin
        case genCfg of
          GenesisByron bCfg ->
            runDbSyncNodeNodeClient ByronEnv
                iomgr trce plugin (mkByronTopLevelConfig bCfg) (enpSocketPath enp)
          GenesisShelley sCfg ->
            runDbSyncNodeNodeClient (ShelleyEnv $ Shelley.sgNetworkId sCfg)
                iomgr trce plugin (mkShelleyTopLevelConfig sCfg) (enpSocketPath enp)

-- -------------------------------------------------------------------------------------------------

runDbSyncNodeNodeClient
    :: forall blk. (MkDbAction blk, RunNode blk)
    => DbSyncEnv -> IOManager -> Trace IO Text -> DbSyncNodePlugin -> TopLevelConfig blk -> SocketPath
    -> IO ()
runDbSyncNodeNodeClient env iomgr trce plugin topLevelConfig (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $ subscribe
    (localSnocket iomgr socketPath)
    (configCodec topLevelConfig)
    (getNetworkMagic $ configBlock topLevelConfig)
    networkSubscriptionTracers
    clientSubscriptionParams
    (dbSyncProtocols trce env plugin topLevelConfig)
  where
    clientSubscriptionParams = ClientSubscriptionParams {
        cspAddress = Snocket.localAddressFromPath socketPath,
        cspConnectionAttemptDelay = Nothing,
        cspErrorPolicies = networkErrorPolicies <> consensusErrorPolicy
        }

    networkSubscriptionTracers = NetworkSubscriptionTracers {
        nsMuxTracer = muxTracer,
        nsHandshakeTracer = handshakeTracer,
        nsErrorPolicyTracer = errorPolicyTracer,
        nsSubscriptionTracer = subscriptionTracer
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
  :: forall blk. (MkDbAction blk, RunNode blk)
  => Trace IO Text
  -> DbSyncEnv
  -> DbSyncNodePlugin
  -> TopLevelConfig blk
  -> Network.NodeToClientVersion
  -> ClientCodecs blk IO
  -> ConnectionId LocalAddress
  -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce env plugin _topLevelConfig _version codecs _connectionId =
    NodeToClientProtocols {
          localChainSyncProtocol = localChainSyncProtocol
        , localTxSubmissionProtocol = dummylocalTxSubmit
        , localStateQueryProtocol = dummyLocalQueryProtocol
        }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync blk (Tip blk)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

    localChainSyncProtocol :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncProtocol = InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
      liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
        logInfo trce "Starting chainSyncClient"
        latestPoints <- getLatestPoints
        currentTip <- getCurrentTipBlockNo
        logDbState trce
        actionQueue <- newDbActionQueue
        (metrics, server) <- registerMetricsServer
        race_
            (runDbThread trce env plugin metrics actionQueue)
            (runPipelinedPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                (chainSyncClientPeerPipelined
                    $ chainSyncClient trce metrics latestPoints currentTip actionQueue)
            )
        atomically $ writeDbActionQueue actionQueue DbFinish
        cancel server
        -- We should return leftover bytes returned by 'runPipelinedPeer', but
        -- client application do not care about them (it's only important if one
        -- would like to restart a protocol on the same mux and thus bearer).
        pure ((), Nothing)

    dummylocalTxSubmit :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    dummylocalTxSubmit = InitiatorProtocolOnly $ MuxPeer
        Logging.nullTracer
        (cTxSubmissionCodec codecs)
        localTxSubmissionPeerNull

    dummyLocalQueryProtocol :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    dummyLocalQueryProtocol = InitiatorProtocolOnly $ MuxPeer
        Logging.nullTracer
        (cStateQueryCodec codecs)
        localStateQueryPeerNull

logDbState :: Trace IO Text -> IO ()
logDbState trce = do
    mblk <- DB.runDbNoLogging DB.queryLatestBlock
    case mblk of
      Nothing -> logInfo trce "Cardano.Db is empty"
      Just block ->
          logInfo trce $ Text.concat
                  [ "Cardano.Db tip is at "
                  , Text.pack (showTip block)
                  ]
  where
    showTip :: DB.Block -> String
    showTip blk =
      case (DB.blockSlotNo blk, DB.blockBlockNo blk) of
        (Just slotNo, Just blkNo) -> "slot " ++ show slotNo ++ ", block " ++ show blkNo
        (Just slotNo, Nothing) -> "slot " ++ show slotNo
        (Nothing, Just blkNo) -> "block " ++ show blkNo
        (Nothing, Nothing) -> "empty (genesis)"


getLatestPoints :: forall blk. ConvertRawHash blk => IO [Point blk]
getLatestPoints =
    -- Blocks (and the transactions they contain) are inserted within an SQL transaction.
    -- That means that all the blocks (including their transactions) returned by the query
    -- have been completely inserted.
    mapMaybe convert <$> DB.runDbNoLogging (DB.queryCheckPoints 200)
  where
    convert :: (Word64, ByteString) -> Maybe (Point blk)
    convert (slot, hashBlob) =
      fmap (Point . Point.block (SlotNo slot)) (convertHashBlob hashBlob)

    -- in Maybe because the bytestring may not be the right size.
    convertHashBlob :: ByteString -> Maybe (HeaderHash blk)
    convertHashBlob = Just . fromRawHash (Proxy @blk)

getCurrentTipBlockNo :: IO (WithOrigin BlockNo)
getCurrentTipBlockNo = do
    maybeTip <- DB.runDbNoLogging DB.queryLatestBlock
    case maybeTip of
      Just tip -> pure $ convert tip
      Nothing -> pure Origin
  where
    convert :: DB.Block -> WithOrigin BlockNo
    convert blk =
      case DB.blockBlockNo blk of
        Just blockno -> At (BlockNo blockno)
        Nothing -> Origin

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
chainSyncClient
  :: forall blk m. (MonadTimer m, MonadIO m, RunNode blk, MkDbAction blk)
  => Trace IO Text -> Metrics -> [Point blk] -> WithOrigin BlockNo -> DbActionQueue -> ChainSyncClientPipelined blk (Tip blk) m ()
chainSyncClient trce metrics latestPoints currentTip actionQueue =
    ChainSyncClientPipelined $ pure $
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null latestPoints then [genesisPoint] else latestPoints)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound    = \_hdr tip -> pure $ go policy Zero currentTip (getTipBlockNo tip)
          , recvMsgIntersectNotFound = \  tip -> pure $ go policy Zero currentTip (getTipBlockNo tip)
          }
  where
    policy = pipelineDecisionLowHighMark 1000 10000

    go :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo
        -> ClientPipelinedStIdle n blk (Tip blk) m ()
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
            (Just $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip)
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (getTipBlockNo newServerTip))
        (Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (getTipBlockNo newServerTip))

    mkClientStNext :: (WithOrigin BlockNo -> Tip blk -> ClientPipelinedStIdle n blk (Tip blk) m a)
                    -> ClientStNext n blk (Tip blk) m a
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
            liftIO .
              logException trce "recvMsgRollForward: " $ do
                Gauge.set (withOrigin 0 (fromIntegral . unBlockNo) (getTipBlockNo tip))
                          (mNodeHeight metrics)
                newSize <- atomically $ do
                  writeDbActionQueue actionQueue $ mkDbApply blk tip
                  lengthDbActionQueue actionQueue
                Gauge.set (fromIntegral newSize) $ mQueuePostWrite metrics
                pure $ finish (At (blockNo blk)) tip
        , recvMsgRollBackward = \point tip ->
            liftIO .
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                atomically $ writeDbActionQueue actionQueue $ mkDbRollback point
                newTip <- getCurrentTipBlockNo
                pure $ finish newTip tip
        }

logProtocolMagic :: Trace IO Text -> Crypto.ProtocolMagic -> ExceptT DbSyncNodeError IO ()
logProtocolMagic tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.getRequiresNetworkMagic pm), " "
    , textShow (Crypto.unProtocolMagicId . unAnnotated $ Crypto.getAProtocolMagicId pm)
    ]
