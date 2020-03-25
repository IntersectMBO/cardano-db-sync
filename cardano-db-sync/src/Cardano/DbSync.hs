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
import           Cardano.BM.Trace (Trace, appendName, logError, logInfo)
import qualified Cardano.BM.Trace as Logging

import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import           Cardano.Crypto (decodeAbstractHash)
import           Cardano.Crypto.Hashing (AbstractHash (..))
import qualified Cardano.Crypto as Crypto

import           Cardano.Prelude hiding (atomically, option, (%), Nat)
import           Cardano.Shell.Lib (GeneralException (ConfigurationError))

import           Cardano.Slotting.Slot (WithOrigin (..))

import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as Serialise

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, StrictTMVar,
                    atomically, newEmptyTMVarM, readTMVar)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IO.Class (liftIO)

import           Crypto.Hash (digestFromByteString)

import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Cardano.Db (LogFileDir (..), MigrationDir)
import qualified Cardano.Db as DB
import           Cardano.DbSync.Config
import           Cardano.DbSync.Database
import           Cardano.DbSync.Error
import           Cardano.DbSync.Genesis
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin (DbSyncNodePlugin (..))
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Util
import           Cardano.DbSync.Tracing.ToObjectOrphans ()

import           Network.Socket (SockAddr (..))
import           Network.Mux (MuxTrace, WithMuxBearer)

import           Ouroboros.Network.Driver.Simple (runPipelinedPeer)
import           Network.TypedProtocol.Pipelined (Nat(Zero, Succ))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..), ByronHash (..), GenTx)
import           Ouroboros.Consensus.Cardano (Protocol (..), protocolInfo)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Ouroboros.Consensus.Node.Run (RunNode, nodeDecodeBlock, nodeDecodeGenTx,
                    nodeDecodeHeaderHash, nodeEncodeBlock, nodeEncodeGenTx, nodeEncodeHeaderHash,
                    nodeNetworkMagic)

import           Ouroboros.Network.Point (withOrigin)
import           Ouroboros.Network.Block (BlockNo (..), Point (..), SlotNo (..), Tip,
                    decodePoint, encodePoint, genesisPoint, getTipBlockNo,
                    blockNo, encodeTip, decodeTip, wrapCBORinCBOR, unwrapCBORinCBOR)
import           Ouroboros.Network.Codec (Codec (..), DeserialiseFailure)
import           Ouroboros.Network.Mux (AppType (..), MuxPeer (..), OuroborosApplication,
                   RunMiniProtocol (..))

import           Ouroboros.Network.NodeToClient (IOManager, ClientSubscriptionParams (..),
                    ConnectionId, ErrorPolicyTrace (..), Handshake, LocalAddress,
                    NetworkSubscriptionTracers (..), NodeToClientProtocols (..),
                    NodeToClientVersion, NodeToClientVersionData (..),
                    TraceSendRecv, WithAddr (..), nodeToClientProtocols, ncSubscriptionWorker_V1,
                    networkErrorPolicies, newNetworkMutableState, withIOManager, localSnocket)

import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ChainSyncClientPipelined (..),
                    ClientPipelinedStIdle (..), ClientPipelinedStIntersect (..), ClientStNext (..),
                    chainSyncClientPeerPipelined, recvMsgIntersectFound, recvMsgIntersectNotFound,
                    recvMsgRollBackward, recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionLowHighMark,
                        PipelineDecision (..), runPipelineDecision, MkPipelineDecision)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (LocalTxSubmissionClient (..),
                    LocalTxClientStIdle (..), localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription (SubscriptionTrace)

import           Prelude (String)
import qualified Prelude

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpGenesisFile :: !GenesisFile
  , enpSocketPath :: !SocketPath
  , enpMigrationDir :: !MigrationDir
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }


runDbSyncNode :: DbSyncNodePlugin -> DbSyncNodeParams -> IO ()
runDbSyncNode plugin enp =
  withIOManager $ \ iomgr -> do
    DB.runMigrations Prelude.id True (enpMigrationDir enp) (LogFileDir "/tmp")

    enc <- readDbSyncNodeConfig (unConfigFile $ enpConfigFile enp)

    trce <- mkTracer enc

    gc <- readGenesisConfig enp enc
    logProtocolMagic trce $ Ledger.configProtocolMagic gc

    -- If the DB is empty it will be inserted, otherwise it will be validated (to make
    -- sure we are on the right chain).
    res <- insertValidateGenesisDistribution trce (unNetworkName $ encNetworkName enc) gc
    case res of
      Left err -> logError trce $ renderDbSyncNodeError err
      Right () -> pure ()

    runDbStartup trce plugin
    void $ runDbSyncNodeNodeClient iomgr trce plugin (mkConsensusConfig gc) (enpSocketPath enp)


mkTracer :: DbSyncNodeConfig -> IO (Trace IO Text)
mkTracer enc =
  if not (encEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ encLoggingConfig enc) "db-sync-node"


mkConsensusConfig :: Genesis.Config -> TopLevelConfig ByronBlock
mkConsensusConfig gc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT gc Nothing (Update.ProtocolVersion 0 2 0)
      (Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1) Nothing

readGenesisConfig :: DbSyncNodeParams -> DbSyncNodeConfig -> IO Genesis.Config
readGenesisConfig enp enc = do
    genHash <- either (throwIO . ConfigurationError) pure $
                decodeAbstractHash (unGenesisHash $ encGenesisHash enc)
    convert =<< runExceptT (Genesis.mkConfigFromFile (encRequiresNetworkMagic enc)
                            (unGenesisFile $ enpGenesisFile enp) genHash)
  where
    convert :: Either Genesis.ConfigurationError Genesis.Config -> IO Genesis.Config
    convert =
      \case
        Left err -> panic $ show err
        Right x -> pure x

runDbSyncNodeNodeClient
    :: forall blk.
        (blk ~ ByronBlock)
    => IOManager -> Trace IO Text -> DbSyncNodePlugin -> TopLevelConfig ByronBlock -> SocketPath
    -> IO Void
runDbSyncNodeNodeClient iomgr trce plugin nodeConfig (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  networkState <- newNetworkMutableState
  txv <- newEmptyTMVarM @_ @(GenTx blk)
  ncSubscriptionWorker_V1
    (localSnocket iomgr socketPath)
    -- TODO: these tracers should be configurable for debugging purposes.
    NetworkSubscriptionTracers {
        nsMuxTracer = muxTracer,
        nsHandshakeTracer = handshakeTracer,
        nsErrorPolicyTracer = errorPolicyTracer,
        nsSubscriptionTracer = subscriptionTracer
        }
    networkState
    ClientSubscriptionParams {
        cspAddress = Snocket.localAddressFromPath socketPath,
        cspConnectionAttemptDelay = Nothing,
        cspErrorPolicies = networkErrorPolicies <> consensusErrorPolicy (Proxy @blk)
        }

    (NodeToClientVersionData { networkMagic = nodeNetworkMagic (Proxy @blk) nodeConfig })
    (localInitiatorNetworkApplication trce plugin nodeConfig txv)
  where
    errorPolicyTracer :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    errorPolicyTracer = toLogObject $ appendName "ErrorPolicy" trce
    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce
    subscriptionTracer :: Tracer IO (Identity (SubscriptionTrace LocalAddress))
    subscriptionTracer = toLogObject $ appendName "Subscription" trce
    handshakeTracer :: Tracer IO (WithMuxBearer
                          (ConnectionId LocalAddress)
                          (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce


localInitiatorNetworkApplication
  :: forall blk.
     (blk ~ ByronBlock)
  -- TODO: the need of a 'Proxy' is an evidence that blk type is not really
  -- needed here.  The wallet client should use some concrete type of block
  -- from 'cardano-chain'.  This should remove the dependency of this module
  -- from 'ouroboros-consensus'.
  => Trace IO Text
  -> DbSyncNodePlugin
  -> TopLevelConfig ByronBlock
  -> StrictTMVar IO (GenTx blk)
  -> ConnectionId LocalAddress
  -> OuroborosApplication 'InitiatorApp BSL.ByteString IO () Void
localInitiatorNetworkApplication trce plugin pInfoConfig txv _ =
    nodeToClientProtocols NodeToClientProtocols
      { localChainSyncProtocol =
          -- local chain sync protocol
          (InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
            liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
              logInfo trce "Starting chainSyncClient"
              latestPoints <- getLatestPoints
              currentTip <- getCurrentTipBlockNo
              logDbState trce
              actionQueue <- newDbActionQueue
              (metrics, server) <- registerMetricsServer
              race_
                (runDbThread trce plugin metrics actionQueue)
                (runPipelinedPeer
                      localChainSyncTracer (localChainSyncCodec @blk pInfoConfig) channel
                      (chainSyncClientPeerPipelined (chainSyncClient trce metrics latestPoints currentTip actionQueue))
                    )
              atomically $
                writeDbActionQueue actionQueue DbFinish
              cancel server)
        , localTxSubmissionProtocol =
            -- local tx submission
            (InitiatorProtocolOnly $ MuxPeer
              (contramap (Text.pack . show) . toLogObject $ appendName "db-sync-local-tx" trce)
              localTxSubmissionCodec
              (localTxSubmissionClientPeer (txSubmissionClient txv)))
        }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

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


getLatestPoints :: IO [Point ByronBlock]
getLatestPoints =
    -- Blocks (and the transactions they contain) are inserted within an SQL transaction.
    -- That means that all the blocks (including their transactions) returned by the query
    -- have been completely inserted.
    mapMaybe convert <$> DB.runDbNoLogging (DB.queryCheckPoints 200)
  where
    convert :: (Word64, ByteString) -> Maybe (Point ByronBlock)
    convert (slot, hashBlob) =
      fmap (Point . Point.block (SlotNo slot)) (convertHashBlob hashBlob)

    -- in Maybe because the bytestring may not be the right size.
    convertHashBlob :: ByteString -> Maybe ByronHash
    convertHashBlob = fmap (ByronHash . AbstractHash) . digestFromByteString

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

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'StrictTMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: forall tx reject m. (Monad m, MonadSTM m)
  => StrictTMVar m tx -> LocalTxSubmissionClient tx reject m ()
txSubmissionClient txv = LocalTxSubmissionClient $
    atomically (readTMVar txv) >>= pure . client
  where
    client :: tx -> LocalTxClientStIdle tx reject m ()
    client tx =
      SendMsgSubmitTx tx $ \mbreject -> do
        case mbreject of
          Nothing -> return ()
          Just _r -> return ()
        tx' <- atomically $ readTMVar txv
        pure $ client tx'

localChainSyncCodec
  :: forall blk m. (blk ~ ByronBlock, MonadST m)
  => TopLevelConfig ByronBlock
  -> Codec (ChainSync blk (Tip blk)) DeserialiseFailure m BSL.ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (wrapCBORinCBOR $ nodeEncodeBlock pInfoConfig)
      (unwrapCBORinCBOR $ nodeDecodeBlock pInfoConfig)
      (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
      (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

localTxSubmissionCodec
  :: (RunNode blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) String) DeserialiseFailure m BSL.ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission nodeEncodeGenTx nodeDecodeGenTx Serialise.encode Serialise.decode

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
chainSyncClient
  :: forall blk m. (MonadTimer m, MonadIO m, blk ~ ByronBlock)
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

    go :: MkPipelineDecision -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo -> ClientPipelinedStIdle n ByronBlock (Tip blk) m a
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

    mkClientStNext :: (WithOrigin BlockNo -> Tip blk -> ClientPipelinedStIdle n ByronBlock (Tip blk) m a)
                    -> ClientStNext n ByronBlock (Tip ByronBlock) m a
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
            liftIO .
              logException trce "recvMsgRollForward: " $ do
                Gauge.set (withOrigin 0 (fromIntegral . unBlockNo) (getTipBlockNo tip))
                          (mNodeHeight metrics)
                newSize <- atomically $ do
                  writeDbActionQueue actionQueue $ DbApplyBlock blk tip
                  lengthDbActionQueue actionQueue
                Gauge.set (fromIntegral newSize) $ mQueuePostWrite metrics
                pure $ finish (At (blockNo blk)) tip
        , recvMsgRollBackward = \point tip -> do
            liftIO .
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                atomically $ writeDbActionQueue actionQueue (DbRollBackToPoint point)
                newTip <- getCurrentTipBlockNo
                pure $ finish newTip tip
        }

logProtocolMagic :: Trace IO Text -> Crypto.ProtocolMagic -> IO ()
logProtocolMagic tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.getRequiresNetworkMagic pm), " "
    , textShow (Crypto.unProtocolMagicId . unAnnotated $ Crypto.getAProtocolMagicId pm)
    ]
