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

module Explorer.Node
  ( ConfigFile (..)
  , ExplorerNodeParams (..)
  , ExplorerNodePlugin (..)
  , GenesisFile (..)
  , GenesisHash (..)
  , NetworkName (..)
  , SocketPath (..)

  , defExplorerNodePlugin
  , runExplorer
  ) where

import           Cardano.Binary (unAnnotated)

import           Control.Tracer (Tracer)

import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
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

import           Explorer.DB (LogFileDir (..), MigrationDir)
import qualified Explorer.DB as DB
import           Explorer.Node.Config
import           Explorer.Node.Database
import           Explorer.Node.Error
import           Explorer.Node.Genesis
import           Explorer.Node.Metrics
import           Explorer.Node.Plugin (ExplorerNodePlugin (..))
import           Explorer.Node.Plugin.Default (defExplorerNodePlugin)
import           Explorer.Node.Util
import           Explorer.Node.Tracing.ToObjectOrphans ()

import           Network.Socket (SockAddr (..))

import           Network.TypedProtocol.Codec (Codec)
import           Network.TypedProtocol.Codec.Cbor (DeserialiseFailure)
import           Network.TypedProtocol.Driver (runPeer, runPipelinedPeer)
import           Network.TypedProtocol.Pipelined (Nat(Zero, Succ))

import           Ouroboros.Consensus.Ledger.Abstract (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..), ByronHash (..), GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig, protocolInfo)
import           Ouroboros.Consensus.Node.Run.Abstract (RunNode, nodeDecodeBlock, nodeDecodeGenTx,
                    nodeDecodeHeaderHash, nodeEncodeBlock, nodeEncodeGenTx, nodeEncodeHeaderHash, nodeNetworkMagic)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Protocol (NodeConfig, Protocol (..))

import           Ouroboros.Network.Block (Point (..), SlotNo (..), Tip (tipBlockNo),
                    decodePoint, encodePoint, genesisPoint, genesisBlockNo, blockNo,
                    BlockNo(unBlockNo, BlockNo),
                    encodeTip, decodeTip)
import           Ouroboros.Network.Mux (AppType (..), OuroborosApplication (..))
import           Ouroboros.Network.NodeToClient (ErrorPolicyTrace (..), IPSubscriptionTarget (..),
                    LocalAddresses (..), NodeToClientProtocols (..), NetworkIPSubscriptionTracers (..),
                    NodeToClientVersionData (..), SubscriptionParams (..), WithAddr (..),
                    ncSubscriptionWorker_V1, networkErrorPolicies, newNetworkMutableState)

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

import           Prelude (String)
import qualified Prelude

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data ExplorerNodeParams = ExplorerNodeParams
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


runExplorer :: ExplorerNodePlugin -> ExplorerNodeParams -> IO ()
runExplorer plugin enp = do
    DB.runMigrations Prelude.id True (enpMigrationDir enp) (LogFileDir "/tmp")

    enc <- readExplorerNodeConfig (unConfigFile $ enpConfigFile enp)

    trce <- mkTracer enc

    gc <- readGenesisConfig enp enc
    logProtocolMagic trce $ Ledger.configProtocolMagic gc

    -- If the DB is empty it will be inserted, otherwise it will be validated (to make
    -- sure we are on the right chain).
    res <- insertValidateGenesisDistribution trce (unNetworkName $ encNetworkName enc) gc
    case res of
      Left err -> logError trce $ renderExplorerNodeError err
      Right () -> pure ()

    void $ runExplorerNodeClient trce plugin (mkNodeConfig gc) (enpSocketPath enp)


mkTracer :: ExplorerNodeConfig -> IO (Trace IO Text)
mkTracer enc =
  if not (encEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ encLoggingConfig enc) "explorer-db-node"


mkNodeConfig :: Genesis.Config -> NodeConfig (BlockProtocol ByronBlock)
mkNodeConfig gc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT gc Nothing (Update.ProtocolVersion 0 2 0)
      (Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1) Nothing

readGenesisConfig :: ExplorerNodeParams -> ExplorerNodeConfig -> IO Genesis.Config
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

runExplorerNodeClient
    :: forall blk.
        (blk ~ ByronBlock)
    => Trace IO Text -> ExplorerNodePlugin -> NodeConfig (BlockProtocol blk) -> SocketPath
    -> IO Void
runExplorerNodeClient trce plugin nodeConfig (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  networkState <- newNetworkMutableState
  ncSubscriptionWorker_V1
    -- TODO: these tracers should be configurable for debugging purposes.
    NetworkIPSubscriptionTracers {
        nistMuxTracer = nullTracer,
        nistHandshakeTracer = nullTracer,
        nistErrorPolicyTracer = errorPolicyTracer,
        nistSubscriptionTracer = nullTracer
        -- TODO subscription tracer should not be 'nullTracer' by default
      }
    networkState
    SubscriptionParams {
        spLocalAddresses = LocalAddresses Nothing Nothing (Just $ SockAddrUnix socketPath),
        spConnectionAttemptDelay = const Nothing,
        spErrorPolicies = networkErrorPolicies <> consensusErrorPolicy,
        spSubscriptionTarget = IPSubscriptionTarget
          { ispIps = [SockAddrUnix socketPath]
          , ispValency = 1 }
        }
    (NodeToClientVersionData { networkMagic = nodeNetworkMagic (Proxy @blk) nodeConfig })
    (localInitiatorNetworkApplication trce plugin nodeConfig)
  where
    errorPolicyTracer :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace)
    errorPolicyTracer = toLogObject $ appendName "ErrorPolicy" trce


localInitiatorNetworkApplication
  :: forall blk peer.
     (blk ~ ByronBlock)
  -- TODO: the need of a 'Proxy' is an evidence that blk type is not really
  -- needed here.  The wallet client should use some concrete type of block
  -- from 'cardano-chain'.  This should remove the dependency of this module
  -- from 'ouroboros-consensus'.
  => Trace IO Text
  -> ExplorerNodePlugin
  -> NodeConfig (BlockProtocol ByronBlock)
  -> OuroborosApplication 'InitiatorApp peer NodeToClientProtocols IO BSL.ByteString Void Void
localInitiatorNetworkApplication trce plugin pInfoConfig =
      OuroborosInitiatorApplication $ \ _peer ptcl ->
        case ptcl of
          LocalTxSubmissionPtcl -> \channel -> do
            txv <- newEmptyTMVarM @_ @(GenTx blk)
            runPeer
              (contramap (Text.pack . show) . toLogObject $ appendName "explorer-db-local-tx" trce)
              localTxSubmissionCodec channel
              (localTxSubmissionClientPeer (txSubmissionClient @(GenTx blk) txv))

          ChainSyncWithBlocksPtcl -> \channel ->
            liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
              logInfo trce "Starting chainSyncClient"
              latestPoints <- getLatestPoints
              currentTip <- getCurrentTipBlockNo
              logDbState trce
              actionQueue <- newDbActionQueue
              (metrics, server) <- registerMetricsServer
              dbThread <- async $ runDbThread trce plugin metrics actionQueue
              ret <- runPipelinedPeer
                      nullTracer (localChainSyncCodec @blk pInfoConfig) channel
                      (chainSyncClientPeerPipelined (chainSyncClient trce metrics latestPoints currentTip actionQueue))
              atomically $
                writeDbActionQueue actionQueue DbFinish
              wait dbThread
              cancel server
              pure ret


logDbState :: Trace IO Text -> IO ()
logDbState trce = do
    mblk <- DB.runDbNoLogging DB.queryLatestBlock
    case mblk of
      Nothing -> logInfo trce "Explorer DB is empty"
      Just block ->
          logInfo trce $ Text.concat
                  [ "Explorer DB tip is at "
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

getCurrentTipBlockNo :: IO BlockNo
getCurrentTipBlockNo = do
    maybeTip <- DB.runDbNoLogging DB.queryLatestBlock
    case maybeTip of
      Just tip -> pure $ convert tip
      Nothing -> pure genesisBlockNo
  where
    convert :: DB.Block -> BlockNo
    convert blk =
      case DB.blockSlotNo blk of
        Just slot -> BlockNo slot
        Nothing -> genesisBlockNo

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'StrictTMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: forall tx reject m. (Monad m, MonadSTM m)
  => StrictTMVar m tx -> LocalTxSubmissionClient tx reject m Void
txSubmissionClient txv = LocalTxSubmissionClient $
    atomically (readTMVar txv) >>= pure . client
  where
    client :: tx -> LocalTxClientStIdle tx reject m Void
    client tx =
      SendMsgSubmitTx tx $ \mbreject -> do
        case mbreject of
          Nothing -> return ()
          Just _r -> return ()
        tx' <- atomically $ readTMVar txv
        pure $ client tx'

localChainSyncCodec
  :: forall blk m. (RunNode blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Tip blk)) DeserialiseFailure m BSL.ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (nodeEncodeBlock pInfoConfig)
      (nodeDecodeBlock pInfoConfig)
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
  => Trace IO Text -> Metrics -> [Point blk] -> BlockNo -> DbActionQueue -> ChainSyncClientPipelined blk (Tip blk) m Void
chainSyncClient trce metrics latestPoints currentTip actionQueue =
    ChainSyncClientPipelined $ pure $
      -- Notify the core node about the our latest points at which we are
      -- synchronised.  This client is not persistent and thus it just
      -- synchronises from the genesis block.  A real implementation should send
      -- a list of points up to a point which is k blocks deep.
      SendMsgFindIntersect
        (if null latestPoints then [genesisPoint] else latestPoints)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound    = \_hdr tip -> pure $ go policy Zero currentTip (tipBlockNo tip)
          , recvMsgIntersectNotFound = \  tip -> pure $ go policy Zero currentTip (tipBlockNo tip)
          }
  where
    policy = pipelineDecisionLowHighMark 1000 10000

    go :: MkPipelineDecision -> Nat n -> BlockNo -> BlockNo -> ClientPipelinedStIdle n ByronBlock (Tip blk) m a
    go mkPipelineDecision n clientTip serverTip =
      case (n, runPipelineDecision mkPipelineDecision n clientTip serverTip) of
        (_Zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext clientStNext (pure clientStNext)
          where
            clientStNext = mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n clientBlockNo (tipBlockNo newServerTip)
        (_, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (go mkPipelineDecision' (Succ n) clientTip serverTip)
        (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            (Just $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip)
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (tipBlockNo newServerTip))
        (Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext $ \clientBlockNo newServerTip -> go mkPipelineDecision' n' clientBlockNo (tipBlockNo newServerTip))

    mkClientStNext :: (BlockNo -> Tip blk -> ClientPipelinedStIdle n ByronBlock (Tip blk) m a)
                    -> ClientStNext n ByronBlock (Tip ByronBlock) m a
    mkClientStNext finish =
      ClientStNext
        { recvMsgRollForward = \blk tip ->
            liftIO .
              logException trce "recvMsgRollForward: " $ do
                Gauge.set (fromIntegral . unBlockNo $ tipBlockNo tip) $ mNodeHeight metrics
                newSize <- atomically $ do
                  writeDbActionQueue actionQueue $ DbApplyBlock blk (tipBlockNo tip)
                  lengthDbActionQueue actionQueue
                Gauge.set (fromIntegral newSize) $ mQueuePostWrite metrics
                pure $ finish (blockNo blk) tip
        , recvMsgRollBackward = \point tip -> do
            liftIO .
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                atomically $ writeDbActionQueue actionQueue (DbRollBackToPoint point)
                newTip <- getCurrentTipBlockNo
                pure $ finish newTip tip
        }

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all explorer code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action =
    action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e

logProtocolMagic :: Trace IO Text -> Crypto.ProtocolMagic -> IO ()
logProtocolMagic tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.getRequiresNetworkMagic pm), " "
    , textShow (Crypto.unProtocolMagicId . unAnnotated $ Crypto.getAProtocolMagicId pm)
    ]
