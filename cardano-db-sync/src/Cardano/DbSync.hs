{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync
  ( ConfigFile (..)
  , DbSyncCommand (..)
  , DbSyncNodeParams (..)
  , DbSyncNodePlugin (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)
  , DB.MigrationDir (..)

  , mkDefDbSyncNodePlugin
  , runDbSyncNode
  ) where

import           Control.Tracer (Tracer)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Trace (Trace, appendName, logInfo, logWarning)
import qualified Cardano.BM.Trace as Logging

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Client.Subscription (subscribe)
import qualified Cardano.Crypto as Crypto

import           Cardano.Db (LogFileDir (..))
import qualified Cardano.Db as DB
import           Cardano.DbSync.Config
import           Cardano.DbSync.Database
import           Cardano.DbSync.DbAction
import           Cardano.DbSync.Era
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin (DbSyncNodePlugin (..))
import           Cardano.DbSync.Plugin.Default (mkDefDbSyncNodePlugin)
import           Cardano.DbSync.Rollback (unsafeRollback)
import           Cardano.DbSync.StateQuery (StateQueryTMVar, getSlotDetails, localStateQueryHandler,
                   newStateQueryTMVar)
import           Cardano.DbSync.Tracing.ToObjectOrphans ()
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude hiding (Nat, option, (%))

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Trans.Except.Exit (orDie)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (contramap)
import qualified Data.Text as Text

import           Network.Mux (MuxTrace, WithMuxBearer)
import           Network.Mux.Types (MuxMode (..))

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero))
import           Ouroboros.Network.Driver.Simple (runPipelinedPeer)

import           Ouroboros.Consensus.Block.Abstract (CodecConfig, ConvertRawHash (..))
import           Ouroboros.Consensus.Byron.Ledger.Config (mkByronCodecConfig)
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, CodecConfig (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.HardFork.History.Qry (Interpreter)
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, cChainSyncCodec,
                   cStateQueryCodec, cTxSubmissionCodec)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (ShelleyCodecConfig))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Ouroboros.Network.Block (BlockNo (..), HeaderHash, Point (..), Tip (..), blockNo,
                   genesisPoint, getTipBlockNo, getTipPoint)
import           Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..), ConnectionId,
                   ErrorPolicyTrace (..), Handshake, IOManager, LocalAddress,
                   NetworkSubscriptionTracers (..), NodeToClientProtocols (..), TraceSendRecv,
                   WithAddr (..), localSnocket, localTxSubmissionPeerNull, networkErrorPolicies,
                   withIOManager)
import qualified Ouroboros.Network.NodeToClient.Version as Network
import           Ouroboros.Network.Point (withOrigin)
import qualified Ouroboros.Network.Point as Point

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (..), ClientPipelinedStIdle (..),
                   ClientPipelinedStIntersect (..), ClientStNext (..), chainSyncClientPeerPipelined,
                   recvMsgIntersectFound, recvMsgIntersectNotFound, recvMsgRollBackward,
                   recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (MkPipelineDecision,
                   PipelineDecision (..), pipelineDecisionLowHighMark, runPipelineDecision)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (localStateQueryClientPeer)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription (SubscriptionTrace)

import           Prelude (String)
import qualified Prelude

import           System.Directory (createDirectoryIfMissing)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge


runDbSyncNode :: DbSyncNodePlugin -> DbSyncNodeParams -> IO ()
runDbSyncNode plugin enp =
  withIOManager $ \ iomgr -> do
    DB.runMigrations Prelude.id True (enpMigrationDir enp) (Just $ LogFileDir "/tmp")

    enc <- readDbSyncNodeConfig (enpConfigFile enp)

    createDirectoryIfMissing True (unLedgerStateDir $ enpLedgerStateDir enp)

    trce <- if not (dncEnableLogging enc)
              then pure Logging.nullTracer
              else liftIO $ Logging.setupTrace (Right $ dncLoggingConfig enc) "db-sync-node"

    -- For testing and debugging.
    case enpMaybeRollback enp of
      Just slotNo -> void $ unsafeRollback trce slotNo
      Nothing -> pure ()

    orDie renderDbSyncNodeError $ do
      genCfg <- readCardanoGenesisConfig enc
      genesisEnv <- hoistEither $ genesisConfigToEnv enp genCfg
      logProtocolMagicId trce $ genesisProtocolMagicId genCfg

      -- If the DB is empty it will be inserted, otherwise it will be validated (to make
      -- sure we are on the right chain).
      insertValidateGenesisDist trce (dncNetworkName enc) genCfg

      liftIO $ do
        -- Must run plugin startup after the genesis distribution has been inserted/validate.
        runDbStartup trce plugin
        case genCfg of
          GenesisCardano _ bCfg _sCfg -> do
            ledgerVar <- initLedgerStateVar genCfg
            loadLatestLedgerState (envLedgerStateDir genesisEnv) ledgerVar
            runDbSyncNodeNodeClient genesisEnv ledgerVar
                iomgr trce plugin (cardanoCodecConfig bCfg) (enpSocketPath enp)
  where
    cardanoCodecConfig :: Byron.Config -> CodecConfig CardanoBlock
    cardanoCodecConfig cfg =
      CardanoCodecConfig
        (mkByronCodecConfig cfg)
        ShelleyCodecConfig
        ShelleyCodecConfig -- Allegra
        ShelleyCodecConfig -- Mary

-- -------------------------------------------------------------------------------------------------

runDbSyncNodeNodeClient
    :: DbSyncEnv -> LedgerStateVar -> IOManager -> Trace IO Text -> DbSyncNodePlugin
    -> CodecConfig CardanoBlock -> SocketPath
    -> IO ()
runDbSyncNodeNodeClient env ledgerVar iomgr trce plugin codecConfig (SocketPath socketPath) = do
  queryVar <- newStateQueryTMVar
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $ subscribe
    (localSnocket iomgr socketPath)
    codecConfig
    (envNetworkMagic env)
    networkSubscriptionTracers
    clientSubscriptionParams
    (dbSyncProtocols trce env plugin queryVar ledgerVar)
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
    -> DbSyncEnv
    -> DbSyncNodePlugin
    -> StateQueryTMVar CardanoBlock (Interpreter (CardanoEras StandardCrypto))
    -> LedgerStateVar
    -> Network.NodeToClientVersion
    -> ClientCodecs CardanoBlock IO
    -> ConnectionId LocalAddress
    -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce env plugin queryVar ledgerVar _version codecs _connectionId =
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
        latestPoints <- getLatestPoints (envLedgerStateDir env)
        currentTip <- getCurrentTipBlockNo
        logDbState trce
        actionQueue <- newDbActionQueue
        (metrics, server) <- registerMetricsServer
        race_
            (runDbThread trce env plugin metrics actionQueue ledgerVar)
            (runPipelinedPeer
                localChainSyncTracer
                (cChainSyncCodec codecs)
                channel
                (chainSyncClientPeerPipelined
                    $ chainSyncClient trce env queryVar metrics latestPoints currentTip actionQueue)
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

    localStateQuery :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localStateQuery =
      InitiatorProtocolOnly $ MuxPeer
        (contramap (Text.pack . show) . toLogObject $ appendName "local-state-query" trce)
        (cStateQueryCodec codecs)
        (localStateQueryClientPeer (localStateQueryHandler queryVar))


logDbState :: Trace IO Text -> IO ()
logDbState trce = do
    mblk <- DB.runDbNoLogging DB.queryLatestBlock
    case mblk of
      Nothing -> logInfo trce "Database is empty"
      Just block ->
          logInfo trce $ mconcat
                  [ "Database tip is at "
                  , Text.pack (showTip block)
                  ]
  where
    showTip :: DB.Block -> String
    showTip blk =
      case (DB.blockSlotNo blk, DB.blockBlockNo blk) of
        (Just slotNo, Just blkNo) -> "slot " ++ show slotNo ++ ", block " ++ show blkNo
        (Just slotNo, Nothing) -> "slot " ++ show slotNo
        (Nothing, Just blkNo) -> "block " ++ show blkNo
        (Nothing, Nothing) -> "genesis"


getLatestPoints :: LedgerStateDir -> IO [Point CardanoBlock]
getLatestPoints ledgerStateDir = do
    xs <- lsfSlotNo <<$>> listLedgerStateFilesOrdered ledgerStateDir
    ys <- catMaybes <$> DB.runDbNoLogging (mapM DB.querySlotHash xs)
    pure $ mapMaybe convert ys
  where
    convert :: (SlotNo, ByteString) -> Maybe (Point CardanoBlock)
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

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
    :: Trace IO Text -> DbSyncEnv
    -> StateQueryTMVar CardanoBlock (Interpreter (CardanoEras StandardCrypto))
    -> Metrics -> [Point CardanoBlock] -> WithOrigin BlockNo -> DbActionQueue
    -> ChainSyncClientPipelined CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClient trce env queryVar metrics latestPoints currentTip actionQueue =
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
            (Just $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) clientTip serverTip)
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
                if withOrigin 0 unBlockNo (getTipBlockNo tip) == 0
                  then do
                    -- https://github.com/input-output-hk/cardano-db-sync/issues/433
                    liftIO $ logWarning trce "Node has not yet received blocks from the network. Sleeping for 1 minute."
                    threadDelay (60 * 1000 * 1000)
                  else do
                    Gauge.set (withOrigin 0 (fromIntegral . unBlockNo) (getTipBlockNo tip)) (mNodeHeight metrics)
                    details <- getSlotDetails trce env queryVar (getTipPoint tip) (cardanoBlockSlotNo blk)
                    newSize <- atomically $ do
                                writeDbActionQueue actionQueue $ mkDbApply blk details
                                lengthDbActionQueue actionQueue
                    Gauge.set (fromIntegral newSize) $ mQueuePostWrite metrics
                pure $ finish (At (blockNo blk)) tip
        , recvMsgRollBackward = \point tip ->
              logException trce "recvMsgRollBackward: " $ do
                -- This will get the current tip rather than what we roll back to
                -- but will only be incorrect for a short time span.
                let slot = toRollbackSlot point
                atomically $ writeDbActionQueue actionQueue (mkDbRollback slot)
                newTip <- getCurrentTipBlockNo
                pure $ finish newTip tip
        }

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT DbSyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.unProtocolMagicId pm)
    ]
