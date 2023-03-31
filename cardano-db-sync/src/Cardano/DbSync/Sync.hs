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
  runSyncNode,
) where

import Cardano.BM.Data.Tracer (ToLogObject (..), ToObject)
import Cardano.BM.Trace (Trace, appendName, logInfo, logWarning)
import qualified Cardano.BM.Trace as Logging
import Cardano.Client.Subscription (subscribe)
import qualified Cardano.Crypto as Crypto
import Cardano.Db (runDbIohkLogging)
import qualified Cardano.Db as Db
import Cardano.DbSync.Api
import Cardano.DbSync.Config
import Cardano.DbSync.Database
import Cardano.DbSync.DbAction
import Cardano.DbSync.Epoch
import Cardano.DbSync.Era
import Cardano.DbSync.Error
import Cardano.DbSync.Fix.PlutusDataBytes
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Metrics
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (Meta, Nat, option, (%))
import Cardano.Slotting.Slot (EpochNo (..), WithOrigin (..))
import qualified Codec.CBOR.Term as CBOR
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Tracer (Tracer)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import qualified Data.Text as Text
import Database.Persist.Postgresql (ConnectionString, SqlBackend, withPostgresqlConn)
import Network.Mux (MuxTrace, WithMuxBearer)
import Network.Mux.Types (MuxMode (..))
import Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (configCodec)
import qualified Ouroboros.Consensus.HardFork.Simple as HardFork
import Ouroboros.Consensus.Network.NodeToClient (
  ClientCodecs,
  Codecs' (..),
  cChainSyncCodec,
  cStateQueryCodec,
  cTxSubmissionCodec,
 )
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
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
import System.Directory (createDirectoryIfMissing)

runSyncNode ::
  MetricSetters ->
  Trace IO Text ->
  IOManager ->
  ConnectionString ->
  Bool ->
  RunMigration ->
  SyncNodeParams ->
  SyncOptions ->
  IO ()
runSyncNode metricsSetters trce iomgr dbConnString ranAll runMigration syncNodeParams syncOptions = do
  let configFile = enpConfigFile syncNodeParams
      maybeLedgerDir = enpMaybeLedgerStateDir syncNodeParams
  syncNodeConfig <- readSyncNodeConfig configFile
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)

  logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfig)
  logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfig)
  logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfig)

  orDie renderSyncNodeError $ do
    genCfg <- readCardanoGenesisConfig syncNodeConfig
    logProtocolMagicId trce $ genesisProtocolMagicId genCfg
    syncEnv <-
      ExceptT $
        mkSyncEnvFromConfig
          trce
          dbConnString
          syncOptions
          (enpMaybeLedgerStateDir syncNodeParams)
          (enpShouldUseLedger syncNodeParams)
          genCfg
          ranAll
          (enpForceIndexes syncNodeParams)
          runMigration

    -- If the DB is empty it will be inserted, otherwise it will be validated (to make
    -- sure we are on the right chain).
    lift $
      Db.runIohkLogging trce $
        withPostgresqlConn dbConnString $ \backend -> do
          liftIO $
            unless (enpShouldUseLedger syncNodeParams) $ do
              logInfo trce "Migrating to a no ledger schema"
              Db.noLedgerMigrations backend trce
          lift $ orDie renderSyncNodeError $ insertValidateGenesisDist trce backend (dncNetworkName syncNodeConfig) genCfg (useShelleyInit syncNodeConfig)
          liftIO $ epochStartup (enpExtended syncNodeParams) trce backend

    case genCfg of
      GenesisCardano {} -> do
        liftIO $ runSyncNodeClient metricsSetters syncEnv iomgr trce (enpSocketPath syncNodeParams)
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        HardFork.TriggerHardForkAtEpoch (EpochNo 0) -> True
        _ -> False

runSyncNodeClient ::
  MetricSetters ->
  SyncEnv ->
  IOManager ->
  Trace IO Text ->
  SocketPath ->
  IO ()
runSyncNodeClient metricsSetters syncEnv iomgr trce (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  void $
    subscribe
      (localSnocket iomgr)
      codecConfig
      (envNetworkMagic syncEnv)
      networkSubscriptionTracers
      clientSubscriptionParams
      (dbSyncProtocols trce syncEnv metricsSetters)
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec $ getTopLevelConfig syncEnv

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
  Trace IO Text ->
  SyncEnv ->
  MetricSetters ->
  Network.NodeToClientVersion ->
  ClientCodecs CardanoBlock IO ->
  ConnectionId LocalAddress ->
  NodeToClientProtocols 'InitiatorMode BSL.ByteString IO () Void
dbSyncProtocols trce syncEnv metricsSetters _version codecs _connectionId =
  NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncPtcl
    , localTxSubmissionProtocol = dummylocalTxSubmit
    , localStateQueryProtocol = localStateQuery
    , localTxMonitorProtocol =
        InitiatorProtocolOnly $ MuxPeer Logging.nullTracer (cTxMonitorCodec codecs) localTxMonitorPeerNull
    }
  where
    localChainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync CardanoBlock (Point CardanoBlock) (Tip CardanoBlock)))
    localChainSyncTracer = toLogObject $ appendName "ChainSync" trce

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    localChainSyncPtcl :: RunMiniProtocol 'InitiatorMode BSL.ByteString IO () Void
    localChainSyncPtcl = InitiatorProtocolOnly $
      MuxPeerRaw $ \channel ->
        liftIO . logException trce "ChainSyncWithBlocksPtcl: " $ do
          Db.runIohkLogging trce $
            withPostgresqlConn (envConnString syncEnv) $ \backend -> liftIO $ do
              replaceConnection syncEnv backend
              setConsistentLevel syncEnv Unchecked

              isFixed <- getIsSyncFixed syncEnv
              let skipFix = soptSkipFix $ envOptions syncEnv
              let onlyFix = soptOnlyFix $ envOptions syncEnv
              if onlyFix || (not isFixed && not skipFix)
                then do
                  fd <- runDbIohkLogging backend tracer $ getWrongPlutusData tracer
                  unless (nullData fd) $
                    void $
                      runPeer
                        localChainSyncTracer
                        (cChainSyncCodec codecs)
                        channel
                        ( Client.chainSyncClientPeer $
                            chainSyncClientFix backend tracer fd
                        )
                  setIsFixedAndMigrate syncEnv
                  when onlyFix $ panic "All Good! This error is only thrown to exit db-sync." -- TODO fix.
                else do
                  when skipFix $ setIsFixedAndMigrate syncEnv
                  -- The Db thread is not forked at this point, so we can use
                  -- the connection here. A connection cannot be used concurrently by many
                  -- threads
                  logInfo trce "Starting chainSyncClient"
                  latestPoints <- getLatestPoints syncEnv
                  let (inMemory, onDisk) = List.span snd latestPoints
                  logInfo trce $
                    mconcat
                      [ "Suggesting intersection points from memory: "
                      , textShow (fst <$> inMemory)
                      , " and from disk: "
                      , textShow (fst <$> onDisk)
                      ]
                  currentTip <- getCurrentTipBlockNo syncEnv
                  logDbState syncEnv
                  -- communication channel between datalayer thread and chainsync-client thread
                  actionQueue <- newDbActionQueue

                  race_
                    ( concurrently
                        (runDbThread syncEnv metricsSetters actionQueue)
                        (runOfflineFetchThread syncEnv)
                    )
                    ( runPipelinedPeer
                        localChainSyncTracer
                        (cChainSyncCodec codecs)
                        channel
                        ( chainSyncClientPeerPipelined $
                            chainSyncClient metricsSetters trce (fst <$> latestPoints) currentTip actionQueue
                        )
                    )

                  atomically $ writeDbActionQueue actionQueue DbFinish
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
              (contramap (Text.pack . show) . toLogObject $ appendName "local-state-query" trce)
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
  DbActionQueue ->
  ChainSyncClientPipelined CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClient metricsSetters trce latestPoints currentTip actionQueue = do
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
                writeDbActionQueue actionQueue $ mkDbApply blk
                lengthDbActionQueue actionQueue

              setDbQueueLength metricsSetters newSize

              pure $ finish (At (blockNo blk)) tip Nothing
        , recvMsgRollBackward = \point tip ->
            logException trce "recvMsgRollBackward: " $ do
              -- This will get the current tip rather than what we roll back to
              -- but will only be incorrect for a short time span.
              (mPoints, newTip) <- waitRollback actionQueue point tip
              pure $ finish newTip tip mPoints
        }

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $
    mconcat
      [ "NetworkMagic: "
      , textShow (Crypto.unProtocolMagicId pm)
      ]

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

chainSyncClientFix ::
  SqlBackend -> Trace IO Text -> FixData -> ChainSyncClient CardanoBlock (Point CardanoBlock) (Tip CardanoBlock) IO ()
chainSyncClientFix backend tracer fixData = Client.ChainSyncClient $ do
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
      case spanOnNextPoint fds of
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
