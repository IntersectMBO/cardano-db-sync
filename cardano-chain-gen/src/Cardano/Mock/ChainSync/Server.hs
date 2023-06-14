{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.ChainSync.Server (
  -- * server
  forkServerThread,
  withServerHandle,
  stopServer,
  restartServer,

  -- * ServerHandle api
  ServerHandle (..),
  MockServerConstraint,
  IOManager,
  replaceGenesis,
  addBlock,
  rollback,
  readChain,
  withIOManager,
) where

import Cardano.Mock.Chain hiding (rollback)
import Cardano.Mock.ChainDB
import Cardano.Mock.ChainSync.State
import Codec.Serialise.Class (Serialise)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Class.MonadSTM.Strict (
  MonadSTM (atomically),
  STM,
  StrictTVar,
  modifyTVar,
  newTVarIO,
  readTVar,
  retry,
  writeTVar,
 )
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Void (Void)
import Network.TypedProtocol.Core (Peer (..))
import Ouroboros.Consensus.Block (CodecConfig, HasHeader, Point, StandardHash, castPoint)
import Ouroboros.Consensus.Config (TopLevelConfig, configCodec)
import Ouroboros.Consensus.Ledger.Query (BlockQuery, ShowQuery)
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Network.NodeToClient (Apps (..), Codecs' (..), DefaultCodecs)
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import Ouroboros.Consensus.Node.DbLock ()
import Ouroboros.Consensus.Node.DbMarker ()
import Ouroboros.Consensus.Node.InitStorage ()
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
  BlockNodeToClientVersion,
  NodeToClientVersion,
  SupportedNetworkProtocolVersion,
  latestReleasedNodeVersion,
  supportedNodeToClientVersions,
 )
import Ouroboros.Consensus.Node.ProtocolInfo ()
import Ouroboros.Consensus.Node.Recovery ()
import Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import Ouroboros.Consensus.Node.Tracers ()
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))
import Ouroboros.Consensus.Util.Args ()
import Ouroboros.Network.Block (
  ChainUpdate (AddBlock, RollBack),
  HeaderHash,
  Serialised (..),
  Tip,
  castTip,
  genesisPoint,
  mkSerialised,
 )
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.IOManager (IOManager)
import qualified Ouroboros.Network.IOManager as IOManager
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux (MuxMode (..), OuroborosApplication)
import Ouroboros.Network.NodeToClient (NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import Ouroboros.Network.NodeToNode (Versions)
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (..),
  ServerStIntersect (..),
  ServerStNext (SendMsgRollBackward, SendMsgRollForward),
  chainSyncServerPeer,
 )
import Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Util.ShowProxy (Proxy (..), ShowProxy)

{- HLINT ignore "Use readTVarIO" -}

data ServerHandle m blk = ServerHandle
  { chainProducerState :: StrictTVar m (ChainProducerState blk)
  , threadHandle :: StrictTVar m (Async ())
  , forkAgain :: m (Async ())
  }

replaceGenesis :: MonadSTM m => ServerHandle m blk -> State blk -> STM m ()
replaceGenesis handle st =
  modifyTVar (chainProducerState handle) $ \cps ->
    cps {chainDB = replaceGenesisDB (chainDB cps) st}

readChain :: MonadSTM m => ServerHandle m blk -> STM m (Chain blk)
readChain handle = do
  cchain . chainDB <$> readTVar (chainProducerState handle)

addBlock :: (LedgerSupportsProtocol blk, MonadSTM m) => ServerHandle m blk -> blk -> STM m ()
addBlock handle blk =
  modifyTVar (chainProducerState handle) $
    addBlockState blk

rollback :: (LedgerSupportsProtocol blk, MonadSTM m) => ServerHandle m blk -> Point blk -> STM m ()
rollback handle point =
  modifyTVar (chainProducerState handle) $ \st ->
    case rollbackState point st of
      Nothing -> error $ "point " <> show point <> " not in chain"
      Just st' -> st'

restartServer :: ServerHandle IO blk -> IO ()
restartServer sh = do
  stopServer sh
  -- TODO not sure why, but this delay is necessary. Without it reconnection doesn't happen
  -- some times.
  threadDelay 1_000_000
  thread <- forkAgain sh
  atomically $ writeTVar (threadHandle sh) thread

stopServer :: ServerHandle IO blk -> IO ()
stopServer sh = do
  srvThread <- atomically $ readTVar $ threadHandle sh
  cancel srvThread

type MockServerConstraint blk =
  ( SerialiseNodeToClientConstraints blk
  , ShowQuery (BlockQuery blk)
  , StandardHash blk
  , ShowProxy (ApplyTxErr blk)
  , Serialise (HeaderHash blk)
  , ShowProxy (BlockQuery blk)
  , ShowProxy blk
  , HasHeader blk
  , ShowProxy (GenTx blk)
  , SupportedNetworkProtocolVersion blk
  , EncodeDisk blk blk
  )

forkServerThread ::
  forall blk.
  MockServerConstraint blk =>
  IOManager ->
  TopLevelConfig blk ->
  State blk ->
  NetworkMagic ->
  FilePath ->
  IO (ServerHandle IO blk)
forkServerThread iom config initSt netMagic path = do
  chainSt <- newTVarIO $ initChainProducerState config initSt
  let runThread = async $ runLocalServer iom (configCodec config) netMagic path chainSt
  thread <- runThread
  threadVar <- newTVarIO thread
  pure $ ServerHandle chainSt threadVar runThread

withServerHandle ::
  forall blk a.
  MockServerConstraint blk =>
  IOManager ->
  TopLevelConfig blk ->
  State blk ->
  NetworkMagic ->
  FilePath ->
  (ServerHandle IO blk -> IO a) ->
  IO a
withServerHandle iom config initSt netMagic path =
  bracket (forkServerThread iom config initSt netMagic path) stopServer

-- | Must be called from the main thread
runLocalServer ::
  forall blk.
  MockServerConstraint blk =>
  IOManager ->
  CodecConfig blk ->
  NetworkMagic ->
  FilePath ->
  StrictTVar IO (ChainProducerState blk) ->
  IO ()
runLocalServer iom codecConfig netMagic localDomainSock chainProdState =
  withSnocket iom localDomainSock $ \localSocket localSnocket -> do
    networkState <- NodeToClient.newNetworkMutableState
    _ <-
      NodeToClient.withServer
        localSnocket
        NodeToClient.nullNetworkServerTracers -- debuggingNetworkServerTracers
        networkState
        localSocket
        (versions chainProdState)
        NodeToClient.networkErrorPolicies
    pure ()
  where
    versions ::
      StrictTVar IO (ChainProducerState blk) ->
      Versions
        NodeToClientVersion
        NodeToClientVersionData
        (OuroborosApplication 'ResponderMode LocalAddress ByteString IO Void ())
    versions state =
      let version = fromJust $ snd $ latestReleasedNodeVersion (Proxy @blk)
          allVersions = supportedNodeToClientVersions (Proxy @blk)
          blockVersion = fromJust $ Map.lookup version allVersions
       in simpleSingletonVersions
            version
            (NodeToClientVersionData netMagic False)
            (NTC.responder version $ mkApps state version blockVersion (NTC.defaultCodecs codecConfig blockVersion version))

    mkApps ::
      StrictTVar IO (ChainProducerState blk) ->
      NodeToClientVersion ->
      BlockNodeToClientVersion blk ->
      DefaultCodecs blk IO ->
      NTC.Apps IO localPeer ByteString ByteString ByteString ByteString ()
    mkApps state _version blockVersion codecs =
      Apps
        { aChainSyncServer = chainSyncServer'
        , aTxSubmissionServer = txSubmitServer
        , aStateQueryServer = stateQueryServer
        , aTxMonitorServer = txMonitorServer
        }
      where
        chainSyncServer' ::
          localPeer ->
          Channel IO ByteString ->
          IO ((), Maybe ByteString)
        chainSyncServer' _them channel =
          runPeer
            nullTracer -- TODO add a tracer!
            (cChainSyncCodec codecs)
            channel
            $ chainSyncServerPeer
            $ chainSyncServer state codecConfig blockVersion

        txSubmitServer ::
          localPeer ->
          Channel IO ByteString ->
          IO ((), Maybe ByteString)
        txSubmitServer _them channel =
          runPeer
            nullTracer
            (cTxSubmissionCodec codecs)
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

        stateQueryServer ::
          localPeer ->
          Channel IO ByteString ->
          IO ((), Maybe ByteString)
        stateQueryServer _them channel =
          runPeer
            nullTracer
            (cStateQueryCodec codecs)
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

        txMonitorServer ::
          localPeer ->
          Channel IO ByteString ->
          IO ((), Maybe ByteString)
        txMonitorServer _them channel =
          runPeer
            nullTracer
            (cStateQueryCodec codecs)
            channel
            (Effect (forever $ threadDelay 3_600_000_000))

chainSyncServer ::
  forall blk m.
  ( HasHeader blk
  , MonadSTM m
  , EncodeDisk blk blk
  ) =>
  StrictTVar m (ChainProducerState blk) ->
  CodecConfig blk ->
  BlockNodeToClientVersion blk ->
  ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncServer state codec _blockVersion =
  ChainSyncServer $ idle <$> newFollower
  where
    idle :: FollowerId -> ServerStIdle (Serialised blk) (Point blk) (Tip blk) m ()
    idle r =
      ServerStIdle
        { recvMsgRequestNext = handleRequestNext r
        , recvMsgFindIntersect = handleFindIntersect r
        , recvMsgDoneClient = pure ()
        }

    idle' :: FollowerId -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
    idle' = ChainSyncServer . pure . idle

    handleRequestNext ::
      FollowerId ->
      m
        ( Either
            (ServerStNext (Serialised blk) (Point blk) (Tip blk) m ())
            (m (ServerStNext (Serialised blk) (Point blk) (Tip blk) m ()))
        )
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> pure (Left (sendNext r update))
        Nothing -> pure (Right (sendNext r <$> readChainUpdate r))
    -- Follower is at the head, have to block and wait for
    -- the producer's state to change.

    handleFindIntersect ::
      FollowerId ->
      [Point blk] ->
      m (ServerStIntersect (Serialised blk) (Point blk) (Tip blk) m ())
    handleFindIntersect r points = do
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip) -> pure $ SendMsgIntersectFound pt tip (idle' r)
        (Nothing, tip) -> pure $ SendMsgIntersectNotFound tip (idle' r)

    sendNext ::
      FollowerId ->
      (Tip blk, ChainUpdate blk blk) ->
      ServerStNext (Serialised blk) (Point blk) (Tip blk) m ()
    sendNext r (tip, AddBlock b) =
      -- SendMsgRollForward  -- (Serialised $ toLazyByteString $ encodeNodeToClient codec blockVersion b) tip (idle' r)
      SendMsgRollForward (mkSerialised (encodeDisk codec) b) tip (idle' r) -- encodeNodeToClient codec blockVersion -- mkSerialised encode b
    sendNext r (tip, RollBack p) = SendMsgRollBackward (castPoint p) tip (idle' r)

    newFollower :: m FollowerId
    newFollower = atomically $ do
      cps <- readTVar state
      let (cps', rid) = initFollower genesisPoint cps
      _ <- writeTVar state cps'
      pure rid

    improveReadPoint ::
      FollowerId ->
      [Point blk] ->
      m (Maybe (Point blk), Tip blk)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar state
        let chain = chainDB cps
        case findFirstPoint (map castPoint points) chain of
          Nothing -> pure (Nothing, castTip (headTip chain))
          Just ipoint -> do
            let !cps' = updateFollower rid ipoint cps
            writeTVar state cps'
            let chain' = chainDB cps'
            pure (Just (castPoint ipoint), castTip (headTip chain'))

    tryReadChainUpdate ::
      FollowerId ->
      m (Maybe (Tip blk, ChainUpdate blk blk))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar state
        case followerInstruction rid cps of
          Nothing -> pure Nothing
          Just (u, cps') -> do
            writeTVar state cps'
            let chain = chainDB cps'
            pure $ Just (castTip (headTip chain), u)

    readChainUpdate :: FollowerId -> m (Tip blk, ChainUpdate blk blk)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar state
        case followerInstruction rid cps of
          Nothing -> retry
          Just (u, cps') -> do
            writeTVar state cps'
            let chain = chainDB cps'
            pure (castTip (headTip chain), u)

withSnocket ::
  forall a.
  IOManager ->
  FilePath ->
  (LocalSocket -> LocalSnocket -> IO a) ->
  IO a
withSnocket iocp localDomainSock k =
  bracket localServerInit localServerCleanup localServerBody
  where
    localServerInit :: IO (LocalSocket, LocalSnocket)
    localServerInit = do
      let sn = Snocket.localSnocket iocp
      sd <-
        Snocket.open
          sn
          ( Snocket.addrFamily sn $
              Snocket.localAddressFromPath localDomainSock
          )
      pure (sd, sn)

    -- We close the socket here, even if it was provided for us.
    localServerCleanup :: (LocalSocket, LocalSnocket) -> IO ()
    localServerCleanup (sd, sn) = Snocket.close sn sd

    localServerBody :: (LocalSocket, LocalSnocket) -> IO a
    localServerBody (sd, sn) = do
      Snocket.bind sn sd (Snocket.localAddressFromPath localDomainSock)
      Snocket.listen sn sd
      k sd sn

withIOManager :: (IOManager -> IO a) -> IO a
withIOManager = IOManager.withIOManager
