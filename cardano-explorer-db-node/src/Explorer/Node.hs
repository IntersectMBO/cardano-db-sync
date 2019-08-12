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
  ( ExplorerNodeParams (..)
  , CLI (..)
  , NodeLayer (..)
  , initializeAllFeatures
  ) where

import           Control.Exception (throw)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadSTM, TMVar, atomically, readTMVar, newEmptyTMVarM)
import           Control.Monad.Class.MonadThrow (MonadThrow)
import           Control.Monad.Class.MonadTimer (MonadTimer)

import           Cardano.Binary (unAnnotated)

import           Cardano.BM.Data.Tracer (ToLogObject (toLogObject), Tracer, nullTracer)
import           Cardano.BM.Trace (Trace, appendName)

import           Cardano.Chain.Common (ChainDifficulty(unChainDifficulty))
import           Cardano.Chain.Block (ABlockOrBoundary (ABOBBlock, ABOBBoundary), ABlock(ABlock),
                    ABoundaryBlock(ABoundaryBlock), aHeaderDifficulty, boundaryDifficulty)
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update


import           Cardano.Crypto (RequiresNetworkMagic(RequiresNoMagic,RequiresMagic),
                    decodeAbstractHash)
import qualified Cardano.Node.CLI as Node
import           Cardano.Prelude hiding (atomically, option, (%))
import           Cardano.Shell.Configuration.Lib (finaliseCardanoConfiguration)
import           Cardano.Shell.Constants.PartialTypes (PartialCardanoConfiguration)
import           Cardano.Shell.Constants.Types (CardanoConfiguration(..),
                    coRequiresNetworkMagic, ccCore, coGenesisFile,
                    RequireNetworkMagic(NoRequireNetworkMagic,RequireNetworkMagic), coGenesisHash)
import           Cardano.Shell.Features.Logging (LoggingLayer, LoggingCLIArguments,
                    createLoggingFeature, llAppendName, llBasicTrace)
import           Cardano.Shell.Lib (GeneralException (ConfigurationError))
import           Cardano.Shell.Types (CardanoEnvironment, CardanoFeature (..),
                    CardanoFeatureInit (..), featureCleanup, featureInit,
                    featureShutdown, featureStart, featureType)

import qualified Codec.Serialise as Serialise

import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Contravariant (contramap)
import           Data.Reflection (give)
import qualified Data.Text as Text

import           Formatting (sformat, int, (%))

import           Network.Socket (SockAddr, AddrInfo, SocketType(Stream), Family(AF_UNIX),
                    AddrInfo (AddrInfo), defaultProtocol, SockAddr (SockAddrUnix))

import           Network.TypedProtocol.Codec (Codec)
import           Network.TypedProtocol.Codec.Cbor (DeserialiseFailure)
import           Network.TypedProtocol.Driver (TraceSendRecv, runPeer)

import           Ouroboros.Consensus.Ledger.Abstract (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (GenTx, ByronBlockOrEBB(unByronBlockOrEBB))
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes(NumCoreNodes),
                    ProtocolInfo (ProtocolInfo,pInfoConfig), protocolInfo)
import           Ouroboros.Consensus.Node.Run.Abstract (RunNode, nodeDecodeBlock, nodeDecodeGenTx,
                    nodeDecodeHeaderHash, nodeEncodeBlock, nodeEncodeGenTx, nodeEncodeHeaderHash)
import           Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId))
import           Ouroboros.Consensus.Protocol (NodeConfig, Protocol(ProtocolRealPBFT))
import           Ouroboros.Network.Block (Point, decodePoint, encodePoint, StandardHash, genesisPoint)
import           Ouroboros.Network.Mux (AppType (InitiatorApp),
                    OuroborosApplication (OuroborosInitiatorApplication))
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (ChainSyncWithBlocksPtcl,
                    LocalTxSubmissionPtcl), NodeToClientVersion (NodeToClientV_1),
                    NodeToClientVersionData (NodeToClientVersionData), connectTo, networkMagic,
                    nodeToClientCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient (ChainSyncClient),
                    ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                    ClientStIntersect (ClientStIntersect), ClientStNext (ClientStNext),
                    chainSyncClientPeer, recvMsgIntersectFound, recvMsgIntersectNotFound,
                    recvMsgRollBackward, recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Ouroboros.Network.Protocol.Handshake.Version (DictVersion(DictVersion), Versions,
                    simpleSingletonVersions)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (LocalTxSubmissionClient (SendMsgSubmitTx),
                    localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)

import           Prelude (String, id)

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data ExplorerNodeParams = ExplorerNodeParams
  { enpLogging :: !LoggingCLIArguments
  , enpCLI :: !CLI
  }

data CLI = CLI
  { cliProtocol :: Node.Protocol
  , cliCommon :: Node.CommonCLI
  }

newtype NodeLayer = NodeLayer
  { nlRunNode :: forall m. MonadIO m => m ()
  }

type NodeCardanoFeature = CardanoFeatureInit LoggingLayer CLI NodeLayer


initializeAllFeatures :: ExplorerNodeParams -> PartialCardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures enp partialConfig cardanoEnvironment = do
  let fcc = finaliseCardanoConfiguration $ Node.mergeConfiguration partialConfig (cliCommon $ enpCLI enp)
  finalConfig <- case fcc of
                  Left err -> throwIO $ ConfigurationError err
                  --TODO: if we're using exceptions for this, then we should use a local
                  -- excption type, local to this app, that enumerates all the ones we
                  -- are reporting, and has proper formatting of the result.
                  -- It would also require catching at the top level and printing.
                  Right x  -> pure x

  (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig (enpLogging enp)
  (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer (enpCLI enp) cardanoEnvironment finalConfig

    -- Here we return all the features.
  let allCardanoFeatures :: [CardanoFeature]
      allCardanoFeatures = [ loggingFeature, nodeFeature ]

  pure (allCardanoFeatures, nodeLayer)


createNodeFeature :: LoggingLayer -> CLI -> CardanoEnvironment -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer cli cardanoEnvironment cardanoConfiguration = do
  -- we parse any additional configuration if there is any
  -- We don't know where the user wants to fetch the additional configuration from, it could be from
  -- the filesystem, so we give him the most flexible/powerful context, @IO@.

  -- we construct the layer
  nodeLayer <- featureInit nodeCardanoFeatureInit cardanoEnvironment loggingLayer cardanoConfiguration cli

  -- we construct the cardano feature
  let cardanoFeature = nodeCardanoFeature nodeCardanoFeatureInit nodeLayer

  -- we return both
  pure (nodeLayer, cardanoFeature)

nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit =
    CardanoFeatureInit
      { featureType    = "NodeFeature"
      , featureInit    = featureStart'
      , featureCleanup = featureCleanup'
      }
  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> CLI -> IO NodeLayer
    featureStart' _ loggingLayer cc cli = do
        let
          tr :: Trace IO Text
          tr = llAppendName loggingLayer "wallet" (llBasicTrace loggingLayer)
          coreClient :: IO ()
          coreClient = runClient cli tr cc
        pure $ NodeLayer {nlRunNode = liftIO coreClient}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()

nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer =
  CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }

runClient :: CLI -> Trace IO Text -> CardanoConfiguration -> IO ()
runClient _ tracer cc = do
  let
    tracer' = contramap Text.pack . toLogObject $ appendName "SQLDumper" tracer
    genHash = either (throw . ConfigurationError) id $
              decodeAbstractHash (coGenesisHash $ ccCore cc)

  gcE <- runExceptT $
            Genesis.mkConfigFromFile
              (cvtRNM . coRequiresNetworkMagic $ ccCore cc)
              (coGenesisFile $ ccCore cc)
              genHash
  let
    gc :: Genesis.Config
    gc = case gcE of
      Left err -> throw err -- TODO: no no no!
      Right x -> x
    Genesis.Config{Genesis.configGenesisData} = gc
    Genesis.GenesisData{Genesis.gdProtocolMagicId} = configGenesisData
    defSoftVer  = Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1
    defProtoVer = Update.ProtocolVersion 0 2 0
    p :: Protocol (ByronBlockOrEBB ByronConfig)
    p = ProtocolRealPBFT gc Nothing defProtoVer defSoftVer Nothing
  give (Genesis.configEpochSlots gc)
    $ give gdProtocolMagicId $ runWalletClient p tracer'

cvtRNM :: RequireNetworkMagic -> RequiresNetworkMagic
cvtRNM =
  \case
    NoRequireNetworkMagic -> RequiresNoMagic
    RequireNetworkMagic -> RequiresMagic

runWalletClient :: forall blk cfg.
                   ( RunNode blk
                   , Node.TraceConstraints blk
                   , blk ~ ByronBlockOrEBB cfg
                   )
                => Ouroboros.Consensus.Protocol.Protocol blk
                -> Tracer IO String
                -> IO ()
runWalletClient ptcl tracer = do
  let
    ProtocolInfo{pInfoConfig} = protocolInfo (NumCoreNodes 7) (CoreNodeId 0) ptcl

    path = localSocketFilePath (CoreNodeId 42)
    addr = localSocketAddrInfo path

    chainSyncTracer :: Tracer IO (TraceSendRecv (ChainSync blk (Point blk)) Peer DeserialiseFailure)
    --chainSyncTracer = contramap show tracer
    chainSyncTracer = nullTracer
    localTxSubmissionTracer :: Tracer IO (TraceSendRecv (LocalTxSubmission (GenTx blk) String) Peer DeserialiseFailure)
    localTxSubmissionTracer = contramap show tracer

  print path
  connectTo
    nullTracer
    Peer
    (localInitiatorNetworkApplication
      (Proxy :: Proxy blk)
      chainSyncTracer
      localTxSubmissionTracer
      pInfoConfig)
    Nothing
    addr

localSocketFilePath :: CoreNodeId -> FilePath
localSocketFilePath (CoreNodeId  n) = "node-core-" ++ show n ++ ".socket"

localSocketAddrInfo :: FilePath -> AddrInfo
localSocketAddrInfo socketPath =
  AddrInfo [] AF_UNIX Stream defaultProtocol (SockAddrUnix socketPath) Nothing

localInitiatorNetworkApplication
  :: forall blk m peer cfg.
     (RunNode blk, MonadST m, MonadThrow m, MonadTimer m, MonadIO m, blk ~ ByronBlockOrEBB cfg)
  -- TODO: the need of a 'Proxy' is an evidence that blk type is not really
  -- needed here.  The wallet client should use some concrete type of block
  -- from 'cardano-chain'.  This should remove the dependency of this module
  -- from 'ouroboros-consensus'.
  => Proxy blk
  -> Tracer m (TraceSendRecv (ChainSync blk (Point blk)) peer DeserialiseFailure)
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (Ouroboros.Consensus.Ledger.Byron.GenTx blk) String) peer DeserialiseFailure)
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> Ouroboros.Consensus.Protocol.NodeConfig (BlockProtocol blk)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication 'InitiatorApp peer NodeToClientProtocols
                                    m BSL.ByteString Void Void)
localInitiatorNetworkApplication Proxy chainSyncTracer localTxSubmissionTracer pInfoConfig =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = 0 })
      (DictVersion nodeToClientCodecCBORTerm)

  $ OuroborosInitiatorApplication $ \peer ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel -> do
        txv <- newEmptyTMVarM @_ @(GenTx blk)
        runPeer
          localTxSubmissionTracer
          localTxSubmissionCodec
          peer
          channel
          (localTxSubmissionClientPeer
              (txSubmissionClient @(GenTx blk) txv))

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          chainSyncTracer
          (localChainSyncCodec @blk pInfoConfig)
          peer
          channel
          (chainSyncClientPeer chainSyncClient)

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'TMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: forall tx reject m.
     ( Monad    m
     , MonadSTM m
     )
  => TMVar m tx
  -> m (LocalTxSubmissionClient tx reject m Void)
txSubmissionClient txv = do
    tx <- atomically $ readTMVar txv
    pure $ SendMsgSubmitTx tx $ \mbreject -> do
      case mbreject of
        Nothing -> return ()
        Just _r -> return ()
      txSubmissionClient txv

localChainSyncCodec
  :: forall blk m. (RunNode blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m BSL.ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (nodeEncodeBlock pInfoConfig)
      (nodeDecodeBlock pInfoConfig)
      (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))

localTxSubmissionCodec
  :: (RunNode blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) String)
           DeserialiseFailure m BSL.ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    Serialise.encode
    Serialise.decode


blockToDifficulty :: ByronBlockOrEBB cfg -> ChainDifficulty
blockToDifficulty blk =
  case (unByronBlockOrEBB blk) of
    ABOBBlock (ABlock header _body _annotation) -> (unAnnotated . aHeaderDifficulty) header
    ABOBBoundary (ABoundaryBlock _len header _body _annotation) -> boundaryDifficulty header

-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
chainSyncClient
  :: forall blk m cfg. (MonadTimer m, MonadIO m, StandardHash blk, blk ~ ByronBlockOrEBB cfg)
  => ChainSyncClient blk (Point blk) m Void
chainSyncClient = ChainSyncClient $ pure $
    -- Notify the core node about the our latest points at which we are
    -- synchronised.  This client is not persistent and thus it just
    -- synchronises from the genesis block.  A real implementation should send
    -- a list of points up to a point which is k blocks deep.
    SendMsgFindIntersect
      [genesisPoint] -- TODO
      ClientStIntersect
        { recvMsgIntersectFound    = \_ _ -> ChainSyncClient (pure clientStIdle)
        , recvMsgIntersectNotFound = \  _ -> ChainSyncClient (pure clientStIdle)
        }
  where
    clientStIdle :: ClientStIdle blk (Point blk) m Void
    clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext blk (Point blk) m Void
    clientStNext =
      ClientStNext
        { recvMsgRollForward = \blk tip -> ChainSyncClient $ do
            print $ sformat ("applying block at depth " % int) ((unChainDifficulty . blockToDifficulty) blk)
            print tip
            pure clientStIdle
        , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
            print tip
            -- we are requested to roll backward to point '_point', the core
            -- node's chain's tip is '_tip'.
            pure clientStIdle
        }
