{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.TxSubmit.Node
  ( ConfigFile (..)
  , TxSubmitNodeParams (..)
  , GenesisFile (..)
  , GenesisHash (..)
  , SocketPath (..)
  , runTxSubmitNode
  ) where

import           Cardano.Binary (unAnnotated)

import           Control.Tracer (Tracer)

import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import qualified Cardano.Crypto as Crypto

import           Cardano.TxSubmit.Config
import           Cardano.TxSubmit.Tracing.ToObjectOrphans ()
import           Cardano.TxSubmit.Metrics
import           Cardano.TxSubmit.Tx
import           Cardano.TxSubmit.Types
import           Cardano.TxSubmit.Util

import           Cardano.Prelude hiding (atomically, option, (%), Nat)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Network.Socket (SockAddr (..))

import           Network.TypedProtocol.Channel (Channel)
import           Network.TypedProtocol.Codec (Codec)
import           Network.TypedProtocol.Codec.Cbor (DeserialiseFailure)
import           Network.TypedProtocol.Driver (runPeer)

import           Ouroboros.Consensus.Ledger.Abstract (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..), GenTx)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig, protocolInfo)
import           Ouroboros.Consensus.Node.Run.Abstract (RunNode, nodeDecodeGenTx, nodeEncodeGenTx,
                    nodeEncodeApplyTxError, nodeDecodeApplyTxError, nodeNetworkMagic)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import           Ouroboros.Consensus.Protocol (NodeConfig, Protocol (..))

import           Ouroboros.Network.Mux (AppType (..), OuroborosApplication (..))
import           Ouroboros.Network.NodeToClient (ErrorPolicyTrace (..), IPSubscriptionTarget (..),
                    LocalAddresses (..), NodeToClientProtocols (..), NetworkIPSubscriptionTracers (..),
                    NodeToClientVersionData (..), SubscriptionParams (..), WithAddr (..),
                    ncSubscriptionWorker_V1, networkErrorPolicies, newNetworkMutableState)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (LocalTxSubmissionClient (..),
                    LocalTxClientStIdle (..), localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Peer = Peer SockAddr SockAddr deriving Show

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspGenesisFile :: !GenesisFile
  , tspSocketPath :: !SocketPath
  , tspWebPort :: !TxSubmitPort
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


runTxSubmitNode :: TxSubmitVar -> Trace IO Text -> Genesis.Config -> SocketPath -> IO ()
runTxSubmitNode tsv trce gc socket = do
  logInfo trce "Running tx-submit node"
  logException trce "tx-submit-node." $ do
    logProtocolMagic trce $ Ledger.configProtocolMagic gc
    void $ runTxSubmitNodeClient tsv (mkNodeConfig gc) trce socket


mkNodeConfig :: Genesis.Config -> NodeConfig (BlockProtocol ByronBlock)
mkNodeConfig gc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT gc Nothing (Update.ProtocolVersion 0 2 0)
      (Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1) Nothing

runTxSubmitNodeClient
  :: forall blk. (blk ~ ByronBlock)
  => TxSubmitVar -> NodeConfig (BlockProtocol blk)
  -> Trace IO Text -> SocketPath
  -> IO Void
runTxSubmitNodeClient tsv nodeConfig trce (SocketPath socketPath) = do
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
    (localInitiatorNetworkApplication trce tsv)
  where
    errorPolicyTracer :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace)
    errorPolicyTracer = toLogObject $ appendName "ErrorPolicy" trce

localInitiatorNetworkApplication
  :: forall peer.
     (Show peer)
  => Trace IO Text
  -> TxSubmitVar
  -> OuroborosApplication 'InitiatorApp peer NodeToClientProtocols IO LBS.ByteString Void Void
localInitiatorNetworkApplication trce tsv =
  OuroborosInitiatorApplication $ \peer ptcl ->
    case ptcl of
      ChainSyncWithBlocksPtcl ->
        nullChainSyncWithBlocksPtcl

      LocalTxSubmissionPtcl -> \channel ->
        logException trce "LocalTxSubmissionPtcl: " $ do
          (metrics, server) <- registerMetricsServer
          ret <- runPeer
                    (contramap (Text.pack . show) . toLogObject $ appendName "cardano-tx-submit" trce)
                    localTxSubmissionCodec peer channel
                    (localTxSubmissionClientPeer (txSubmissionClient tsv metrics))
          cancel server
          pure ret

-- | This should be provided by ouroboros-network.
nullChainSyncWithBlocksPtcl :: Channel IO LBS.ByteString -> IO Void
nullChainSyncWithBlocksPtcl =
  const . forever $ threadDelay (1000 * 1000 * 1000)

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'StrictTMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: TxSubmitVar -> TxSubmitMetrics
  -> LocalTxSubmissionClient (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO Void
txSubmissionClient tsv metrics =
    LocalTxSubmissionClient $
      readTxSubmit tsv >>= pure . loop
  where
    loop :: GenTx ByronBlock -> LocalTxClientStIdle (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO Void
    loop tx =
      SendMsgSubmitTx tx $ \mbreject -> do
        case mbreject of
          Nothing -> liftIO $ Gauge.inc (tsmCount metrics)
          Just _r -> return ()
        writeTxSubmitResponse tsv mbreject
        nextTx <- readTxSubmit tsv
        pure $ loop nextTx

localTxSubmissionCodec
  :: forall m blk . (RunNode blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) DeserialiseFailure m LBS.ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    (nodeEncodeApplyTxError (Proxy @blk))
    (nodeDecodeApplyTxError (Proxy @blk))

logProtocolMagic :: Trace IO Text -> Crypto.ProtocolMagic -> IO ()
logProtocolMagic tracer pm =
  liftIO . logInfo tracer $ mconcat
    [ "NetworkMagic: ", textShow (Crypto.getRequiresNetworkMagic pm), " "
    , textShow (Crypto.unProtocolMagicId . unAnnotated $ Crypto.getAProtocolMagicId pm)
    ]
