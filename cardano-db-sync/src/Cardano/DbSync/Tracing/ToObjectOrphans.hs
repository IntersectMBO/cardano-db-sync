{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text
import           Data.Aeson ((.=))

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Tracing

import           Control.Monad.IO.Class (MonadIO)

import qualified Network.Socket as Socket
import           Ouroboros.Network.NodeToClient
                   (WithAddr(..), ErrorPolicyTrace(..))

instance DefinePrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)
instance DefineSeverity (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  defineSeverity (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error


-- transform @ErrorPolicyTrace@
instance Transformable Text IO (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  trTransformer = defaultTextTransformer

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= ("ErrorPolicyTrace" :: String)
             , "address" .= show addr
             , "event" .= show ev ]

defaultTextTransformer
  :: ( MonadIO m
     , DefinePrivacyAnnotation b
     , DefineSeverity b
     , Show b
     , ToObject b)
  => TracingFormatting
  -> TracingVerbosity
  -> Trace m Text
  -> Tracer m b
defaultTextTransformer TextualRepresentation _verb tr =
  Tracer $ \s -> do
    meta <- mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ pack $ show s))
defaultTextTransformer _ verb tr =
  trStructured verb tr
