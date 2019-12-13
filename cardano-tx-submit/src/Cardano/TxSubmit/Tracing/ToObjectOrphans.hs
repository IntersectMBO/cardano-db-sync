{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.TxSubmit.Tracing.ToObjectOrphans () where

import           Data.Text
import           Data.Aeson ((.=))

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Tracer

import qualified Network.Socket as Socket
import           Ouroboros.Network.NodeToClient (WithAddr(..), ErrorPolicyTrace(..))

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
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage $ pack $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= ("ErrorPolicyTrace" :: String)
             , "address" .= show addr
             , "event" .= show ev ]
