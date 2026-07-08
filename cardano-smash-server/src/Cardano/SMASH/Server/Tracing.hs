-- | Tracer setup for the SMASH server.
--
-- Log messages are plain 'LogMessage's (severity + text, see
-- "Cardano.Db.Log") routed through @trace-dispatcher@ to stdout.
module Cardano.SMASH.Server.Tracing (
  defaultSmashTraceConfig,
  mkSmashTracer,
) where

import Cardano.Db (LogMessage)
import Cardano.Logging (
  BackendConfig (..),
  DetailLevel (..),
  FormatLogging (..),
  SeverityS (..),
  Trace,
  TraceConfig,
  configureTracers,
  emptyConfigReflection,
  mkCardanoTracer,
  mkConfigurationWithFallback,
  standardTracer,
 )
import Cardano.Prelude

-- | Stdout only logging on 'Info' level.
defaultSmashTraceConfig :: TraceConfig
defaultSmashTraceConfig =
  mkConfigurationWithFallback Info DNormal (Stdout HumanFormatUncoloured)

-- | Build the stdout tracer for the SMASH server.
mkSmashTracer :: TraceConfig -> Text -> IO (Trace IO LogMessage)
mkSmashTracer traceConfig name = do
  stdoutTrace <- standardTracer
  configReflection <- emptyConfigReflection
  tracer <- mkCardanoTracer stdoutTrace mempty Nothing [name]
  configureTracers configReflection traceConfig [tracer]
  pure tracer
