module Cardano.DbSync.Api.Functions (
  getSeverity,
) where

import qualified Cardano.BM.Configuration.Model as BM
import qualified Cardano.BM.Data.Severity as BM
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Config.Types (SyncNodeConfig (..))

getSeverity :: SyncEnv -> IO BM.Severity
getSeverity = BM.minSeverity . dncLoggingConfig . envSyncNodeConfig
