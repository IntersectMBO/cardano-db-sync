module Cardano.DbSync.Threads.Promise where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Stake
import Cardano.DbSync.Cache.Types
import Cardano.DbSync.Util
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)

data Promise = 