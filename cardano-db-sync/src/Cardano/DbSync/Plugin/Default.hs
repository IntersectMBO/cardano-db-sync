module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.Prelude
import           Cardano.BM.Trace (Trace)

import           Cardano.DbSync.Config
import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Era.Byron.Insert as Byron
import qualified Cardano.DbSync.Era.Shelley.Insert as Shelley
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Rollback (rollbackToSlot)
import           Cardano.DbSync.Types

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)



-- | The default DbSyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: DbSyncNodePlugin
defDbSyncNodePlugin =
  DbSyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock]
    , plugRollbackBlock = [rollbackToSlot]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: Trace IO Text -> DbSyncEnv -> BlockDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertDefaultBlock tracer env blkTip = do
  case blkTip of
    ByronBlockDetails blk details ->
      Byron.insertByronBlock tracer blk details
    ShelleyBlockDetails blk details ->
      Shelley.insertShelleyBlock tracer env blk details
