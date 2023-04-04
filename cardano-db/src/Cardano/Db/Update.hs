
module Cardano.Db.Update (
  updateTxOutConsumedByTxInId,
  updateListTxOutConsumedByTxInId
) where

import Cardano.Db.Schema
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist ((=.))
import Database.Persist.Class (update)
import Database.Persist.Sql (SqlBackend)

updateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxInId = mapM_ (uncurry updateTxOutConsumedByTxInId)

updateTxOutConsumedByTxInId :: MonadIO m => TxOutId -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInId txOutId txInId =
    update txOutId [TxOutConsumedByTxInId =. Just txInId]
