{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Operations.TxOut.TxOutDelete where

import Cardano.Db.Operations.Types (TxOutTableType (..))
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Prelude (Int64)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class.PersistQuery (deleteWhere)
import Database.Persist.Sql (
  Filter,
  SqlBackend,
  deleteWhereCount,
  (>=.),
 )

--------------------------------------------------------------------------------
-- Delete
--------------------------------------------------------------------------------
deleteCoreTxOutTablesAfterTxId :: MonadIO m => Maybe C.TxOutId -> Maybe C.MaTxOutId -> ReaderT SqlBackend m ()
deleteCoreTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [C.MaTxOutId >=. maTxOutId]
  whenJust mtxOutId $ \txOutId -> deleteWhere [C.TxOutId >=. txOutId]

deleteVariantTxOutTablesAfterTxId :: MonadIO m => Maybe V.TxOutId -> Maybe V.MaTxOutId -> ReaderT SqlBackend m ()
deleteVariantTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [V.MaTxOutId >=. maTxOutId]
  whenJust mtxOutId $ \txOutId -> deleteWhere [V.TxOutId >=. txOutId]

deleteTxOut :: MonadIO m => TxOutTableType -> ReaderT SqlBackend m Int64
deleteTxOut = \case
  TxOutCore -> deleteWhereCount ([] :: [Filter C.TxOut])
  TxOutVariantAddress -> deleteWhereCount ([] :: [Filter V.TxOut])
