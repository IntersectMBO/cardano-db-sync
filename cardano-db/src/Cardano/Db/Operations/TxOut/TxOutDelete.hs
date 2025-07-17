{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Operations.TxOut.TxOutDelete where

import Cardano.Db.Operations.Types (TxOutVariantType (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
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
deleteCoreTxOutTablesAfterTxId :: MonadIO m => Maybe VC.TxOutId -> Maybe VC.MaTxOutId -> ReaderT SqlBackend m ()
deleteCoreTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [VC.MaTxOutId >=. maTxOutId]
  whenJust mtxOutId $ \txOutId -> deleteWhere [VC.TxOutId >=. txOutId]

deleteVariantTxOutTablesAfterTxId :: MonadIO m => Maybe VA.TxOutId -> Maybe VA.MaTxOutId -> ReaderT SqlBackend m ()
deleteVariantTxOutTablesAfterTxId mtxOutId mmaTxOutId = do
  whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [VA.MaTxOutId >=. maTxOutId]
  whenJust mtxOutId $ \txOutId -> deleteWhere [VA.TxOutId >=. txOutId]

deleteTxOut :: MonadIO m => TxOutVariantType -> ReaderT SqlBackend m Int64
deleteTxOut = \case
  TxOutVariantCore -> deleteWhereCount ([] :: [Filter VC.TxOut])
  TxOutVariantAddress -> deleteWhereCount ([] :: [Filter VA.TxOut])
