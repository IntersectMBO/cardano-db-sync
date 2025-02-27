{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cardano.Db.Statement.EpochAndProtocol where

import Cardano.Db.Schema.Core.EpochAndProtocol (Epoch(..), epochEncoder)
import Cardano.Db.Schema.Ids (idDecoder, EpochId (..))
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, insertCheckUnique)
import Cardano.Db.Types (DbAction, DbTxMode (..))
import Cardano.Prelude (MonadIO)
import qualified Hasql.Decoders as HsqlD

insertEpoch:: MonadIO m => Epoch -> DbAction m EpochId
insertEpoch epoch = runDbT Write $ mkDbTransaction "insertEpoch" $
  insertCheckUnique
        "unique_epoch_no"
        epochEncoder
        (WithResult (HsqlD.singleRow $ idDecoder EpochId))
        epoch

-- Epoch And Protocol Parameters
-- These tables store epoch-specific data and protocol parameters.

-- epoch
-- epoch_param
-- epoch_state
-- epoch_sync_time
-- ada_pots
-- treasury
-- reserve
-- pot_transfer
