{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Byron.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Chain.Block as Byron
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Network.Block (BlockNo (..), Point (..))


rollbackToPoint :: Trace IO Text -> Point ByronBlock -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
    case Byron.pointToSlotHash point of
      Nothing -> pure $ Right ()
      Just (slot, hash) ->
          DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => SlotNo -> Byron.HeaderHash -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        liftIO . logInfo trce $
            mconcat
              [ "Byron: Rolling back to slot ", textShow (unSlotNo slot)
              , ", hash ", Byron.renderAbstractHash hash
              ]
        xs <- lift $ DB.queryBlockNosWithSlotNoGreater (unSlotNo slot)
        liftIO . logInfo trce $
            mconcat
              [ "Byron: Deleting blocks numbered: ", textShow (map unBlockNo xs)
              ]
        mapM_ (void . lift . DB.deleteCascadeBlockNo) xs
