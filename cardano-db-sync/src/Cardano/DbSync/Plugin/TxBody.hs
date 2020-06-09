{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Plugin.TxBody
  ( defDbSyncTxBodyPlugin
  )
where
import           Cardano.Binary                 ( serialize' )
import           Cardano.BM.Trace               ( Trace
                                                , logInfo
                                                )

import           Control.Monad.Logger           ( LoggingT )
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Trans.Reader     ( ReaderT )

import qualified Cardano.Chain.Block           as Ledger
import qualified Cardano.Chain.UTxO            as Ledger

import           Cardano.Crypto                 ( serializeCborHash )

import qualified Cardano.Db                    as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Util

import           Cardano.Prelude


import           Database.Persist.Sql           ( SqlBackend )

import           Ouroboros.Consensus.Byron.Ledger
                                                ( ByronBlock(..) )
import           Ouroboros.Network.Block        ( Tip )


insertTxBody
  :: Trace IO Text
  -> ByronBlock
  -> Tip ByronBlock
  -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertTxBody tracer blk _ = runExceptT $ case byronBlockRaw blk of
  Ledger.ABOBBlock ablk -> insertBlock ablk
  _                     -> return ()
 where
  insertBlock
    :: MonadIO m
    => Ledger.ABlock ByteString
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
  insertBlock ablk = do
    liftIO . logInfo tracer $ mconcat
      [ "doing transactions for ABlock "
      , textShow (blockNumber ablk)
      , ", hash "
      , renderByteArray (blockHash ablk)
      ]
    mapM_ insertTx $ blockPayload ablk

  insertTx
    :: MonadIO m
    => Ledger.TxAux
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
  insertTx tx =
    let hash = unTxHash $ serializeCborHash (Ledger.taTx tx)
        body = serialize' $ Ledger.taTx tx
    in  void $ lift . DB.insertTxBody $ DB.TxBody { DB.txBodyHash = hash
                                                  , DB.txBodyBody = body
                                                  }

defDbSyncTxBodyPlugin :: DbSyncNodePlugin
defDbSyncTxBodyPlugin = DbSyncNodePlugin { plugOnStartup     = []
                                         , plugInsertBlock   = [insertTxBody]
                                         , plugRollbackBlock = []
                                         }
