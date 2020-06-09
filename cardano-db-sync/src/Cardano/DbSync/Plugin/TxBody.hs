module Cardano.DbSync.Plugin.TxBody
  ( insertTxBody
  ) where
import           Cardano.Binary (serialize')
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Common as Ledger
import qualified Cardano.Chain.UTxO as Ledger


insertTxBody :: Trace IO Text -> ByronBlock -> Tip ByronBlock
             -> ReadterT SqlBackend (LoggingT IO) (Either DBSyncNodeError ())
insertTxBody tracer blk tip = do
  runExceptT $ 
    case byronBlockRaw blk of
      Ledger.ABOBBlock ablk -> insert
      _ -> return ()
    
