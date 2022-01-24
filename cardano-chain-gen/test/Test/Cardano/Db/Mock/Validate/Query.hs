module Test.Cardano.Db.Mock.Validate.Query where

import qualified Cardano.DB as DB

queryScriptOutputs :: MonadIO m => ReaderT SqlBackend m [TxOut]
queryScriptOutputs = do
    res <- select . from $ \tx_out -> do
        where_ (tx_out ^. AddressHasScript ==. val True)
        pure $ tx_out
        