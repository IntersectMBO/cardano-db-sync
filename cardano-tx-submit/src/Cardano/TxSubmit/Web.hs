module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import           Cardano.TxSubmit.Tx
import           Cardano.TxSubmit.Types

import           Data.Proxy (Proxy (..))

import qualified Network.Wai.Handler.Warp as Warp

import           Servant (Application, Handler)
import qualified Servant as Servant

import           Servant.API.Generic (toServant)
import           Servant.Server.Generic (AsServerT)

runTxSubmitServer :: TxSubmitVar (Maybe String) -> TxSubmitPort -> IO ()
runTxSubmitServer tsv (TxSubmitPort port) = do
  putStrLn $ "Running full server on http://localhost:" ++ show port ++ "/"
  Warp.run port (txSubmitApp tsv)

txSubmitApp :: TxSubmitVar (Maybe String) -> Application
txSubmitApp tsv =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost tsv
      }

txSubmitPost :: TxSubmitVar (Maybe String) -> Handler (Either TxSubmitError Bool)
txSubmitPost = undefined
