module Cardano.TxSubmit
  ( module X
  , runTxSubmitWebapi
  ) where

import           Cardano.TxSubmit.Config as X
import           Cardano.TxSubmit.Node as X
import           Cardano.TxSubmit.Tx as X
import           Cardano.TxSubmit.Types as X
import           Cardano.TxSubmit.Web as X

import qualified Control.Concurrent.Async as Async

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp =
    loop
  where
    loop :: IO ()
    loop = do
      tsv <- newTxSubmitVar
      Async.race_
        (Async.async $ runTxSubmitNode tsv tsnp)
        (Async.async $ runTxSubmitServer tsv (tspWebPort tsnp))
      loop
