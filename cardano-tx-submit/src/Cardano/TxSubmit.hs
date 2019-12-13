{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.TxSubmit
  ( module X
  , runTxSubmitWebapi
  ) where

import qualified Cardano.BM.Setup as Logging
import qualified Cardano.BM.Trace as Logging
import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Crypto (decodeAbstractHash)

import           Cardano.Prelude

import           Cardano.Shell.Lib (GeneralException (ConfigurationError))

import           Cardano.TxSubmit.Config as X
import           Cardano.TxSubmit.Node as X
import           Cardano.TxSubmit.Tx as X
import           Cardano.TxSubmit.Types as X
import           Cardano.TxSubmit.Util as X
import           Cardano.TxSubmit.Web as X

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)


runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
  tsnc <- readTxSubmitNodeConfig (unConfigFile $ tspConfigFile tsnp)
  gc <- readGenesisConfig tsnp tsnc
  trce <- mkTracer tsnc
  tsv <- newTxSubmitVar
  Async.race_
    (runTxSubmitNode tsv trce gc (tspSocketPath tsnp))
    (runTxSubmitServer tsv trce (tspWebPort tsnp))
  logInfo trce "runTxSubmitWebapi: Async.race_ returned"

mkTracer :: TxSubmitNodeConfig -> IO (Trace IO Text)
mkTracer enc =
  if not (tscEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "cardano-tx-submit"

readGenesisConfig :: TxSubmitNodeParams -> TxSubmitNodeConfig -> IO Genesis.Config
readGenesisConfig enp enc = do
    genHash <- either (throwIO . ConfigurationError) pure $
                decodeAbstractHash (unGenesisHash $ tscGenesisHash enc)
    convert =<< runExceptT (Genesis.mkConfigFromFile (tscRequiresNetworkMagic enc)
                            (unGenesisFile $ tspGenesisFile enp) genHash)
  where
    convert :: Either Genesis.ConfigurationError Genesis.Config -> IO Genesis.Config
    convert =
      \case
        Left err -> panic $ show err
        Right x -> pure x
