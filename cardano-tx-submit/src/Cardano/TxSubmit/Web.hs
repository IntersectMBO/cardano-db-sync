{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import           Cardano.Binary (DecoderError)
import qualified Cardano.Binary as Binary

import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.TxSubmit.Tx
import           Cardano.TxSubmit.Types
import           Cardano.TxSubmit.Util

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)

import qualified Network.Wai.Handler.Warp as Warp

import           Ouroboros.Consensus.Ledger.Byron (GenTx, ByronBlock)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron

import           Servant (Application, Handler)
import qualified Servant as Servant

import           Servant.API.Generic (toServant)
import           Servant.Server.Generic (AsServerT)

runTxSubmitServer :: TxSubmitVar-> Trace IO Text -> TxSubmitPort -> IO ()
runTxSubmitServer tsv trce (TxSubmitPort port) = do
  logInfo trce $ "Running tx-submit web server on http://localhost:" <> textShow port <> "/"
  logException trce "tx-submit-webapi." $
    Warp.run port (txSubmitApp tsv trce)
  logInfo trce "txSubmitApp: exiting"

txSubmitApp :: TxSubmitVar-> Trace IO Text -> Application
txSubmitApp tsv trce =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost tsv trce
      }

txSubmitPost
    :: TxSubmitVar -> Trace IO Text -> ByteString
    -> Handler TxSubmitStatus
txSubmitPost tsv trce tx = do
  liftIO $ logInfo trce ("txSubmitPost: tx is " <> textShow (BS.length tx) <> " bytes")
  case decodeByronTx tx of
    Left err -> pure $ TxSubmitDecodeFail err
    Right tx1 -> do
      resp <- liftIO $ submitTx tsv tx1
      liftIO $ logInfo trce (textShow resp)
      case resp of
        Nothing -> pure TxSubmitOk
                    -- FFS, why is there not a way of pretty printing this????
        Just r -> pure $ TxSubmitFail (textShow r)

decodeByronTx :: ByteString -> Either DecoderError (GenTx ByronBlock)
decodeByronTx =
  Binary.decodeFullDecoder "Cardano.TxSubmit.Web.decodeByronTx" Byron.decodeByronGenTx
    . LBS.fromStrict
