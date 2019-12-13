{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitStatus (..)
  , TxSubmitPort (..)
  ) where

import           Cardano.Binary (DecoderError)

import           Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)

import           Formatting ((%), build, sformat)

import           GHC.Generics (Generic)

import           Servant (JSON, OctetStream, Post, ReqBody, (:>))
import           Servant.API.Generic (ToServantApi, (:-))

newtype TxSubmitPort
  = TxSubmitPort Int

data TxSubmitStatus
  = TxSubmitOk
  | TxSubmitDecodeFail DecoderError
  | TxSubmitFail Text
  deriving Eq

instance ToJSON TxSubmitStatus where
  toJSON = convertJson

convertJson :: TxSubmitStatus -> Value
convertJson st =
    Aeson.object
      [ ( "status", String (if st == TxSubmitOk then "success" else "fail") )
      , ( "errorMsg", String failMsg )
      ]
  where
    failMsg :: Text
    failMsg =
      case st of
        TxSubmitOk -> "No error"
        TxSubmitDecodeFail err -> sformat ("Decoding provided ByetString failed: " % build) err
        TxSubmitFail err -> err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
data TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[OctetStream] ByteString
        :> Post '[JSON] TxSubmitStatus
  } deriving (Generic)
