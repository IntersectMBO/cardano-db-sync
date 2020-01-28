{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)

import           Formatting (build, sformat)

import           GHC.Generics (Generic)

import           Network.HTTP.Media ((//))

import           Servant (Accept (..), JSON, MimeRender (..), MimeUnrender (..), Post, ReqBody, (:>))
import           Servant.API.Generic (ToServantApi, (:-))

newtype TxSubmitPort
  = TxSubmitPort Int

data TxSubmitStatus
  = TxSubmitOk
  | TxSubmitDecodeHex
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
        TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
        TxSubmitDecodeFail err -> sformat build err
        TxSubmitFail err -> err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
data TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[CBORStream] ByteString
        :> Post '[JSON] TxSubmitStatus
  } deriving (Generic)



data CBORStream

instance Accept CBORStream where
  contentType _ = "application" // "cbor"

instance MimeRender CBORStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right . id
