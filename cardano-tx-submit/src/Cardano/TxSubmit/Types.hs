{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitError (..)
  , TxSubmitPort (..)
  ) where

import           Data.Aeson (ToJSON (..), Value (..))

import           GHC.Generics (Generic)

import           Servant (JSON, Post)

import           Servant.API ((:>))
import           Servant.API.Generic (ToServantApi, (:-))

newtype TxSubmitPort
  = TxSubmitPort Int

data TxSubmitError
  = TxSubmitError

instance ToJSON TxSubmitError where
  toJSON _ = String "ToJSON TxSubmitError"

-- | Servant API which provides access to explorer
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
data TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> Post '[JSON] (Either TxSubmitError Bool)
  } deriving (Generic)
