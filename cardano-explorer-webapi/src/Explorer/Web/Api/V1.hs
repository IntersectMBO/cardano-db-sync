{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Explorer.Web.Api.V1
  ( ExplorerApi1Record (..)
  , V1Utxo (..)
  ) where

import           GHC.Generics (Generic)

import           Servant.API ((:>), Get, JSON, QueryParam)
import           Servant.API.Generic ((:-))
import           Data.Word (Word64)

import           Explorer.Web.Error (ExplorerError)
import           Explorer.Web.ClientTypes (CHash)
import           Explorer.Web.Api.V1.Types (V1Utxo (..))

data ExplorerApi1Record route = ExplorerApi1Record
  { _utxoHeight
        :: route
        :- "utxo"
        :> QueryParam "block" Word64
        :> Get '[JSON] (Either ExplorerError [V1Utxo])
  , _utxoHash
        :: route
        :- "utxo"
        :> QueryParam "block" CHash
        :> Get '[JSON] (Either ExplorerError [V1Utxo])
  } deriving Generic
