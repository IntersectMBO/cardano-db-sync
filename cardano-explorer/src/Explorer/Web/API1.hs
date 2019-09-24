{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Explorer.Web.API1 
  ( ExplorerApi1Record(ExplorerApi1Record,_utxoHeight, _utxoHash)
  , V1Utxo(V1Utxo, cuId,cuOutIndex,cuAddress,cuCoins)
  ) where

import           GHC.Generics             (Generic)

import           Servant.API              ((:>), Get, JSON, QueryParam)
import           Servant.API.Generic      ((:-))
import           Data.Aeson.TH             (defaultOptions, deriveToJSON)
import           Data.Word (Word64, Word16)

import           Explorer.Web.Error       (ExplorerError)
import           Explorer.Web.ClientTypes (CTxHash, CAddress, CCoin, CHash)

data V1Utxo = V1Utxo
    { cuId       :: !CTxHash
    , cuOutIndex :: !Word16
    , cuAddress  :: !CAddress
    , cuCoins    :: !CCoin
    } deriving (Show, Generic)

deriveToJSON defaultOptions ''V1Utxo

data ExplorerApi1Record route = ExplorerApi1Record
  { _utxoHeight :: route :- "utxo" :> QueryParam "block" Word64 :> Get '[JSON] (Either ExplorerError [V1Utxo])
  , _utxoHash :: route :- "utxo" :> QueryParam "block" CHash :> Get '[JSON] (Either ExplorerError [V1Utxo])
  } deriving Generic
