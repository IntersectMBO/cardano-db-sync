{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.SMASH.Server.Orphans where

import           Cardano.Prelude

import           Data.Swagger (NamedSchema (..), Schema (..), SwaggerType (..), ToParamSchema (..),
                   ToSchema (..), declareSchemaRef, properties, required, type_)

import           Cardano.Db

-- instance ToSchema LookupFail where
--   declareNamedSchema _ =
--     return $ NamedSchema (Just "DBFail") $ mempty
