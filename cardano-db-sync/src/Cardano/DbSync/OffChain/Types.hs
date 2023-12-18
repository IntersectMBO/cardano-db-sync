{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.Types (
  PoolDescription (..),
  PoolHomepage (..),
  PoolOffChainMetadata (..),
  PoolName (..),
  PoolTicker (..),
) where

import Cardano.Prelude
import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Swagger (ToSchema (..))

newtype PoolDescription = PoolDescription
  { unPoolDescription :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolDescription

newtype PoolHomepage = PoolHomepage
  { unPoolHomepage :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolHomepage

-- | The bit of the pool data off the chain.
data PoolOffChainMetadata = PoolOffChainMetadata
  { pomName :: !PoolName
  , pomDescription :: !PoolDescription
  , pomTicker :: !PoolTicker
  , pomHomepage :: !PoolHomepage
  }
  deriving (Eq, Show, Ord, Generic)

newtype PoolName = PoolName
  { unPoolName :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolName

newtype PoolTicker = PoolTicker
  { unPoolTicker :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolTicker

instance FromJSON PoolOffChainMetadata where
  parseJSON =
    withObject "poolOffChainMetadata" $ \o ->
      PoolOffChainMetadata
        <$> parseName o
        <*> parseDescription o
        <*> parseTicker o
        <*> fmap PoolHomepage (o .: "homepage")

-- | We presume the validation is not required the other way around?
instance ToJSON PoolOffChainMetadata where
  toJSON (PoolOffChainMetadata name' description' ticker' homepage') =
    object
      [ "name" .= unPoolName name'
      , "description" .= unPoolDescription description'
      , "ticker" .= unPoolTicker ticker'
      , "homepage" .= unPoolHomepage homepage'
      ]

instance ToSchema PoolOffChainMetadata

-- -------------------------------------------------------------------------------------------------

-- Copied from https://github.com/IntersectMBO/cardano-node/pull/1299

-- | Parse and validate the stake pool metadata name from a JSON object.
--
-- If the name consists of more than 50 characters, the parser will fail.
parseName :: Object -> Parser PoolName
parseName obj = do
  name <- obj .: "name"
  if length name <= 50
    then pure $ PoolName name
    else
      fail $
        mconcat
          [ "\"name\" must have at most 50 characters, but it has "
          , show (length name)
          , " characters."
          ]

-- | Parse and validate the stake pool metadata description from a JSON
-- object.
--
-- If the description consists of more than 255 characters, the parser will
-- fail.
parseDescription :: Object -> Parser PoolDescription
parseDescription obj = do
  description <- obj .: "description"
  if length description <= 255
    then pure $ PoolDescription description
    else
      fail $
        mconcat
          [ "\"description\" must have at most 255 characters, but it has "
          , show (length description)
          , " characters."
          ]

-- | Parse and validate the stake pool ticker description from a JSON object.
--
-- If the ticker consists of less than 3 or more than 5 characters, the parser
-- will fail.
parseTicker :: Object -> Parser PoolTicker
parseTicker obj = do
  ticker <- obj .: "ticker"
  let tickerLen = length ticker
  if tickerLen >= 3 && tickerLen <= 5
    then pure $ PoolTicker ticker
    else
      fail $
        mconcat
          [ "\"ticker\" must have at least 3 and at most 5 "
          , "characters, but it has "
          , show (length ticker)
          , " characters."
          ]
