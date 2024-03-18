{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbSync.OffChain.Vote.Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

data OffChainVoteData
  = OffChainVoteDataOther (OffChainVoteDataTp OtherOffChainData)
  | OffChainVoteDataGa (OffChainVoteDataTp GovernanceOffChainData)
  deriving (Show)

getMinimalBody :: OffChainVoteData -> MinimalBody
getMinimalBody = \case
  OffChainVoteDataOther dt -> body dt
  OffChainVoteDataGa dt -> minimalBody $ body dt

getTitle :: OffChainVoteData -> Maybe Text
getTitle = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ title $ body ga

getAbstract :: OffChainVoteData -> Maybe Text
getAbstract = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ abstract $ body ga

getMotivation :: OffChainVoteData -> Maybe Text
getMotivation = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ motivation $ body ga

getRationale :: OffChainVoteData -> Maybe Text
getRationale = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ rationale $ body ga

eitherDecodeOffChainVoteData :: LBS.ByteString -> Bool -> Either String OffChainVoteData
eitherDecodeOffChainVoteData lbs isGa
  | isGa = OffChainVoteDataGa <$> eitherDecode' lbs
  | otherwise = OffChainVoteDataOther <$> eitherDecode' lbs

getAuthors :: OffChainVoteData -> [Author]
getAuthors = \case
  OffChainVoteDataOther dt -> authors dt
  OffChainVoteDataGa dt -> authors dt

getHashAlgorithm :: OffChainVoteData -> Text
getHashAlgorithm = \case
  OffChainVoteDataOther dt -> hashAlgorithm dt
  OffChainVoteDataGa dt -> hashAlgorithm dt

getLanguage :: OffChainVoteData -> Text
getLanguage = \case
  OffChainVoteDataOther dt -> language $ context dt
  OffChainVoteDataGa dt -> language $ context dt

data OffChainVoteDataTp tp = OffChainVoteDataTp
  { hashAlgorithm :: Text
  , authors :: [Author]
  , body :: Body tp
  , context :: Context
  }

deriving instance (Show (Body tp)) => Show (OffChainVoteDataTp tp)
deriving instance Generic (OffChainVoteDataTp tp)

data Author = Author
  { name :: Maybe Text
  , witness :: Witness
  }
  deriving (Show, Generic, FromJSON)

data Witness = Witness
  { witnessAlgorithm :: Text
  , publicKey :: Text
  , signature :: Text
  }
  deriving (Show, Generic, FromJSON)

data MinimalBody = Body
  { references :: Maybe [Reference]
  , comment :: Maybe Text
  , externalUpdates :: Maybe [ExternalUpdate]
  }
  deriving (Show, Generic, FromJSON)

data GABody = GABody
  { minimalBody :: MinimalBody
  , title :: Text -- 80 chars max
  , abstract :: Text -- 2500 chars
  , motivation :: Text
  , rationale :: Text
  }
  deriving (Show)

data Reference = Reference
  { rtype :: Text -- key is @type. It can be "GovernanceMetadata" or "Other" or ?? "other" ??
  , label :: Text
  , uri :: Text
  , referenceHash :: Maybe ReferenceHash
  }
  deriving (Show, Generic)

data ReferenceHash = ReferenceHash
  { hashDigest :: Text
  , rhHashAlgorithm :: Text -- key 'hashAlgorithm'
  }
  deriving (Show, Generic)

data ExternalUpdate = ExternalUpdate
  { euTitle :: Text -- key 'title'
  , euUri :: Text -- key 'uri'
  }
  deriving (Show, Generic)

data OtherOffChainData = OtherOffChainData
data GovernanceOffChainData = OffChainData

class HasBody tp where
  type Body tp
  toMinimal :: Body tp -> MinimalBody

instance HasBody OtherOffChainData where
  type Body OtherOffChainData = MinimalBody
  toMinimal = id

instance HasBody GovernanceOffChainData where
  type Body GovernanceOffChainData = GABody
  toMinimal = minimalBody

instance FromJSON (Body tp) => FromJSON (OffChainVoteDataTp tp) where
  parseJSON =
    withObject "offChainVoteDataTp" $ \o ->
      OffChainVoteDataTp
        <$> o .: "hashAlgorithm"
        <*> o .: "authors"
        <*> o .: "body"
        <*> o .:? "@context" .!= defaultContext

instance FromJSON Reference where
  parseJSON =
    withObject "reference" $ \o ->
      Reference
        <$> parseRefType o
        <*> o .: "label"
        <*> o .: "uri"
        <*> o .:? "referenceHash"

instance FromJSON ReferenceHash where
  parseJSON =
    withObject "referenceHash" $ \o ->
      ReferenceHash
        <$> o .: "hashDigest"
        <*> o .: "hashAlgorithm"

parseRefType :: Object -> Parser Text
parseRefType obj = do
  tp <- obj .: "@type"
  if tp `elem` ["GovernanceMetadata", "Other", "other"]
    then pure tp
    else
      fail $
        mconcat
          [ "reference type should be GovernanceMetadata or Other other"
          , " but it's "
          , Text.unpack tp
          ]

instance FromJSON ExternalUpdate where
  parseJSON =
    withObject "ExternalUpdate" $ \o ->
      ExternalUpdate
        <$> o .: "title"
        <*> o .: "uri"

instance FromJSON GABody where
  parseJSON v = do
    mBody <- parseJSON v
    withObjectV v "GABody" $ \o ->
      GABody mBody
        <$> parseTextLimit 80 "title" o
        <*> parseTextLimit 2500 "abstract" o
        <*> o .: "motivation"
        <*> o .: "rationale"
    where
      withObjectV v' s p = withObject s p v'

parseTextLimit :: Int -> Key -> Object -> Parser Text
parseTextLimit maxSize str o = do
  txt <- o .: str
  if Text.length txt <= maxSize
    then pure txt
    else
      fail $
        mconcat
          [ show str
          , " must have at most "
          , show maxSize
          , "characters, but it has "
          , show (Text.length txt)
          , " characters."
          ]

newtype Context = Context
  { language :: Text -- keys is "@language"
  }
  deriving (Show)

defaultContext :: Context
defaultContext = Context "en-us"

instance FromJSON Context where
  parseJSON =
    withObject "Context" $ \o ->
      Context
        <$> o .: "@language"
