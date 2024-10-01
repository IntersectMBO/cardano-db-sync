{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbSync.OffChain.Vote.Types where

import qualified Cardano.Db as DB
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

data OffChainVoteData
  = OffChainVoteDataOther (OffChainVoteDataTp OtherOffChainData)
  | OffChainVoteDataGa (OffChainVoteDataTp GovernanceOffChainData)
  | OffChainVoteDataDr (OffChainVoteDataTp DrepOffChainData)
  deriving (Show)

getMinimalBody :: OffChainVoteData -> MinimalBody OtherOffChainData
getMinimalBody = \case
  OffChainVoteDataOther dt -> body dt
  OffChainVoteDataGa dt -> coerceMinimalBody @GovernanceOffChainData $ toMinimal $ body dt
  OffChainVoteDataDr dt -> coerceMinimalBody @DrepOffChainData $ toMinimal $ body dt

getTitle :: OffChainVoteData -> Maybe Text
getTitle = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ textValue $ title $ body ga
  OffChainVoteDataDr _ -> Nothing

getAbstract :: OffChainVoteData -> Maybe Text
getAbstract = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ textValue $ abstract $ body ga
  OffChainVoteDataDr _ -> Nothing

getMotivation :: OffChainVoteData -> Maybe Text
getMotivation = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ textValue $ motivation $ body ga
  OffChainVoteDataDr _ -> Nothing

getRationale :: OffChainVoteData -> Maybe Text
getRationale = \case
  OffChainVoteDataOther _ -> Nothing
  OffChainVoteDataGa ga -> Just $ textValue $ rationale $ body ga
  OffChainVoteDataDr _ -> Nothing

eitherDecodeOffChainVoteData :: LBS.ByteString -> DB.AnchorType -> Either String OffChainVoteData
eitherDecodeOffChainVoteData lbs = \case
  DB.GovActionAnchor ->
    eitherDecodeAlternative (OffChainVoteDataGa <$> eitherDecode' lbs) (OffChainVoteDataOther <$> eitherDecode' lbs)
  DB.DrepAnchor ->
    eitherDecodeAlternative (OffChainVoteDataDr <$> eitherDecode' lbs) (OffChainVoteDataOther <$> eitherDecode' lbs)
  DB.VoteAnchor -> OffChainVoteDataOther <$> eitherDecode' lbs
  DB.CommitteeDeRegAnchor -> OffChainVoteDataOther <$> eitherDecode' lbs
  DB.ConstitutionAnchor -> Left "Unsupported Constitution metadata"
  DB.OtherAnchor -> OffChainVoteDataOther <$> eitherDecode' lbs

eitherDecodeAlternative :: Either String OffChainVoteData -> Either String OffChainVoteData -> Either String OffChainVoteData
eitherDecodeAlternative one two =
  case one of
    Right _ -> one
    Left err1 -> case two of
      Right _ -> two
      Left err2 -> Left $ err1 <> ", CIP-100:" <> err2

getAuthors :: OffChainVoteData -> [Author]
getAuthors = \case
  OffChainVoteDataOther dt -> authors dt
  OffChainVoteDataGa dt -> authors dt
  OffChainVoteDataDr dt -> authors dt

getHashAlgorithm :: OffChainVoteData -> Text
getHashAlgorithm = \case
  OffChainVoteDataOther dt -> textValue $ hashAlgorithm dt
  OffChainVoteDataGa dt -> textValue $ hashAlgorithm dt
  OffChainVoteDataDr dt -> textValue $ hashAlgorithm dt

getLanguage :: OffChainVoteData -> Text
getLanguage = \case
  OffChainVoteDataOther dt -> language $ context dt
  OffChainVoteDataGa dt -> language $ context dt
  OffChainVoteDataDr dt -> language $ context dt

data OffChainVoteDataTp tp = OffChainVoteDataTp
  { hashAlgorithm :: TextValue
  , authors :: [Author]
  , body :: Body tp
  , context :: Context
  }

newtype BoolValue = BoolValue {boolValue :: Bool}

instance Show BoolValue where
  show = show . boolValue

newtype TextValue = TextValue {textValue :: Text}

instance Show TextValue where
  show = show . textValue

deriving instance (Show (Body tp)) => Show (OffChainVoteDataTp tp)
deriving instance Generic (OffChainVoteDataTp tp)

data Author = Author
  { name :: Maybe TextValue
  , witness :: Witness
  }
  deriving (Show, Generic, FromJSON)

data Witness = Witness
  { witnessAlgorithm :: TextValue
  , publicKey :: TextValue
  , signature :: TextValue
  }
  deriving (Show, Generic, FromJSON)

data MinimalBody tp = Body
  { references :: Maybe [Reference tp]
  , comment :: Maybe TextValue
  , externalUpdates :: Maybe [ExternalUpdate]
  }
  deriving (Show, Generic, FromJSON)

coerceMinimalBody :: MinimalBody tp -> MinimalBody tp'
coerceMinimalBody mb =
  Body
    { references = map coerceReference <$> references mb
    , comment = comment mb
    , externalUpdates = externalUpdates mb
    }
  where
    coerceReference rf =
      Reference
        { rtype = rtype rf
        , label = label rf
        , uri = uri rf
        , referenceHash = referenceHash rf
        }

data GABody = GABody
  { minimalBodyGA :: MinimalBody GovernanceOffChainData
  , title :: TextValue -- 80 chars max
  , abstract :: TextValue -- 2500 chars
  , motivation :: TextValue
  , rationale :: TextValue
  }
  deriving (Show)

data DrepBody = DrepBody
  { minimalBodyDrep :: MinimalBody DrepOffChainData
  , paymentAddress :: Maybe TextValue
  , givenName :: TextValue -- 80 chars max
  , image :: Maybe Image
  , objectives :: Maybe TextValue -- 1000 chars max
  , motivations :: Maybe TextValue -- 1000 chars max
  , qualifications :: Maybe TextValue -- 1000 chars max
  , doNotList :: Maybe BoolValue
  }
  deriving (Show, Generic)

data Image = Image
  { content :: TextValue
  , msha256 :: Maybe TextValue
  }
  deriving (Show, Generic)

data ImageUrl = ImageUrl
  { contentUrl :: TextValue
  , sha256 :: TextValue
  }
  deriving (Show, Generic, FromJSON)

fromImageUrl :: ImageUrl -> Image
fromImageUrl img = Image (contentUrl img) (Just (sha256 img))

data Reference tp = Reference
  { rtype :: TextValue -- key is @type. It can be "GovernanceMetadata" or "Other" or ?? "other" ?? or ""
  , label :: TextValue
  , uri :: TextValue
  , referenceHash :: Maybe ReferenceHash
  }
  deriving (Show, Generic)

data ReferenceHash = ReferenceHash
  { hashDigest :: TextValue
  , rhHashAlgorithm :: TextValue -- key 'hashAlgorithm'
  }
  deriving (Show, Generic)

data ExternalUpdate = ExternalUpdate
  { euTitle :: TextValue -- key 'title'
  , euUri :: TextValue -- key 'uri'
  }
  deriving (Show, Generic)

data OtherOffChainData
data GovernanceOffChainData
data DrepOffChainData

class HasBody tp where
  type Body tp
  parseAuthors :: Object -> Parser [Author]
  refrenceTypes :: [Text]
  toMinimal :: Body tp -> MinimalBody tp

instance HasBody OtherOffChainData where
  type Body OtherOffChainData = MinimalBody OtherOffChainData
  parseAuthors o = o .: "authors"
  refrenceTypes = ["Other", "GovernanceMetadata"]
  toMinimal = id

instance HasBody GovernanceOffChainData where
  type Body GovernanceOffChainData = GABody
  parseAuthors o = o .: "authors"
  refrenceTypes = ["Other", "GovernanceMetadata"]
  toMinimal = minimalBodyGA

instance HasBody DrepOffChainData where
  type Body DrepOffChainData = DrepBody
  parseAuthors _ = pure []
  refrenceTypes = ["Other", "GovernanceMetadata", "Identity", "Link"]
  toMinimal = minimalBodyDrep

instance (HasBody tp, FromJSON (Body tp)) => FromJSON (OffChainVoteDataTp tp) where
  parseJSON =
    withObject "offChainVoteDataTp" $ \o ->
      OffChainVoteDataTp
        <$> o .: "hashAlgorithm"
        <*> parseAuthors @tp o
        <*> o .: "body"
        <*> o .:? "@context" .!= defaultContext

instance HasBody tp => FromJSON (Reference tp) where
  parseJSON =
    withObject "reference" $ \o ->
      Reference
        <$> parseRefType o (refrenceTypes @tp)
        <*> o .: "label"
        <*> o .: "uri"
        <*> o .:? "referenceHash"

instance FromJSON ReferenceHash where
  parseJSON =
    withObject "referenceHash" $ \o ->
      ReferenceHash
        <$> o .: "hashDigest"
        <*> o .: "hashAlgorithm"

parseRefType :: Object -> [Text] -> Parser TextValue
parseRefType obj typeKeys = do
  tp <- obj .: "@type"
  if textValue tp `elem` typeKeys
    then pure tp
    else
      fail $
        mconcat
          [ "reference type should be one of "
          , show typeKeys
          , " but it's "
          , Text.unpack (textValue tp)
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

instance FromJSON DrepBody where
  parseJSON v = do
    mBody <- parseJSON v
    withObjectV v "DrepBody" $ \o ->
      DrepBody mBody
        <$> o .:? "paymentAddress"
        <*> parseTextLimit 80 "givenName" o
        <*> o .:? "image"
        <*> parseTextLimitMaybe 1000 "objectives" o
        <*> parseTextLimitMaybe 1000 "motivations" o
        <*> parseTextLimitMaybe 1000 "qualifications" o
        <*> o .:? "doNotList"
    where
      withObjectV v' s p = withObject s p v'

instance FromJSON Image where
  parseJSON v = withObjectV v "Image" $ \o -> do
    curl <- o .: "contentUrl"
    case Text.stripPrefix "data:" (textValue curl) of
      Just ctb
        | (_, tb) <- Text.break (== '/') ctb
        , Text.isPrefixOf "/" tb
        , (_, b) <- Text.break (== ';') tb
        , Just imageData <- Text.stripPrefix ";base64," b ->
            pure $ Image (TextValue imageData) Nothing
      _ -> fromImageUrl <$> parseJSON v
    where
      withObjectV v' s p = withObject s p v'

parseTextLimit :: Int -> Key -> Object -> Parser TextValue
parseTextLimit maxSize str o = do
  txt <- o .: str
  if Text.length (textValue txt) <= maxSize
    then pure txt
    else
      fail $
        mconcat
          [ show str
          , " must have at most "
          , show maxSize
          , "characters, but it has "
          , show (Text.length (textValue txt))
          , " characters."
          ]

parseTextLimitMaybe :: Int -> Key -> Object -> Parser (Maybe TextValue)
parseTextLimitMaybe maxSize str o = do
  mtxt <- o .:? str
  case mtxt of
    Nothing -> pure Nothing
    Just txt | Text.length (textValue txt) <= maxSize -> pure $ Just txt
    Just txt ->
      fail $
        mconcat
          [ show str
          , " must have at most "
          , show maxSize
          , "characters, but it has "
          , show (Text.length (textValue txt))
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
        <$> o .:? "@language" .!= "en-us"

instance FromJSON TextValue where
  parseJSON v = case v of
    String txt -> pure $ TextValue txt
    Object o -> TextValue <$> (o .: "@value")
    _ -> fail $ "expected String or Object with @value but encountered " ++ typeOf v

instance FromJSON BoolValue where
  parseJSON v = case v of
    Bool bl -> pure $ BoolValue bl
    Object o -> BoolValue <$> (o .: "@value")
    _ -> fail $ "expected String or Object with @value but encountered " ++ typeOf v

typeOf :: Value -> String
typeOf v = case v of
  Object _ -> "Object"
  Array _ -> "Array"
  String _ -> "String"
  Number _ -> "Number"
  Bool _ -> "Boolean"
  Null -> "Null"
