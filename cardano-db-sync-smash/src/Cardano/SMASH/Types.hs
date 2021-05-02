{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.SMASH.Types
    ( ApplicationUser (..)
    , ApplicationUsers (..)
    , stubbedApplicationUsers
    , User
    , UserValidity (..)
    , checkIfUserValid
    -- * Pool info
    , PoolIdentifier (..)
    , PoolIdBlockNumber (..)
    , PoolUrl (..)
    , PoolMetaHash (..)
    , bytestringToPoolMetaHash
    , PoolMetadataRaw (..)
    , TickerName (..)
    , UniqueTicker (..)
    , PolicyResult (..)
    -- * Wrapper
    , PoolName (..)
    , PoolDescription (..)
    , PoolTicker (..)
    , PoolHomepage (..)
    , PoolOfflineMetadata (..)
    , createPoolOfflineMetadata
    , examplePoolOfflineMetadata
    -- * Configuration
    , HealthStatus (..)
    , Configuration (..)
    , defaultConfiguration
    -- * API
    , ApiResult (..)
    -- * HTTP
    , SmashURL (..)
    , FetchError (..)
    , PoolFetchError (..)
    , TimeStringFormat (..)
    -- * Util
    , DBConversion (..)
    , formatTimeToNormal
    ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encoding (unsafeToEncoding)
import qualified Data.Aeson.Types as Aeson
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

import           Data.Swagger (NamedSchema (..), ToParamSchema (..), ToSchema (..))
import           Data.Text.Encoding (encodeUtf8Builder)

import           Network.URI (URI, parseURI)
import           Servant (FromHttpApiData (..), MimeUnrender (..), OctetStream)

import           Cardano.Db
import           Cardano.SMASH.Db.Error
import           Cardano.SMASH.Db.Types

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as E

-- | The Smash @URI@ containing remote filtering data.
newtype SmashURL = SmashURL { getSmashURL :: URI }
    deriving (Eq, Show, Generic)

instance ToJSON SmashURL where
    toJSON (SmashURL uri) =
        object
            [ "smashURL" .= (show uri :: Text)
            ]

instance FromJSON SmashURL where
    parseJSON = withObject "SmashURL" $ \o -> do
        uri <- o .: "smashURL"

        let parsedURI = parseURI uri

        case parsedURI of
            Nothing -> fail "Not a valid URI for SMASH server."
            Just uri' -> return $ SmashURL uri'

instance ToSchema SmashURL where
  declareNamedSchema _ =
    return $ NamedSchema (Just "SmashURL") mempty

-- | The basic @Configuration@.
newtype Configuration = Configuration
    { cPortNumber :: Int
    } deriving (Eq, Show)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration 3100

-- | A list of users with very original passwords.
stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "ksaric" "cirask"]

examplePoolOfflineMetadata :: PoolOfflineMetadata
examplePoolOfflineMetadata =
    PoolOfflineMetadata
        (PoolName "TestPool")
        (PoolDescription "This is a pool for testing")
        (PoolTicker "testp")
        (PoolHomepage "https://iohk.io")

data PolicyResult = PolicyResult
    { prSmashURL :: !SmashURL
    , prHealthStatus :: !HealthStatus
    , prDelistedPools :: ![PoolIdentifier]
    , prUniqueTickers :: ![UniqueTicker]
    } deriving (Eq, Show, Generic)

instance ToJSON PolicyResult where
    toJSON (PolicyResult smashURL healthStatus delistedPools uniqueTickers) =
        object
            [ "smashURL" .= toJSON smashURL
            , "healthStatus" .= toJSON healthStatus
            , "delistedPools" .= toJSON delistedPools
            , "uniqueTickers" .= toJSON uniqueTickers
            ]

instance ToSchema PolicyResult

newtype UniqueTicker = UniqueTicker { getUniqueTicker :: (TickerName, PoolMetaHash) }
    deriving (Eq, Show, Generic)

instance ToJSON UniqueTicker where
    toJSON (UniqueTicker (tickerName, poolMetadataHash')) =
        object
            [ "tickerName" .= getTickerName tickerName
            , "poolMetadataHash" .= getPoolMetaHash poolMetadataHash'
            ]

instance FromJSON UniqueTicker where
    parseJSON = withObject "UniqueTicker" $ \o -> do
        tickerName <- o .: "tickerName"
        poolMetadataHash' <- o .: "poolMetadataHash"

        return . UniqueTicker $ (tickerName, poolMetadataHash')

instance ToSchema UniqueTicker

instance ToParamSchema TickerName

instance ToSchema TickerName

instance ToParamSchema PoolIdentifier

instance ToSchema PoolIdentifier

instance ToParamSchema PoolMetaHash

-- A data type we use to store user credentials.
data ApplicationUser = ApplicationUser
    { username :: !Text
    , password :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON ApplicationUser
instance FromJSON ApplicationUser

-- A list of users we use.
newtype ApplicationUsers = ApplicationUsers [ApplicationUser]
    deriving (Eq, Show, Generic)

instance ToJSON ApplicationUsers
instance FromJSON ApplicationUsers

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
  deriving (Eq, Show)

-- | This we can leak.
data UserValidity
    = UserValid !User
    | UserInvalid
    deriving (Eq, Show)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
checkIfUserValid :: ApplicationUsers -> ApplicationUser -> UserValidity
checkIfUserValid (ApplicationUsers applicationUsers) applicationUser@(ApplicationUser usernameText _) =
    if applicationUser `elem` applicationUsers
        then UserValid (User usernameText)
        else UserInvalid

instance FromHttpApiData TickerName where
    parseUrlPiece tickerName = validateTickerName tickerName

-- Currently deserializing from safe types, unwrapping and wrapping it up again.
-- The underlying DB representation is HEX.
instance FromHttpApiData PoolIdentifier where
    parseUrlPiece poolId = parsePoolId poolId

instance ToSchema PoolMetaHash

-- TODO(KS): Temporarily, validation!?
instance FromHttpApiData PoolMetaHash where
    parseUrlPiece poolMetadataHash' = Right $ PoolMetaHash poolMetadataHash'
    --TODO: parse hex or bech32

newtype PoolName = PoolName
    { getPoolName :: Text
    } deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolName

newtype PoolDescription = PoolDescription
    { getPoolDescription :: Text
    } deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolDescription

newtype PoolTicker = PoolTicker
    { getPoolTicker :: Text
    } deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolTicker

newtype PoolHomepage = PoolHomepage
    { getPoolHomepage :: Text
    } deriving (Eq, Show, Ord, Generic)

instance ToSchema PoolHomepage

-- | The bit of the pool data off the chain.
data PoolOfflineMetadata = PoolOfflineMetadata
    { pomName        :: !PoolName
    , pomDescription :: !PoolDescription
    , pomTicker      :: !PoolTicker
    , pomHomepage    :: !PoolHomepage
    } deriving (Eq, Show, Ord, Generic)

-- | Smart constructor, just adding one more layer of indirection.
createPoolOfflineMetadata
    :: PoolName
    -> PoolDescription
    -> PoolTicker
    -> PoolHomepage
    -> PoolOfflineMetadata
createPoolOfflineMetadata = PoolOfflineMetadata

-- Required instances
instance FromJSON PoolOfflineMetadata where
    parseJSON = withObject "poolOfflineMetadata" $ \o -> do
        name'           <- parseName o
        description'    <- parseDescription o
        ticker'         <- parseTicker o
        homepage'       <- o .: "homepage"

        return $ PoolOfflineMetadata
            { pomName           = PoolName name'
            , pomDescription    = PoolDescription description'
            , pomTicker         = PoolTicker ticker'
            , pomHomepage       = PoolHomepage homepage'
            }
      where

        -- Copied from https://github.com/input-output-hk/cardano-node/pull/1299

        -- | Parse and validate the stake pool metadata name from a JSON object.
        --
        -- If the name consists of more than 50 characters, the parser will fail.
        parseName :: Aeson.Object -> Aeson.Parser Text
        parseName obj = do
          name <- obj .: "name"
          if length name <= 50
            then pure name
            else fail $
                 "\"name\" must have at most 50 characters, but it has "
              <> show (length name)
              <> " characters."

        -- | Parse and validate the stake pool metadata description from a JSON
        -- object.
        --
        -- If the description consists of more than 255 characters, the parser will
        -- fail.
        parseDescription :: Aeson.Object -> Aeson.Parser Text
        parseDescription obj = do
          description <- obj .: "description"
          if length description <= 255
            then pure description
            else fail $
                 "\"description\" must have at most 255 characters, but it has "
              <> show (length description)
              <> " characters."

        -- | Parse and validate the stake pool ticker description from a JSON object.
        --
        -- If the ticker consists of less than 3 or more than 5 characters, the parser
        -- will fail.
        parseTicker :: Aeson.Object -> Aeson.Parser Text
        parseTicker obj = do
          ticker <- obj .: "ticker"
          let tickerLen = length ticker
          if tickerLen >= 3 && tickerLen <= 5
            then pure ticker
            else fail $
                 "\"ticker\" must have at least 3 and at most 5 "
              <> "characters, but it has "
              <> show (length ticker)
              <> " characters."

-- |We presume the validation is not required the other way around?
instance ToJSON PoolOfflineMetadata where
    toJSON (PoolOfflineMetadata name' description' ticker' homepage') =
        object
            [ "name"            .= getPoolName name'
            , "description"     .= getPoolDescription description'
            , "ticker"          .= getPoolTicker ticker'
            , "homepage"        .= getPoolHomepage homepage'
            ]

instance ToSchema PoolOfflineMetadata

instance MimeUnrender OctetStream PoolMetadataRaw where
    mimeUnrender _ = Right . PoolMetadataRaw . E.decodeUtf8 . BL.toStrict

-- Here we are usingg the unsafe encoding since we already have the JSON format
-- from the database.
instance ToJSON PoolMetadataRaw where
    toJSON (PoolMetadataRaw metadata) = toJSON metadata
    toEncoding (PoolMetadataRaw metadata) = unsafeToEncoding $ encodeUtf8Builder metadata

instance ToSchema PoolMetadataRaw

instance ToSchema DBFail where
  declareNamedSchema _ =
    return $ NamedSchema (Just "DBFail") mempty

-- Result wrapper.
newtype ApiResult err a = ApiResult (Either err a)
    deriving (Generic)

instance (ToSchema a, ToSchema err) => ToSchema (ApiResult err a)

instance (ToJSON err, ToJSON a) => ToJSON (ApiResult err a) where

    toJSON (ApiResult (Left dbFail))  = toJSON dbFail
    toJSON (ApiResult (Right result)) = toJSON result

    toEncoding (ApiResult (Left result))  = toEncoding result
    toEncoding (ApiResult (Right result)) = toEncoding result

-- |Fetch error for the HTTP client fetching the pool.
data FetchError
  = FEHashMismatch !PoolIdentifier !Text !Text !Text
  | FEDataTooLong !PoolIdentifier !Text
  | FEUrlParseFail !PoolIdentifier !Text !Text
  | FEJsonDecodeFail !PoolIdentifier !Text !Text
  | FEHttpException !PoolIdentifier !Text !Text
  | FEHttpResponse !PoolIdentifier !Text !Int
  | FEIOException !Text
  | FETimeout !PoolIdentifier !Text !Text
  | FEConnectionFailure !PoolIdentifier !Text
  deriving (Eq, Generic)

data PoolIdBlockNumber = PoolIdBlockNumber !PoolIdentifier !Word64
    deriving (Eq, Show, Generic)

instance ToJSON PoolIdBlockNumber where
    toJSON (PoolIdBlockNumber poolId blockNumber) =
        object
            [ "poolId"      .= poolId
            , "blockNumber" .= blockNumber
            ]

instance FromJSON PoolIdBlockNumber where
    parseJSON = withObject "poolIdBlockNumber" $ \o -> do
        poolId          <- o .: "poolId"
        blockNumber     <- o .: "blockNumber"

        return $ PoolIdBlockNumber poolId blockNumber

instance ToSchema PoolIdBlockNumber

-- |Fetch error for the specific @PoolIdentifier@ and the @PoolMetaHash@.
data PoolFetchError = PoolFetchError !Time.POSIXTime !PoolIdentifier !PoolMetaHash !Text !Word
  deriving (Eq, Show, Generic)

instance ToJSON PoolFetchError where
    toJSON (PoolFetchError time poolId poolHash errorCause retryCount) =
        object
            [ "time"        .= formatTimeToNormal time
            , "utcTime"     .= (show time :: Text)
            , "poolId"      .= getPoolIdentifier poolId
            , "poolHash"    .= getPoolMetaHash poolHash
            , "cause"       .= errorCause
            , "retryCount"  .= retryCount
            ]

instance ToSchema PoolFetchError

formatTimeToNormal :: Time.POSIXTime -> Text
formatTimeToNormal = toS . formatTime defaultTimeLocale "%d.%m.%Y. %T" . Time.posixSecondsToUTCTime

-- |Specific time string format.
newtype TimeStringFormat = TimeStringFormat { unTimeStringFormat :: UTCTime }
    deriving (Eq, Show, Generic)

instance FromHttpApiData TimeStringFormat where
    --parseQueryParam :: Text -> Either Text a
    parseQueryParam queryParam =
        let timeFormat = "%d.%m.%Y"

            --parsedTime :: UTCTime <- parseTimeM False defaultTimeLocale "%d.%m.%Y %T" "04.03.2010 16:05:21"
            parsedTime = parseTimeM False defaultTimeLocale timeFormat $ toS queryParam
        in  TimeStringFormat <$> parsedTime

-- Required for the above, error with newer GHC versions
instance MonadFail (Either Text) where
    fail = Left . toS

instance ToParamSchema TimeStringFormat

-- |The data for returning the health check for SMASH.
data HealthStatus = HealthStatus
    { hsStatus  :: !Text
    , hsVersion :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON HealthStatus where
    toJSON (HealthStatus hsStatus' hsVersion') =
        object
            [ "status"      .= hsStatus'
            , "version"     .= hsVersion'
            ]

instance FromJSON HealthStatus where
    parseJSON = withObject "healthStatus" $ \o -> do
        status          <- o .: "status"
        version         <- o .: "version"

        return $ HealthStatus
            { hsStatus  = status
            , hsVersion = version
            }

instance ToSchema HealthStatus

-- We need a "conversion" layer between custom DB types and the rest of the
-- codebase se we can have a clean separation and replace them at any point.
-- The natural place to have this conversion is in the types.
-- The choice is to use the typeclass here since the operation is general and
-- will be used multiple times (more than 3!).
class DBConversion dbType regularType where
    convertFromDB   :: dbType -> regularType
    convertToDB     :: regularType -> dbType

