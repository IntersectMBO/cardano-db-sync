{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.SMASH.Server.Types where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encoding (unsafeToEncoding)
import qualified Data.Aeson.Types ()
import qualified Data.ByteString.Lazy as LBS
import           Data.Swagger (NamedSchema (..), ToParamSchema (..),
                   ToSchema (..))
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)


import           Network.URI (URI, parseURI)
import           Servant (FromHttpApiData (..), MimeUnrender (..), OctetStream)

import           Cardano.Db hiding (TickerName)

import           Cardano.Api            (AsType (..), Hash,
                                         deserialiseFromBech32,
                                         deserialiseFromRawBytesHex,
                                         serialiseToRawBytes)
import           Cardano.Api.Shelley    (StakePoolKey)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BSC
import           Quiet                  (Quiet (..))
-- import           Cardano.SMASH.Db.Error
-- import           Cardano.SMASH.Db.Types
-- import           Cardano.Sync.Types (PoolMetaHash (..), PoolMetaHex (..))

-- | The stake pool identifier. It is the hash of the stake pool operator's
-- vkey.
--
-- It may be rendered as hex or as bech32 using the @pool@ prefix.
--
newtype PoolId = PoolId { getPoolId :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet PoolId)

instance ToJSON PoolId where
    toJSON (PoolId poolId) =
        object
            [ "poolId" .= poolId
            ]

instance FromJSON PoolId where
    parseJSON = withObject "PoolId" $ \o -> do
        poolId <- o .: "poolId"
        case parsePoolId poolId of
            Left err      -> fail $ toS err
            Right poolId' -> return poolId'

instance ToParamSchema PoolId

instance ToSchema PoolId

instance FromHttpApiData PoolId where
    parseUrlPiece poolId = parsePoolId poolId

-- Currently deserializing from safe types, unwrapping and wrapping it up again.
-- The underlying DB representation is HEX.
--
-- pool ids as key hashes and so use the "address hash" size, which is 28 bytes, and hence a hex encoding of that is 2*28 = 56
parsePoolId :: Text -> Either Text PoolId
parsePoolId poolId =
    case pBech32OrHexStakePoolId poolId of
        Nothing -> Left "Unable to parse pool id. Wrong format."
        Just poolId' -> Right . PoolId . decodeUtf8 . B16.encode . serialiseToRawBytes $ poolId'

      where
        -- bech32 pool <<< e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
        -- bech32 <<< pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
        pBech32OrHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pBech32OrHexStakePoolId str = pBech32StakePoolId str <|> pHexStakePoolId str

        -- e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
        pHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pHexStakePoolId =
            deserialiseFromRawBytesHex (AsHash AsStakePoolKey) . BSC.pack . toS

        -- pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
        pBech32StakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pBech32StakePoolId =
          either (const Nothing) Just
            . deserialiseFromBech32 (AsHash AsStakePoolKey)


-- | The hash of a stake pool's metadata.
--
-- It may be rendered as hex.
--
newtype PoolMetadataHash = PoolMetadataHash { getPoolMetadataHash :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetadataHash)

instance ToJSON PoolMetadataHash where
    toJSON (PoolMetadataHash poolHash) =
        object
            [ "poolHash" .= poolHash
            ]

-- The validation of @PoolMetadataHash@ is a bit more involved and would require
-- an analysis with some bounds on the size.
instance FromJSON PoolMetadataHash where
    parseJSON = withObject "PoolMetadataHash" $ \o -> do
        poolHash <- o .: "poolHash"
        return $ PoolMetadataHash poolHash

-- Converting the basic type to a strong one.
-- Presumes the user knows what he is doing, NOT TYPE SAFE!
bytestringToPoolMetaHash :: ByteString -> PoolMetadataHash
bytestringToPoolMetaHash bs = PoolMetadataHash . decodeUtf8 . B16.encode $ bs

instance ToSchema PoolMetadataHash

instance ToParamSchema PoolMetadataHash

-- TODO: Add sanity checks
instance FromHttpApiData PoolMetadataHash where
    parseUrlPiece poolMetadataHash = Right $ PoolMetadataHash poolMetadataHash

-- Result wrapper.
newtype ApiResult err a = ApiResult (Either err a)
    deriving (Generic)

instance (ToSchema a, ToSchema err) => ToSchema (ApiResult err a)

instance (ToJSON err, ToJSON a) => ToJSON (ApiResult err a) where

    toJSON (ApiResult (Left dbFail))  = toJSON dbFail
    toJSON (ApiResult (Right result)) = toJSON result

    toEncoding (ApiResult (Left result))  = toEncoding result
    toEncoding (ApiResult (Right result)) = toEncoding result


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


data PolicyResult = PolicyResult
    { prSmashURL :: !SmashURL
    , prHealthStatus :: !HealthStatus
    , prDelistedPools :: ![PoolId]
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


-- |Fetch error for the specific @PoolId@ and the @PoolMetadataHash@.
data PoolFetchError = PoolFetchError !Time.POSIXTime !PoolId !PoolMetadataHash !Text !Word
  deriving (Eq, Show, Generic)

instance ToJSON PoolFetchError where
    toJSON (PoolFetchError time poolId poolHash errorCause retryCount) =
        object
            [ "time"        .= formatTimeToNormal time
            , "utcTime"     .= (show time :: Text)
            , "poolId"      .= getPoolId poolId
            , "poolHash"    .= getPoolMetadataHash poolHash
            , "cause"       .= errorCause
            , "retryCount"  .= retryCount
            ]

instance ToSchema PoolFetchError

formatTimeToNormal :: Time.POSIXTime -> Text
formatTimeToNormal = toS . formatTime defaultTimeLocale "%d.%m.%Y. %T" . Time.posixSecondsToUTCTime


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
    return $ NamedSchema (Just "SmashURL") $ mempty


newtype UniqueTicker = UniqueTicker { getUniqueTicker :: (TickerName, PoolMetadataHash) }
    deriving (Eq, Show, Generic)

instance ToJSON UniqueTicker where
    toJSON (UniqueTicker (tickerName, poolMetadataHash)) =
        object
            [ "tickerName" .= getTickerName tickerName
            , "poolMetadataHash" .= getPoolMetadataHash poolMetadataHash
            ]

instance FromJSON UniqueTicker where
    parseJSON = withObject "UniqueTicker" $ \o -> do
        tickerName <- o .: "tickerName"
        poolMetadataHash <- o .: "poolMetadataHash"

        return . UniqueTicker $ (tickerName, poolMetadataHash)

instance ToSchema UniqueTicker


-- | The ticker name wrapper so we have some additional safety.
newtype TickerName = TickerName { getTickerName :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet TickerName)

instance ToJSON TickerName where
    toJSON (TickerName name) =
        object
            [ "name" .= name
            ]

instance FromJSON TickerName where
    parseJSON = withObject "TickerName" $ \o -> do
        name <- o .: "name"

        eitherToMonadFail $ validateTickerName name

instance FromHttpApiData TickerName where
    parseUrlPiece tickerName = validateTickerName tickerName

-- |Util.
eitherToMonadFail :: MonadFail m => Either Text a -> m a
eitherToMonadFail (Left err)  = fail $ toS err
eitherToMonadFail (Right val) = return val

-- |The validation for the ticker name we can reuse.
validateTickerName :: Text -> Either Text TickerName
validateTickerName name =  do
    let tickerLen = length name
    if tickerLen >= 3 && tickerLen <= 5
        then Right $ TickerName name
        else Left $
             "\"ticker\" must have at least 3 and at most 5 "
          <> "characters, but it has "
          <> show (length name)
          <> " characters."

instance ToParamSchema TickerName

instance ToSchema TickerName


data PoolIdBlockNumber = PoolIdBlockNumber !PoolId !Word64
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

-- | The stake pool metadata. It is JSON format. This type represents it in
-- its raw original form. The hash of this content is the 'PoolMetadataHash'.
newtype PoolMetadataRaw = PoolMetadataRaw { getPoolMetadata :: Text }
  deriving stock (Eq, Show, Ord, Generic)

instance MimeUnrender OctetStream PoolMetadataRaw where
    mimeUnrender _ = Right . PoolMetadataRaw . Text.decodeUtf8 . LBS.toStrict

-- Here we are usingg the unsafe encoding since we already have the JSON format
-- from the database.
instance ToJSON PoolMetadataRaw where
    toJSON (PoolMetadataRaw metadata) = toJSON metadata
    toEncoding (PoolMetadataRaw metadata) = unsafeToEncoding $ Text.encodeUtf8Builder metadata

instance ToSchema PoolMetadataRaw

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

data DBFail
  = UnknownError !Text
  | DbLookupPoolMetadataHash !PoolId !PoolMetadataHash
  | RecordDoesNotExist
  | DBFail LookupFail
  deriving (Eq, Show)

instance ToSchema DBFail where
  declareNamedSchema _ =
    return $ NamedSchema (Just "DBFail") $ mempty

{-

The example we agreed would be:
```
{
    "code": "ERR_4214",
    "description": "You did something wrong."
}
```

-}
instance ToJSON DBFail where
    toJSON (UnknownError err) =
        object
            [ "code"            .= Aeson.String "UnknownError"
            , "description"     .= Aeson.String err
            ]
    toJSON failure@(DbLookupPoolMetadataHash pid pmh) =
        object
            [ "code"            .= Aeson.String "DbLookupPoolMetadataHash"
            , "description"     .= Aeson.String (renderDBFail failure)
            ]
    toJSON failure@RecordDoesNotExist =
        object
            [ "code"            .= Aeson.String "RecordDoesNotExist"
            , "description"     .= Aeson.String (renderDBFail failure)
            ]
    toJSON (DBFail err) =
        object
            [ "code"            .= Aeson.String "DBFail"
            , "description"     .= Aeson.String (renderLookupFail err)
            ]

renderDBFail :: DBFail -> Text
renderDBFail (UnknownError err) = err
renderDBFail (DbLookupPoolMetadataHash poolId poolMDHash) =
    "The metadata with hash " <> show poolMDHash <> " for pool " <> show poolId <> " is missing from the DB."
renderDBFail RecordDoesNotExist =
    "The requested record does not exist."
