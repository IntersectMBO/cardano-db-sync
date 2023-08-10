{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.SMASH.Server.Types (
  ApiResult (..),
  DBFail (..),
  HealthStatus (..),
  PolicyResult (..),
  PoolFetchError (..),
  PoolId (..),
  PoolMetaHash (..),
  PoolMetadataHash (..),
  PoolMetadataRaw (..),
  SmashURL (..),
  TickerName (..),
  TimeStringFormat (..),
  UniqueTicker (..),
  User (..),
  UserValidity (..),
) where

import Cardano.Api (
  AsType (..),
  Hash,
  deserialiseFromBech32,
  deserialiseFromRawBytesHex,
  serialiseToRawBytes,
 )
import Cardano.Api.Shelley (StakePoolKey)
import Cardano.Db (LookupFail (..), PoolMetaHash (..))
import Cardano.Prelude
import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encoding (unsafeToEncoding)
import qualified Data.Aeson.Types ()
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (NamedSchema (..), ToParamSchema (..), ToSchema (..))
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock.POSIX as Time
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.URI (URI, parseURI)
import Quiet (Quiet (..))
import Servant (FromHttpApiData (..), MimeUnrender (..), OctetStream)
import qualified Text.Show as Text

-- | The stake pool identifier. It is the hash of the stake pool operator's
-- vkey.
--
-- It may be rendered as hex or as bech32 using the @pool@ prefix.
newtype PoolId = PoolId {getPoolId :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (Quiet PoolId)

instance ToJSON PoolId where
  toJSON (PoolId poolId) =
    object
      [ "poolId" .= poolId
      ]

instance FromJSON PoolId where
  parseJSON = withObject "PoolId" $ \o -> do
    poolId <- o .: "poolId"
    case parsePoolId poolId of
      Left err -> fail $ toS err
      Right poolId' -> pure poolId'

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
      either (const Nothing) Just
        . deserialiseFromRawBytesHex (AsHash AsStakePoolKey)
        . BS.pack
        . toS

    -- pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
    pBech32StakePoolId :: Text -> Maybe (Hash StakePoolKey)
    pBech32StakePoolId =
      either (const Nothing) Just
        . deserialiseFromBech32 (AsHash AsStakePoolKey)

-- | The hash of a stake pool's metadata.
--
-- It may be rendered as hex.
newtype PoolMetadataHash = PoolMetadataHash {getPoolMetadataHash :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (Quiet PoolMetadataHash)

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
    pure $ PoolMetadataHash poolHash

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
  toJSON (ApiResult (Left dbFail)) = toJSON dbFail
  toJSON (ApiResult (Right result)) = toJSON result

  toEncoding (ApiResult (Left result)) = toEncoding result
  toEncoding (ApiResult (Right result)) = toEncoding result

-- | The data for returning the health check for SMASH.
data HealthStatus = HealthStatus
  { hsStatus :: !Text
  , hsVersion :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HealthStatus where
  toJSON (HealthStatus hsStatus' hsVersion') =
    object
      [ "status" .= hsStatus'
      , "version" .= hsVersion'
      ]

instance FromJSON HealthStatus where
  parseJSON = withObject "healthStatus" $ \o -> do
    status <- o .: "status"
    version <- o .: "version"

    pure
      HealthStatus
        { hsStatus = status
        , hsVersion = version
        }

instance ToSchema HealthStatus

data PolicyResult = PolicyResult
  { prSmashURL :: !SmashURL
  , prHealthStatus :: !HealthStatus
  , prDelistedPools :: ![PoolId]
  , prUniqueTickers :: ![UniqueTicker]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PolicyResult where
  toJSON (PolicyResult smashURL healthStatus delistedPools uniqueTickers) =
    object
      [ "smashURL" .= toJSON smashURL
      , "healthStatus" .= toJSON healthStatus
      , "delistedPools" .= toJSON delistedPools
      , "uniqueTickers" .= toJSON uniqueTickers
      ]

instance ToSchema PolicyResult

-- | Fetch error for the specific @PoolId@ and the @PoolMetadataHash@.
data PoolFetchError = PoolFetchError !Time.POSIXTime !PoolId !PoolMetadataHash !Text !Word
  deriving (Eq, Show, Generic)

instance ToJSON PoolFetchError where
  toJSON (PoolFetchError time poolId poolHash errorCause retryCount) =
    object
      [ "time" .= formatTimeToNormal time
      , "utcTime" .= (show time :: Text)
      , "poolId" .= getPoolId poolId
      , "poolHash" .= getPoolMetadataHash poolHash
      , "cause" .= errorCause
      , "retryCount" .= retryCount
      ]

instance ToSchema PoolFetchError

formatTimeToNormal :: Time.POSIXTime -> Text
formatTimeToNormal = toS . formatTime defaultTimeLocale "%d.%m.%Y. %T" . Time.posixSecondsToUTCTime

-- | The Smash @URI@ containing remote filtering data.
newtype SmashURL = SmashURL {getSmashURL :: URI}
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
      Just uri' -> pure (SmashURL uri')

instance ToSchema SmashURL where
  declareNamedSchema _ =
    pure (NamedSchema (Just "SmashURL") mempty)

newtype UniqueTicker = UniqueTicker {getUniqueTicker :: (TickerName, PoolId)}
  deriving (Eq, Show, Generic)

instance ToJSON UniqueTicker where
  toJSON (UniqueTicker (tickerName, poolMetadataHash)) =
    object
      [ "tickerName" .= getTickerName tickerName
      , "poolId" .= getPoolId poolMetadataHash
      ]

instance FromJSON UniqueTicker where
  parseJSON = withObject "UniqueTicker" $ \o -> do
    tickerName <- o .: "tickerName"
    poolMetadataHash <- o .: "poolId"

    pure . UniqueTicker $ (tickerName, poolMetadataHash)

instance ToSchema UniqueTicker

-- | The ticker name wrapper so we have some additional safety.
newtype TickerName = TickerName {getTickerName :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (Quiet TickerName)

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

eitherToMonadFail :: MonadFail m => Either Text a -> m a
eitherToMonadFail (Left err) = fail $ toS err
eitherToMonadFail (Right val) = pure val

-- | The validation for the ticker name we can reuse.
validateTickerName :: Text -> Either Text TickerName
validateTickerName name = do
  let tickerLen = length name
  if tickerLen >= 3 && tickerLen <= 5
    then Right $ TickerName name
    else
      Left $
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
      [ "poolId" .= poolId
      , "blockNumber" .= blockNumber
      ]

instance FromJSON PoolIdBlockNumber where
  parseJSON = withObject "poolIdBlockNumber" $ \o -> do
    poolId <- o .: "poolId"
    blockNumber <- o .: "blockNumber"

    pure (PoolIdBlockNumber poolId blockNumber)

instance ToSchema PoolIdBlockNumber

-- | The stake pool metadata in JSON format. This type represents it in
-- its raw original form. The hash of this content is the 'PoolMetadataHash'.
newtype PoolMetadataRaw = PoolMetadataRaw {getPoolMetadata :: ByteString}
  deriving stock (Eq, Show, Ord, Generic)

instance MimeUnrender OctetStream PoolMetadataRaw where
  mimeUnrender _ = Right . PoolMetadataRaw . LBS.toStrict

-- Here we are usingg the unsafe encoding since we already have the JSON format
-- from the database.
instance ToJSON PoolMetadataRaw where
  toJSON (PoolMetadataRaw metadata) = Aeson.String $ decodeUtf8 metadata
  toEncoding (PoolMetadataRaw metadata) = unsafeToEncoding $ BSB.byteString metadata

instance ToSchema PoolMetadataRaw where
  declareNamedSchema _ = pure (NamedSchema (Just "RawPoolMetadata") mempty)

-- | Specific time string format.
newtype TimeStringFormat = TimeStringFormat {unTimeStringFormat :: UTCTime}
  deriving (Eq, Show, Generic)

instance FromHttpApiData TimeStringFormat where
  parseQueryParam :: Text -> Either Text TimeStringFormat
  parseQueryParam queryParam =
    let timeFormat = "%d.%m.%Y"
        parsedTime = parseTimeM False defaultTimeLocale timeFormat $ toS queryParam
     in TimeStringFormat <$> parsedTime

-- Required for the above, error with newer GHC versions
instance MonadFail (Either Text) where
  fail = Left . toS

instance ToParamSchema TimeStringFormat

-- | A user we'll grab from the database when we authenticate someone
newtype User = User {userName :: Text}
  deriving (Eq, Show)

-- | This we can leak.
data UserValidity
  = UserValid !User
  | UserInvalid
  deriving (Eq, Show)

data DBFail
  = UnknownError !Text
  | DbInsertError !Text
  | DbLookupPoolMetadataHash !PoolId !PoolMetadataHash
  | TickerAlreadyReserved !TickerName
  | RecordDoesNotExist
  | DBFail LookupFail
  | PoolDataLayerError !Text
  | ConfigError !Text
  deriving (Eq)

instance ToSchema DBFail where
  declareNamedSchema _ =
    pure (NamedSchema (Just "DBFail") mempty)

instance Exception DBFail

instance Show DBFail where
  show =
    \case
      UnknownError err -> "Unknown error. Context: " <> Text.show err
      DbInsertError err ->
        "The database got an error while trying to insert a record. Error: " <> Text.show err
      DbLookupPoolMetadataHash poolId poolMDHash ->
        "The metadata with hash " <> Text.show poolMDHash <> " for pool " <> Text.show poolId <> " is missing from the DB."
      TickerAlreadyReserved ticker -> "Ticker name " <> Text.show (getTickerName ticker) <> " is already reserved"
      RecordDoesNotExist -> "The requested record does not exist."
      DBFail lookupFail -> Text.show lookupFail
      PoolDataLayerError err -> Text.show err
      ConfigError err -> "Config Error: " <> Text.show err

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
      [ "code" .= Aeson.String "UnknownError"
      , "description" .= Aeson.String err
      ]
  toJSON (DbInsertError err) =
    object
      [ "code" .= Aeson.String "DbInsertError"
      , "description" .= Aeson.String err
      ]
  toJSON failure@DbLookupPoolMetadataHash {} =
    object
      [ "code" .= Aeson.String "DbLookupPoolMetadataHash"
      , "description" .= Aeson.String (show failure)
      ]
  toJSON failure@TickerAlreadyReserved {} =
    object
      [ "code" .= Aeson.String "TickerAlreadyReserved"
      , "description" .= Aeson.String (show failure)
      ]
  toJSON failure@RecordDoesNotExist =
    object
      [ "code" .= Aeson.String "RecordDoesNotExist"
      , "description" .= Aeson.String (Cardano.Prelude.show failure)
      ]
  toJSON (DBFail err) =
    object
      [ "code" .= Aeson.String "DBFail"
      , "description" .= Aeson.String (Cardano.Prelude.show err)
      ]
  toJSON (PoolDataLayerError err) =
    object
      [ "code" .= Aeson.String "PoolDataLayerError"
      , "description" .= Aeson.String (Cardano.Prelude.show err)
      ]
  toJSON (ConfigError err) =
    object
      [ "code" .= Aeson.String "ConfigError"
      , "description" .= Aeson.String (Cardano.Prelude.show err)
      ]
