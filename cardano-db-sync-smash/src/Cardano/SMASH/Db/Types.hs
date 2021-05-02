{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingVia #-}

module Cardano.SMASH.Db.Types where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

import           Cardano.Db

import           Cardano.Api (AsType (..), Hash, deserialiseFromBech32, deserialiseFromRawBytesHex,
                   serialiseToRawBytes)
import           Cardano.Api.Shelley (StakePoolKey)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS

instance ToJSON PoolIdentifier where
    toJSON (PoolIdentifier poolId) =
        object
            [ "poolId" .= poolId
            ]

instance FromJSON PoolIdentifier where
    parseJSON = withObject "PoolId" $ \o -> do
        poolId <- o .: "poolId"
        case parsePoolId poolId of
            Left err      -> fail $ toS err
            Right poolId' -> return poolId'

-- Currently deserializing from safe types, unwrapping and wrapping it up again.
-- The underlying DB representation is HEX.
--
-- pool ids as key hashes and so use the "address hash" size, which is 28 bytes, and hence a hex encoding of that is 2*28 = 56
parsePoolId :: Text -> Either Text PoolIdentifier
parsePoolId poolId =
    case pBech32OrHexStakePoolId poolId of
        Nothing -> Left "Unable to parse pool id. Wrong format."
        Just poolId' -> Right . PoolIdentifier . decodeUtf8 . B16.encode . serialiseToRawBytes $ poolId'

      where
        -- bech32 pool <<< e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
        -- bech32 <<< pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
        pBech32OrHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pBech32OrHexStakePoolId str = pBech32StakePoolId str <|> pHexStakePoolId str

        -- e5cb8a89cabad2cb22ea85423bcbbe270f292be3dbe838948456d3ae
        pHexStakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pHexStakePoolId =
            deserialiseFromRawBytesHex (AsHash AsStakePoolKey) . BS.pack . toS

        -- pool1uh9c4zw2htfvkgh2s4prhja7yu8jj2lrm05r39yy2mf6uqqegn6
        pBech32StakePoolId :: Text -> Maybe (Hash StakePoolKey)
        pBech32StakePoolId =
          either (const Nothing) Just
            . deserialiseFromBech32 (AsHash AsStakePoolKey)

poolMetadataHashToByteString :: PoolMetaHash -> ByteString
poolMetadataHashToByteString (PoolMetaHash poolMetadataHash') = encodeUtf8 poolMetadataHash'

instance ToJSON PoolMetaHash where
    toJSON (PoolMetaHash poolHash) =
        object
            [ "poolHash" .= poolHash
            ]

-- The validation of @PoolMetadataHash@ is a bit more involved and would require
-- an analysis with some bounds on the size.
instance FromJSON PoolMetaHash where
    parseJSON = withObject "PoolMetadataHash" $ \o -> do
        poolHash <- o .: "poolHash"
        return $ PoolMetaHash poolHash

-- Converting the basic type to a strong one.
-- Presumes the user knows what he is doing, NOT TYPE SAFE!
bytestringToPoolMetaHash :: ByteString -> PoolMetaHash
bytestringToPoolMetaHash = PoolMetaHash . decodeUtf8 . B16.encode

instance ToJSON TickerName where
    toJSON (TickerName name) =
        object
            [ "name" .= name
            ]

instance FromJSON TickerName where
    parseJSON = withObject "TickerName" $ \o -> do
        name <- o .: "name"

        eitherToMonadFail $ validateTickerName name

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

