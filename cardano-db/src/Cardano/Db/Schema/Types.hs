{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Db.Schema.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import Data.Time (UTCTime, localTimeToUTC, utc, utcToLocalTime)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as E
import Quiet (Quiet (..))

newtype AddressHash -- Length (28 bytes) enforced by Postgres
  = AddressHash {unAddressHash :: ByteString}
  deriving (Generic)
  deriving (Read, Show) via (Quiet AddressHash)

newtype PaymentAddrHash -- Length (32 bytes) enforced by Postgres
  = PaymentAddrHash {unPaymentAddrHash :: ByteString}
  deriving (Generic)
  deriving (Read, Show) via (Quiet PaymentAddrHash)

-- The pool's Bech32 encoded identifier (the hash of the stake pool operator's vkey).
newtype PoolIdent = PoolIdent {unPoolIdent :: Text}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet PoolIdent)

-- | The raw binary hash of a stake pool's metadata.
newtype PoolMetaHash = PoolMetaHash {unPoolMetaHash :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet PoolMetaHash)

-- | The pool url wrapper so we have some additional safety.
newtype PoolUrl = PoolUrl {unPoolUrl :: Text}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet PoolUrl)

poolUrlDecoder :: HsqlD.Value PoolUrl
poolUrlDecoder = PoolUrl <$> HsqlD.text

textDecoder :: HsqlD.Value Text
textDecoder = HsqlD.custom (\_ bytes -> Right (Text.decodeUtf8With TextError.lenientDecode bytes))

-- Custom decoders/encoders that mimic Persistent's UTCTime sqltype=timestamp behavior
-- Persistent stores UTCTime as timestamp (without timezone) by treating it as LocalTime in UTC
utcTimeAsTimestampDecoder :: D.Value UTCTime
utcTimeAsTimestampDecoder = localTimeToUTC utc <$> D.timestamp

utcTimeAsTimestampEncoder :: E.Value UTCTime
utcTimeAsTimestampEncoder = utcToLocalTime utc >$< E.timestamp
