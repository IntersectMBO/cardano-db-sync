{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Db.Schema.Types
  ( Address (..)
  , AddressHash (..)
  , PaymentAddrHash (..)
  , PoolIdentifier (..)
  , PoolMetaHash (..)
  , PoolMetadataRaw (..)
  , PoolUrl (..)
  , TickerName (..)
  ) where

import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)

import           Database.Persist.Class (PersistField)

import           GHC.Generics (Generic)

import           Quiet (Quiet (..))


newtype Address -- Length of 28/56/94 bytes enforced by Postgres.
  = Address { unAddress :: ByteString }
  deriving (Generic, PersistField)
  deriving (Read, Show) via (Quiet Address)

newtype AddressHash -- Length (28 bytes) enforced by Postgres
  = AddressHash { unAddressHash :: ByteString }
  deriving (Generic, PersistField)
  deriving (Read, Show) via (Quiet AddressHash)

newtype PaymentAddrHash -- Length (32 bytes) enforced by Postgres
  = PaymentAddrHash { unPaymentAddrHash :: ByteString }
  deriving (Generic, PersistField)
  deriving (Read, Show) via (Quiet PaymentAddrHash)


-- | The stake pool identifier. It is the hash of the stake pool operator's
-- vkey.
--
-- It may be rendered as hex or as bech32 using the @pool@ prefix.
--
newtype PoolIdentifier = PoolIdentifier { getPoolIdentifier :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet PoolIdentifier)
  deriving newtype PersistField

-- | The hash of a stake pool's metadata.
--
-- It may be rendered as hex.
--
newtype PoolMetaHash = PoolMetaHash { getPoolMetaHash :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHash)
  deriving newtype PersistField

-- | The stake pool metadata. It is JSON format. This type represents it in
-- its raw original form. The hash of this content is the 'PoolMetadataHash'.
newtype PoolMetadataRaw = PoolMetadataRaw { getPoolMetadata :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField

-- | The pool url wrapper so we have some additional safety.
newtype PoolUrl = PoolUrl { getPoolUrl :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet PoolUrl)
  deriving newtype PersistField

-- | The ticker name wrapper so we have some additional safety.
newtype TickerName = TickerName { getTickerName :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving Show via (Quiet TickerName)
  deriving newtype PersistField

