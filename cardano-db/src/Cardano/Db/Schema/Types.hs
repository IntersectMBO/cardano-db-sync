{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Db.Schema.Types
  ( Address (..)
  , AddressHash (..)
  , PaymentAddrHash (..)
  -- * SMASH types
  , DbPoolId (..)
  , DbPoolMetadataHash (..)
  , DbPoolMetadataRaw (..)
  , DbPoolUrl (..)
  , DbTickerName (..)
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
newtype DbPoolId = DbPoolId { getPoolId :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField

-- | The hash of a stake pool's metadata.
--
-- It may be rendered as hex.
newtype DbPoolMetadataHash = DbPoolMetadataHash { getPoolMetadataHash :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField

-- | The stake pool metadata. This type represents it in
-- its raw original form. The hash of this content is the 'PoolMetadataHash'.
newtype DbPoolMetadataRaw = DbPoolMetadataRaw { getPoolMetadata :: ByteString }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField

-- | The pool url wrapper so we have some additional safety.
newtype DbPoolUrl = DbPoolUrl { getPoolUrl :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField

-- | The ticker name wrapper so we have some additional safety.
newtype DbTickerName = DbTickerName { getTickerName :: Text }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype PersistField


