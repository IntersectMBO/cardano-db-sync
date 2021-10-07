{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Db.Schema.Types
  ( Address (..)
  , AddressHash (..)
  , PaymentAddrHash (..)
  , PoolMetaHash (..)
  , PoolUrl (..)
  , TickerName (..)
  ) where

import           Data.ByteString.Char8 (ByteString)

import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Quiet (Quiet (..))


newtype Address -- Length of 28/56/94 bytes enforced by Postgres.
  = Address { unAddress :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet Address)

newtype AddressHash -- Length (28 bytes) enforced by Postgres
  = AddressHash { unAddressHash :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet AddressHash)

newtype PaymentAddrHash -- Length (32 bytes) enforced by Postgres
  = PaymentAddrHash { unPaymentAddrHash :: ByteString }
  deriving (Generic)
  deriving (Read, Show) via (Quiet PaymentAddrHash)


-- The pool's Bech32 encoded identifier (the hash of the stake pool operator's vkey).
newtype PoolIdent
  = PoolIdent { unPoolIdent :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolIdent)

-- | The raw binary hash of a stake pool's metadata.
newtype PoolMetaHash
  = PoolMetaHash { unPoolMetaHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHash)

-- | The pool url wrapper so we have some additional safety.
newtype PoolUrl
  = PoolUrl { unPoolUrl :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolUrl)

-- | The ticker name wrapper so we have some additional safety.
newtype TickerName
  = TickerName { unTickerName :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet TickerName)

