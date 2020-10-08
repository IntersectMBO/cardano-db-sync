{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Db.Schema.Types
  ( Address (..)
  , AddressHash (..)
  , PaymentAddrHash (..)
  ) where

import Data.ByteString.Char8 (ByteString)

import Database.Persist.Class (PersistField)

import GHC.Generics (Generic)

import Quiet (Quiet (..))


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
