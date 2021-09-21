{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Byron.Util
  ( boundaryEpochNumber
  , configSlotDuration
  , mkSlotLeader
  , slotLeaderHash
  , unAbstractHash
  , unAddressHash
  , unCryptoHash
  ) where

import           Cardano.Prelude hiding (catch)

import           Cardano.Binary (Raw)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Wallet as Crypto

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron

import qualified Cardano.Db as DB

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text



boundaryEpochNumber :: Byron.ABoundaryBlock ByteString -> Word64
boundaryEpochNumber = Byron.boundaryEpoch . Byron.boundaryHeader

configSlotDuration :: Byron.Config -> Word64
configSlotDuration =
  fromIntegral . Byron.ppSlotDuration . Byron.gdProtocolParameters . Byron.configGenesisData

mkSlotLeader :: Byron.ABlock ByteString -> DB.SlotLeader
mkSlotLeader blk =
  let slHash = slotLeaderHash blk
      slName = "ByronGenesis-" <> Text.decodeUtf8 (Base16.encode $ BS.take 8 slHash)
  -- On Byrom poolHashId will always be Nothing.
  in DB.SlotLeader slHash Nothing slName

slotLeaderHash :: Byron.ABlock ByteString -> ByteString
slotLeaderHash =
  BS.take 28
    . Crypto.abstractHashToBytes . Crypto.hashRaw .LBS.fromStrict . Crypto.xpubPublicKey
    . Crypto.unVerificationKey . Byron.headerGenesisKey . Byron.blockHeader

unAbstractHash :: Crypto.Hash Raw -> ByteString
unAbstractHash = Crypto.abstractHashToBytes

unAddressHash :: Byron.AddressHash Byron.Address' -> ByteString
unAddressHash = Crypto.abstractHashToBytes

unCryptoHash :: Crypto.Hash Raw -> ByteString
unCryptoHash = Crypto.abstractHashToBytes
