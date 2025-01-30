{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.Pool where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Types (
  PoolUrl (..),
  unPoolUrl
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E
import Cardano.Db.Types (
  DbWord64 (..),
  DbLovelace (..),
  dbLovelaceDecoder,
  dbLovelaceEncoder
  )
import Data.Functor.Contravariant ((>$<))

-----------------------------------------------------------------------------------------------------------------------------------
-- POOLS
-- These tables manage stake pool-related data, including pool registration, updates, and retirements.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_hash
Description: A table containing information about pool hashes.
-}
data PoolHash = PoolHash
  { poolHashId :: !PoolHashId
  , poolHashHashRaw :: !ByteString  -- unique hashRaw sqltype=hash28type
  , poolHashView :: !Text
  } deriving (Eq, Show, Generic)

poolHashDecoder :: D.Row PoolHash
poolHashDecoder =
  PoolHash
    <$> idDecoder PoolHashId -- poolHashId
    <*> D.column (D.nonNullable D.bytea) -- poolHashHashRaw
    <*> D.column (D.nonNullable D.text) -- poolHashView

poolHashEncoder :: E.Params PoolHash
poolHashEncoder =
  mconcat
    [ (getPoolHashId . poolHashId) >$< E.param (E.nonNullable E.int8) -- poolHashId
    , poolHashHashRaw >$< E.param (E.nonNullable E.bytea) --poolHashHashRaw
    , poolHashView >$< E.param (E.nonNullable E.text) -- poolHashView
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_meta_data
Description: A table containing information about pool metadata.
-}
data PoolStat = PoolStat
  { poolStatId :: !PoolStatId
  , poolStatPoolHashId :: !PoolHashId   -- noreference
  , poolStatEpochNo :: !Word64          -- sqltype=word31type
  , poolStatNumberOfBlocks :: !DbWord64 -- sqltype=word64type
  , poolStatNumberOfDelegators :: !DbWord64 -- sqltype=word64type
  , poolStatStake :: !DbWord64          -- sqltype=word64type
  , poolStatVotingPower :: !(Maybe DbWord64) -- sqltype=word64type
  } deriving (Eq, Show, Generic)

poolStatDecoder :: D.Row PoolStat
poolStatDecoder =
  PoolStat
    <$> idDecoder PoolStatId -- poolStatId
    <*> idDecoder PoolHashId -- poolStatPoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolStatEpochNo
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatNumberOfBlocks
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatNumberOfDelegators
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatStake
    <*> D.column (D.nullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatVotingPower

poolStatEncoder :: E.Params PoolStat
poolStatEncoder =
  mconcat
    [ poolStatId >$< idEncoder getPoolStatId
    , poolStatPoolHashId >$< idEncoder getPoolHashId
    , poolStatEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolStatNumberOfBlocks >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatNumberOfDelegators >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatStake >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatVotingPower >$< E.param (E.nullable $ fromIntegral . unDbWord64 >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_update
Description: A table containing information about pool updates.
-}
data PoolUpdate = PoolUpdate
  { poolUpdateId :: !PoolUpdateId
  , poolUpdateHashId :: !PoolHashId        -- noreference
  , poolUpdateCertIndex :: !Word16
  , poolUpdateVrfKeyHash :: !ByteString    -- sqltype=hash32type
  , poolUpdatePledge :: !DbLovelace        -- sqltype=lovelace
  , poolUpdateRewardAddrId :: !StakeAddressId -- noreference
  , poolUpdateActiveEpochNo :: !Word64
  , poolUpdateMetaId :: !(Maybe PoolMetadataRefId) -- noreference
  , poolUpdateMargin :: !Double            -- sqltype=percentage????
  , poolUpdateFixedCost :: !DbLovelace     -- sqltype=lovelace
  , poolUpdateDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , poolUpdateRegisteredTxId :: !TxId      -- noreference -- Slot number in which the pool was registered.
  } deriving (Eq, Show, Generic)

poolUpdateDecoder :: D.Row PoolUpdate
poolUpdateDecoder =
  PoolUpdate
    <$> idDecoder PoolUpdateId -- poolUpdateId
    <*> idDecoder PoolHashId -- poolUpdateHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- poolUpdateCertIndex (Word16)
    <*> D.column (D.nonNullable D.bytea) -- poolUpdateVrfKeyHash
    <*> dbLovelaceDecoder -- poolUpdatePledge
    <*> idDecoder StakeAddressId -- poolUpdateRewardAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolUpdateActiveEpochNo
    <*> maybeIdDecoder PoolMetadataRefId -- poolUpdateMetaId
    <*> D.column (D.nonNullable D.float8) -- poolUpdateMargin
    <*> dbLovelaceDecoder -- poolUpdateFixedCost
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- poolUpdateDeposit
    <*> idDecoder TxId -- poolUpdateRegisteredTxId

poolUpdateEncoder :: E.Params PoolUpdate
poolUpdateEncoder =
  mconcat
    [ poolUpdateId >$< idEncoder getPoolUpdateId
    , poolUpdateHashId >$< idEncoder getPoolHashId
    , poolUpdateCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolUpdateVrfKeyHash >$< E.param (E.nonNullable E.bytea)
    , poolUpdatePledge >$< dbLovelaceEncoder
    , poolUpdateRewardAddrId >$< idEncoder getStakeAddressId
    , poolUpdateActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolUpdateMetaId >$< maybeIdEncoder getPoolMetadataRefId
    , poolUpdateMargin >$< E.param (E.nonNullable E.float8)
    , poolUpdateFixedCost >$< dbLovelaceEncoder
    , poolUpdateDeposit >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , poolUpdateRegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_metadata_ref
Description: A table containing references to pool metadata.
-}
data PoolMetadataRef = PoolMetadataRef
  { poolMetadataRefId :: !PoolMetadataRefId
  , poolMetadataRefPoolId :: !PoolHashId -- noreference
  , poolMetadataRefUrl :: !PoolUrl      -- sqltype=varchar
  , poolMetadataRefHash :: !ByteString  -- sqltype=hash32type
  , poolMetadataRefRegisteredTxId :: !TxId -- noreference
  } deriving (Eq, Show, Generic)

poolMetadataRefDecoder :: D.Row PoolMetadataRef
poolMetadataRefDecoder =
  PoolMetadataRef
    <$> idDecoder PoolMetadataRefId -- poolMetadataRefId
    <*> idDecoder PoolHashId -- poolMetadataRefPoolId
    <*> D.column (D.nonNullable (PoolUrl <$> D.text))-- poolMetadataRefUrl
    <*> D.column (D.nonNullable D.bytea) -- poolMetadataRefHash
    <*> idDecoder TxId -- poolMetadataRefRegisteredTxId

poolMetadataRefEncoder :: E.Params PoolMetadataRef
poolMetadataRefEncoder =
  mconcat
    [ poolMetadataRefId >$< idEncoder getPoolMetadataRefId
    , poolMetadataRefPoolId >$< idEncoder getPoolHashId
    , poolMetadataRefUrl >$< E.param (E.nonNullable (unPoolUrl >$< E.text))
    , poolMetadataRefHash >$< E.param (E.nonNullable E.bytea)
    , poolMetadataRefRegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_owner
Description: A table containing information about pool owners.
-}
data PoolOwner = PoolOwner
  { poolOwnerId :: !PoolOwnerId
  , poolOwnerAddrId :: !StakeAddressId     -- noreference
  , poolOwnerPoolUpdateId :: !PoolUpdateId -- noreference
  } deriving (Eq, Show, Generic)

poolOwnerDecoder :: D.Row PoolOwner
poolOwnerDecoder =
  PoolOwner
    <$> idDecoder PoolOwnerId      -- poolOwnerId
    <*> idDecoder StakeAddressId   -- poolOwnerAddrId
    <*> idDecoder PoolUpdateId     -- poolOwnerPoolUpdateId

poolOwnerEncoder :: E.Params PoolOwner
poolOwnerEncoder =
  mconcat
    [ poolOwnerId >$< idEncoder getPoolOwnerId
    , poolOwnerAddrId >$< idEncoder getStakeAddressId
    , poolOwnerPoolUpdateId >$< idEncoder getPoolUpdateId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_retire
Description: A table containing information about pool retirements.
-}
data PoolRetire = PoolRetire
  { poolRetireId :: !PoolRetireId
  , poolRetireHashId :: !PoolHashId        -- noreference
  , poolRetireCertIndex :: !Word16
  , poolRetireAnnouncedTxId :: !TxId       -- noreference -- Slot number in which the pool announced it was retiring.
  , poolRetireRetiringEpoch :: !Word64     -- sqltype=word31type -- Epoch number in which the pool will retire.
  } deriving (Eq, Show, Generic)

poolRetireDecoder :: D.Row PoolRetire
poolRetireDecoder =
  PoolRetire
    <$> idDecoder PoolRetireId -- poolRetireId
    <*> idDecoder PoolHashId -- poolRetireHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- poolRetireCertIndex
    <*> idDecoder TxId -- poolRetireAnnouncedTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolRetireRetiringEpoch

poolRetireEncoder :: E.Params PoolRetire
poolRetireEncoder =
  mconcat
    [ poolRetireId >$< idEncoder getPoolRetireId
    , poolRetireHashId >$< idEncoder getPoolHashId
    , poolRetireCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolRetireAnnouncedTxId >$< idEncoder getTxId
    , poolRetireRetiringEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_relay
Description: A table containing information about pool relays.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data PoolRelay = PoolRelay
  { poolRelayId :: !PoolRelayId
  , poolRelayUpdateId :: !PoolUpdateId  -- noreference
  , poolRelayIpv4 :: !(Maybe Text)
  , poolRelayIpv6 :: !(Maybe Text)
  , poolRelayDnsName :: !(Maybe Text)
  , poolRelayDnsSrvName :: !(Maybe Text)
  , poolRelayPort :: !(Maybe Word16)
  } deriving (Eq, Show, Generic)

poolRelayDecoder :: D.Row PoolRelay
poolRelayDecoder =
  PoolRelay
    <$> idDecoder PoolRelayId -- poolRelayId
    <*> idDecoder PoolUpdateId -- poolRelayUpdateId
    <*> D.column (D.nullable D.text) -- poolRelayIpv4
    <*> D.column (D.nullable D.text) -- poolRelayIpv6
    <*> D.column (D.nullable D.text) -- poolRelayDnsName
    <*> D.column (D.nullable D.text) -- poolRelayDnsSrvName
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- poolRelayPort

poolRelayEncoder :: E.Params PoolRelay
poolRelayEncoder =
  mconcat
    [ poolRelayId >$< idEncoder getPoolRelayId
    , poolRelayUpdateId >$< idEncoder getPoolUpdateId
    , poolRelayIpv4 >$< E.param (E.nullable E.text)
    , poolRelayIpv6 >$< E.param (E.nullable E.text)
    , poolRelayDnsName >$< E.param (E.nullable E.text)
    , poolRelayDnsSrvName >$< E.param (E.nullable E.text)
    , poolRelayPort >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delisted_pool
Description: A table containing a managed list of delisted pools.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data DelistedPool = DelistedPool
  { delistedPoolId :: !DelistedPoolId
  , delistedPoolHashRaw :: !ByteString                      -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

delistedPoolDecoder :: D.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    <$> idDecoder DelistedPoolId -- delistedPoolId
    <*> D.column (D.nonNullable D.bytea) -- delistedPoolHashRaw

delistedPoolEncoder :: E.Params DelistedPool
delistedPoolEncoder =
  mconcat
    [ delistedPoolId >$< idEncoder getDelistedPoolId
    , delistedPoolHashRaw >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: resser_pool_ticker
Description: A table containing a managed list of reserved ticker names.
  For now they are grouped under the specific hash of the pool.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data ReservedPoolTicker = ReservedPoolTicker
  { reservedPoolTickerId :: !ReservedPoolTickerId
  , reservedPoolTickerName :: !Text
  , reservedPoolTickerPoolHash :: !ByteString               -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

reservedPoolTickerDecoder :: D.Row ReservedPoolTicker
reservedPoolTickerDecoder =
  ReservedPoolTicker
    <$> idDecoder ReservedPoolTickerId -- reservedPoolTickerId
    <*> D.column (D.nonNullable D.text) -- reservedPoolTickerName
    <*> D.column (D.nonNullable D.bytea) -- reservedPoolTickerPoolHash

reservedPoolTickerEncoder :: E.Params ReservedPoolTicker
reservedPoolTickerEncoder =
  mconcat
    [ reservedPoolTickerId >$< idEncoder getReservedPoolTickerId
    , reservedPoolTickerName >$< E.param (E.nonNullable E.text)
    , reservedPoolTickerPoolHash >$< E.param (E.nonNullable E.bytea)
    ]
