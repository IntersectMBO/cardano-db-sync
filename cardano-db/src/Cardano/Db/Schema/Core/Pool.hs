{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.Pool where

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Types (
  PoolUrl (..),
  unPoolUrl,
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  DbLovelace (..),
  DbWord64 (..),
  dbLovelaceDecoder,
  dbLovelaceEncoder,
 )
import Contravariant.Extras (contrazip6)
import Data.Functor.Contravariant ((>$<))
import Hasql.Decoders as D
import Hasql.Encoders as E

-----------------------------------------------------------------------------------------------------------------------------------
-- POOLS
-- These tables manage stake pool-related data, including pool registration, updates, and retirements.
-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_hash
-- Description: A table containing information about pool hashes.
data PoolHash = PoolHash
  { poolHashHashRaw :: !ByteString -- unique hashRaw sqltype=hash28type
  , poolHashView :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key PoolHash = PoolHashId
instance DbInfo PoolHash where
  uniqueFields _ = ["hash_raw"]

entityPoolHashDecoder :: D.Row (Entity PoolHash)
entityPoolHashDecoder =
  Entity
    <$> idDecoder PoolHashId
    <*> poolHashDecoder

poolHashDecoder :: D.Row PoolHash
poolHashDecoder =
  PoolHash
    <$> D.column (D.nonNullable D.bytea) -- poolHashHashRaw
    <*> D.column (D.nonNullable D.text) -- poolHashView

entityPoolHashEncoder :: E.Params (Entity PoolHash)
entityPoolHashEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolHashId
    , entityVal >$< poolHashEncoder
    ]

poolHashEncoder :: E.Params PoolHash
poolHashEncoder =
  mconcat
    [ poolHashHashRaw >$< E.param (E.nonNullable E.bytea) -- poolHashHashRaw
    , poolHashView >$< E.param (E.nonNullable E.text) -- poolHashView
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_stat
-- Description: A table containing information about pool metadata.
data PoolStat = PoolStat
  { poolStatPoolHashId :: !PoolHashId -- noreference
  , poolStatEpochNo :: !Word64 -- sqltype=word31type
  , poolStatNumberOfBlocks :: !DbWord64 -- sqltype=word64type
  , poolStatNumberOfDelegators :: !DbWord64 -- sqltype=word64type
  , poolStatStake :: !DbWord64 -- sqltype=word64type
  , poolStatVotingPower :: !(Maybe DbWord64) -- sqltype=word64type
  }
  deriving (Eq, Show, Generic)

type instance Key PoolStat = PoolStatId
instance DbInfo PoolStat

entityPoolStatDecoder :: D.Row (Entity PoolStat)
entityPoolStatDecoder =
  Entity
    <$> idDecoder PoolStatId
    <*> poolStatDecoder

poolStatDecoder :: D.Row PoolStat
poolStatDecoder =
  PoolStat
    <$> idDecoder PoolHashId -- poolStatPoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolStatEpochNo
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatNumberOfBlocks
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatNumberOfDelegators
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatStake
    <*> D.column (D.nullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStatVotingPower

entityPoolStatEncoder :: E.Params (Entity PoolStat)
entityPoolStatEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolStatId
    , entityVal >$< poolStatEncoder
    ]

poolStatEncoder :: E.Params PoolStat
poolStatEncoder =
  mconcat
    [ poolStatPoolHashId >$< idEncoder getPoolHashId
    , poolStatEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolStatNumberOfBlocks >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatNumberOfDelegators >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatStake >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStatVotingPower >$< E.param (E.nullable $ fromIntegral . unDbWord64 >$< E.int8)
    ]

poolStatBulkEncoder :: E.Params ([PoolHashId], [Word64], [DbWord64], [DbWord64], [DbWord64], [Maybe DbWord64])
poolStatBulkEncoder =
  contrazip6
    (bulkEncoder $ E.nonNullable $ getPoolHashId >$< E.int8) -- poolHashId
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int4) -- epoch_no
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- number_of_blocks
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- number_of_delegators
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- stake
    (bulkEncoder $ E.nullable $ fromIntegral . unDbWord64 >$< E.numeric) -- voting_power

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_update
-- Description: A table containing information about pool updates.
data PoolUpdate = PoolUpdate
  { poolUpdateHashId :: !PoolHashId -- noreference
  , poolUpdateCertIndex :: !Word16
  , poolUpdateVrfKeyHash :: !ByteString -- sqltype=hash32type
  , poolUpdatePledge :: !DbLovelace -- sqltype=lovelace
  , poolUpdateRewardAddrId :: !StakeAddressId -- noreference
  , poolUpdateActiveEpochNo :: !Word64
  , poolUpdateMetaId :: !(Maybe PoolMetadataRefId) -- noreference
  , poolUpdateMargin :: !Double -- sqltype=percentage????
  , poolUpdateFixedCost :: !DbLovelace -- sqltype=lovelace
  , poolUpdateDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , poolUpdateRegisteredTxId :: !TxId -- noreference -- Slot number in which the pool was registered.
  }
  deriving (Eq, Show, Generic)

type instance Key PoolUpdate = PoolUpdateId
instance DbInfo PoolUpdate

entityPoolUpdateDecoder :: D.Row (Entity PoolUpdate)
entityPoolUpdateDecoder =
  Entity
    <$> idDecoder PoolUpdateId
    <*> poolUpdateDecoder

poolUpdateDecoder :: D.Row PoolUpdate
poolUpdateDecoder =
  PoolUpdate
    <$> idDecoder PoolHashId -- poolUpdateHashId
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

entityPoolUpdateEncoder :: E.Params (Entity PoolUpdate)
entityPoolUpdateEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolUpdateId
    , entityVal >$< poolUpdateEncoder
    ]

poolUpdateEncoder :: E.Params PoolUpdate
poolUpdateEncoder =
  mconcat
    [ poolUpdateHashId >$< idEncoder getPoolHashId
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

-- |
-- Table Name: pool_metadata_ref
-- Description: A table containing references to pool metadata.
data PoolMetadataRef = PoolMetadataRef
  { poolMetadataRefPoolId :: !PoolHashId -- noreference
  , poolMetadataRefUrl :: !PoolUrl -- sqltype=varchar
  , poolMetadataRefHash :: !ByteString -- sqltype=hash32type
  , poolMetadataRefRegisteredTxId :: !TxId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key PoolMetadataRef = PoolMetadataRefId
instance DbInfo PoolMetadataRef

entityPoolMetadataRefDecoder :: D.Row (Entity PoolMetadataRef)
entityPoolMetadataRefDecoder =
  Entity
    <$> idDecoder PoolMetadataRefId
    <*> poolMetadataRefDecoder

poolMetadataRefDecoder :: D.Row PoolMetadataRef
poolMetadataRefDecoder =
  PoolMetadataRef
    <$> idDecoder PoolHashId -- poolMetadataRefPoolId
    <*> D.column (D.nonNullable (PoolUrl <$> D.text)) -- poolMetadataRefUrl
    <*> D.column (D.nonNullable D.bytea) -- poolMetadataRefHash
    <*> idDecoder TxId -- poolMetadataRefRegisteredTxId

entityPoolMetadataRefEncoder :: E.Params (Entity PoolMetadataRef)
entityPoolMetadataRefEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolMetadataRefId
    , entityVal >$< poolMetadataRefEncoder
    ]

poolMetadataRefEncoder :: E.Params PoolMetadataRef
poolMetadataRefEncoder =
  mconcat
    [ poolMetadataRefPoolId >$< idEncoder getPoolHashId
    , poolMetadataRefUrl >$< E.param (E.nonNullable (unPoolUrl >$< E.text))
    , poolMetadataRefHash >$< E.param (E.nonNullable E.bytea)
    , poolMetadataRefRegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_owner
-- Description: A table containing information about pool owners.
data PoolOwner = PoolOwner
  { poolOwnerAddrId :: !StakeAddressId -- noreference
  , poolOwnerPoolUpdateId :: !PoolUpdateId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key PoolOwner = PoolOwnerId
instance DbInfo PoolOwner

entityPoolOwnerDecoder :: D.Row (Entity PoolOwner)
entityPoolOwnerDecoder =
  Entity
    <$> idDecoder PoolOwnerId
    <*> poolOwnerDecoder

poolOwnerDecoder :: D.Row PoolOwner
poolOwnerDecoder =
  PoolOwner
    <$> idDecoder StakeAddressId -- poolOwnerAddrId
    <*> idDecoder PoolUpdateId -- poolOwnerPoolUpdateId

entityPoolOwnerEncoder :: E.Params (Entity PoolOwner)
entityPoolOwnerEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolOwnerId
    , entityVal >$< poolOwnerEncoder
    ]

poolOwnerEncoder :: E.Params PoolOwner
poolOwnerEncoder =
  mconcat
    [ poolOwnerAddrId >$< idEncoder getStakeAddressId
    , poolOwnerPoolUpdateId >$< idEncoder getPoolUpdateId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_retire
-- Description: A table containing information about pool retirements.
data PoolRetire = PoolRetire
  { poolRetireHashId :: !PoolHashId -- noreference
  , poolRetireCertIndex :: !Word16
  , poolRetireAnnouncedTxId :: !TxId -- noreference -- Slot number in which the pool announced it was retiring.
  , poolRetireRetiringEpoch :: !Word64 -- sqltype=word31type -- Epoch number in which the pool will retire.
  }
  deriving (Eq, Show, Generic)

type instance Key PoolRetire = PoolRetireId
instance DbInfo PoolRetire

entityPoolRetireDecoder :: D.Row (Entity PoolRetire)
entityPoolRetireDecoder =
  Entity
    <$> idDecoder PoolRetireId
    <*> poolRetireDecoder

poolRetireDecoder :: D.Row PoolRetire
poolRetireDecoder =
  PoolRetire
    <$> idDecoder PoolHashId -- poolRetireHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- poolRetireCertIndex
    <*> idDecoder TxId -- poolRetireAnnouncedTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolRetireRetiringEpoch

entityPoolRetireEncoder :: E.Params (Entity PoolRetire)
entityPoolRetireEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolRetireId
    , entityVal >$< poolRetireEncoder
    ]

poolRetireEncoder :: E.Params PoolRetire
poolRetireEncoder =
  mconcat
    [ poolRetireHashId >$< idEncoder getPoolHashId
    , poolRetireCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolRetireAnnouncedTxId >$< idEncoder getTxId
    , poolRetireRetiringEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pool_relay
-- Description: A table containing information about pool relays.

-----------------------------------------------------------------------------------------------------------------------------------
data PoolRelay = PoolRelay
  { poolRelayUpdateId :: !PoolUpdateId -- noreference
  , poolRelayIpv4 :: !(Maybe Text)
  , poolRelayIpv6 :: !(Maybe Text)
  , poolRelayDnsName :: !(Maybe Text)
  , poolRelayDnsSrvName :: !(Maybe Text)
  , poolRelayPort :: !(Maybe Word16)
  }
  deriving (Eq, Show, Generic)

type instance Key PoolRelay = PoolRelayId
instance DbInfo PoolRelay

entityPoolRelayDecoder :: D.Row (Entity PoolRelay)
entityPoolRelayDecoder =
  Entity
    <$> idDecoder PoolRelayId
    <*> poolRelayDecoder

poolRelayDecoder :: D.Row PoolRelay
poolRelayDecoder =
  PoolRelay
    <$> idDecoder PoolUpdateId -- poolRelayUpdateId
    <*> D.column (D.nullable D.text) -- poolRelayIpv4
    <*> D.column (D.nullable D.text) -- poolRelayIpv6
    <*> D.column (D.nullable D.text) -- poolRelayDnsName
    <*> D.column (D.nullable D.text) -- poolRelayDnsSrvName
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- poolRelayPort

entityPoolRelayEncoder :: E.Params (Entity PoolRelay)
entityPoolRelayEncoder =
  mconcat
    [ entityKey >$< idEncoder getPoolRelayId
    , entityVal >$< poolRelayEncoder
    ]

poolRelayEncoder :: E.Params PoolRelay
poolRelayEncoder =
  mconcat
    [ poolRelayUpdateId >$< idEncoder getPoolUpdateId
    , poolRelayIpv4 >$< E.param (E.nullable E.text)
    , poolRelayIpv6 >$< E.param (E.nullable E.text)
    , poolRelayDnsName >$< E.param (E.nullable E.text)
    , poolRelayDnsSrvName >$< E.param (E.nullable E.text)
    , poolRelayPort >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: delisted_pool
-- Description: A table containing a managed list of delisted pools.

-----------------------------------------------------------------------------------------------------------------------------------

newtype DelistedPool = DelistedPool
  { delistedPoolHashRaw :: ByteString -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key DelistedPool = DelistedPoolId
instance DbInfo DelistedPool where
  uniqueFields _ = ["hash_raw"]

entityDelistedPoolDecoder :: D.Row (Entity DelistedPool)
entityDelistedPoolDecoder =
  Entity
    <$> idDecoder DelistedPoolId
    <*> delistedPoolDecoder

delistedPoolDecoder :: D.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    <$> D.column (D.nonNullable D.bytea) -- delistedPoolHashRaw

entityDelistedPoolEncoder :: E.Params (Entity DelistedPool)
entityDelistedPoolEncoder =
  mconcat
    [ entityKey >$< idEncoder getDelistedPoolId
    , entityVal >$< delistedPoolEncoder
    ]

delistedPoolEncoder :: E.Params DelistedPool
delistedPoolEncoder = delistedPoolHashRaw >$< E.param (E.nonNullable E.bytea)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: resser_pool_ticker
-- Description: A table containing a managed list of reserved ticker names.
--   For now they are grouped under the specific hash of the pool.

-----------------------------------------------------------------------------------------------------------------------------------
data ReservedPoolTicker = ReservedPoolTicker
  { reservedPoolTickerName :: !Text
  , reservedPoolTickerPoolHash :: !ByteString -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key ReservedPoolTicker = ReservedPoolTickerId
instance DbInfo ReservedPoolTicker where
  uniqueFields _ = ["name"]

entityReservedPoolTickerDecoder :: D.Row (Entity ReservedPoolTicker)
entityReservedPoolTickerDecoder =
  Entity
    <$> idDecoder ReservedPoolTickerId
    <*> reservedPoolTickerDecoder

reservedPoolTickerDecoder :: D.Row ReservedPoolTicker
reservedPoolTickerDecoder =
  ReservedPoolTicker
    <$> D.column (D.nonNullable D.text) -- reservedPoolTickerName
    <*> D.column (D.nonNullable D.bytea) -- reservedPoolTickerPoolHash

entityReservedPoolTickerEncoder :: E.Params (Entity ReservedPoolTicker)
entityReservedPoolTickerEncoder =
  mconcat
    [ entityKey >$< idEncoder getReservedPoolTickerId
    , entityVal >$< reservedPoolTickerEncoder
    ]

reservedPoolTickerEncoder :: E.Params ReservedPoolTicker
reservedPoolTickerEncoder =
  mconcat
    [ reservedPoolTickerName >$< E.param (E.nonNullable E.text)
    , reservedPoolTickerPoolHash >$< E.param (E.nonNullable E.bytea)
    ]
