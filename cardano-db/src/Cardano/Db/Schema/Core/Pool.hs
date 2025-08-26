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

import Contravariant.Extras (contrazip6)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (
  PoolUrl (..),
  unPoolUrl,
 )
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  DbLovelace (..),
  DbWord64 (..),
  dbLovelaceEncoder,
  dbWord64ValueEncoder,
 )

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

type instance Key PoolHash = Id.PoolHashId
instance DbInfo PoolHash where
  uniqueFields _ = ["hash_raw"]

poolHashEncoder :: E.Params PoolHash
poolHashEncoder =
  mconcat
    [ poolHashHashRaw >$< E.param (E.nonNullable E.bytea) -- poolHashHashRaw
    , poolHashView >$< E.param (E.nonNullable E.text) -- poolHashView
    ]

-- |
-- Table Name: pool_stat
-- Description: A table containing information about pool metadata.
data PoolStat = PoolStat
  { poolStatPoolHashId :: !Id.PoolHashId -- noreference
  , poolStatEpochNo :: !Word64 -- sqltype=word31type
  , poolStatNumberOfBlocks :: !DbWord64 -- sqltype=word64type
  , poolStatNumberOfDelegators :: !DbWord64 -- sqltype=word64type
  , poolStatStake :: !DbWord64 -- sqltype=word64type
  , poolStatVotingPower :: !(Maybe DbWord64) -- sqltype=word64type
  }
  deriving (Eq, Show, Generic)

type instance Key PoolStat = Id.PoolStatId

instance DbInfo PoolStat where
  unnestParamTypes _ =
    [ ("pool_hash_id", "bigint[]")
    , ("epoch_no", "integer[]")
    , ("number_of_blocks", "numeric[]")
    , ("number_of_delegators", "numeric[]")
    , ("stake", "numeric[]")
    , ("voting_power", "numeric[]")
    ]

poolStatBulkEncoder :: E.Params ([Id.PoolHashId], [Word64], [DbWord64], [DbWord64], [DbWord64], [Maybe DbWord64])
poolStatBulkEncoder =
  contrazip6
    (bulkEncoder $ E.nonNullable $ Id.getPoolHashId >$< E.int8) -- poolHashId
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int4) -- epoch_no
    (bulkEncoder $ E.nonNullable dbWord64ValueEncoder) -- number_of_blocks
    (bulkEncoder $ E.nonNullable dbWord64ValueEncoder) -- number_of_delegators
    (bulkEncoder $ E.nonNullable dbWord64ValueEncoder) -- stake
    (bulkEncoder $ E.nullable dbWord64ValueEncoder) -- voting_power

-- |
-- Table Name: pool_update
-- Description: A table containing information about pool updates.
data PoolUpdate = PoolUpdate
  { poolUpdateHashId :: !Id.PoolHashId -- noreference
  , poolUpdateCertIndex :: !Word16
  , poolUpdateVrfKeyHash :: !ByteString -- sqltype=hash32type
  , poolUpdatePledge :: !DbLovelace -- sqltype=lovelace
  , poolUpdateActiveEpochNo :: !Word64
  , poolUpdateMetaId :: !(Maybe Id.PoolMetadataRefId) -- noreference
  , poolUpdateMargin :: !Double -- sqltype=percentage????
  , poolUpdateFixedCost :: !DbLovelace -- sqltype=lovelace
  , poolUpdateRegisteredTxId :: !Id.TxId -- noreference -- Slot number in which the pool was registered.
  , poolUpdateRewardAddrId :: !Id.StakeAddressId -- noreference
  , poolUpdateDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  }
  deriving (Eq, Show, Generic)

type instance Key PoolUpdate = Id.PoolUpdateId
instance DbInfo PoolUpdate

poolUpdateEncoder :: E.Params PoolUpdate
poolUpdateEncoder =
  mconcat
    [ poolUpdateHashId >$< Id.idEncoder Id.getPoolHashId
    , poolUpdateCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolUpdateVrfKeyHash >$< E.param (E.nonNullable E.bytea)
    , poolUpdatePledge >$< dbLovelaceEncoder
    , poolUpdateActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolUpdateMetaId >$< Id.maybeIdEncoder Id.getPoolMetadataRefId
    , poolUpdateMargin >$< E.param (E.nonNullable E.float8)
    , poolUpdateFixedCost >$< dbLovelaceEncoder
    , poolUpdateRegisteredTxId >$< Id.idEncoder Id.getTxId
    , poolUpdateRewardAddrId >$< Id.idEncoder Id.getStakeAddressId
    , poolUpdateDeposit >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    ]

-- |
-- Table Name: pool_metadata_ref
-- Description: A table containing references to pool metadata.
data PoolMetadataRef = PoolMetadataRef
  { poolMetadataRefPoolId :: !Id.PoolHashId -- noreference
  , poolMetadataRefUrl :: !PoolUrl -- sqltype=varchar
  , poolMetadataRefHash :: !ByteString -- sqltype=hash32type
  , poolMetadataRefRegisteredTxId :: !Id.TxId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key PoolMetadataRef = Id.PoolMetadataRefId
instance DbInfo PoolMetadataRef

poolMetadataRefEncoder :: E.Params PoolMetadataRef
poolMetadataRefEncoder =
  mconcat
    [ poolMetadataRefPoolId >$< Id.idEncoder Id.getPoolHashId
    , poolMetadataRefUrl >$< E.param (E.nonNullable (unPoolUrl >$< E.text))
    , poolMetadataRefHash >$< E.param (E.nonNullable E.bytea)
    , poolMetadataRefRegisteredTxId >$< Id.idEncoder Id.getTxId
    ]

-- |
-- Table Name: pool_owner
-- Description: A table containing information about pool owners.
data PoolOwner = PoolOwner
  { poolOwnerAddrId :: !Id.StakeAddressId -- noreference
  , poolOwnerPoolUpdateId :: !Id.PoolUpdateId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key PoolOwner = Id.PoolOwnerId
instance DbInfo PoolOwner

poolOwnerEncoder :: E.Params PoolOwner
poolOwnerEncoder =
  mconcat
    [ poolOwnerAddrId >$< Id.idEncoder Id.getStakeAddressId
    , poolOwnerPoolUpdateId >$< Id.idEncoder Id.getPoolUpdateId
    ]

-- |
-- Table Name: pool_retire
-- Description: A table containing information about pool retirements.
data PoolRetire = PoolRetire
  { poolRetireHashId :: !Id.PoolHashId -- noreference
  , poolRetireCertIndex :: !Word16
  , poolRetireAnnouncedTxId :: !Id.TxId -- noreference -- Slot number in which the pool announced it was retiring.
  , poolRetireRetiringEpoch :: !Word64 -- sqltype=word31type -- Epoch number in which the pool will retire.
  }
  deriving (Eq, Show, Generic)

type instance Key PoolRetire = Id.PoolRetireId
instance DbInfo PoolRetire

poolRetireEncoder :: E.Params PoolRetire
poolRetireEncoder =
  mconcat
    [ poolRetireHashId >$< Id.idEncoder Id.getPoolHashId
    , poolRetireCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolRetireAnnouncedTxId >$< Id.idEncoder Id.getTxId
    , poolRetireRetiringEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-- |
-- Table Name: pool_relay
-- Description: A table containing information about pool relays.
data PoolRelay = PoolRelay
  { poolRelayUpdateId :: !Id.PoolUpdateId -- noreference
  , poolRelayIpv4 :: !(Maybe Text)
  , poolRelayIpv6 :: !(Maybe Text)
  , poolRelayDnsName :: !(Maybe Text)
  , poolRelayDnsSrvName :: !(Maybe Text)
  , poolRelayPort :: !(Maybe Word16)
  }
  deriving (Eq, Show, Generic)

type instance Key PoolRelay = Id.PoolRelayId
instance DbInfo PoolRelay

poolRelayEncoder :: E.Params PoolRelay
poolRelayEncoder =
  mconcat
    [ poolRelayUpdateId >$< Id.idEncoder Id.getPoolUpdateId
    , poolRelayIpv4 >$< E.param (E.nullable E.text)
    , poolRelayIpv6 >$< E.param (E.nullable E.text)
    , poolRelayDnsName >$< E.param (E.nullable E.text)
    , poolRelayDnsSrvName >$< E.param (E.nullable E.text)
    , poolRelayPort >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    ]

-- |
-- Table Name: delisted_pool
-- Description: A table containing a managed list of delisted pools.
newtype DelistedPool = DelistedPool
  { delistedPoolHashRaw :: ByteString -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key DelistedPool = Id.DelistedPoolId
instance DbInfo DelistedPool where
  uniqueFields _ = ["hash_raw"]

delistedPoolDecoder :: D.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    <$> D.column (D.nonNullable D.bytea) -- delistedPoolHashRaw

delistedPoolEncoder :: E.Params DelistedPool
delistedPoolEncoder = delistedPoolHashRaw >$< E.param (E.nonNullable E.bytea)

-- |
-- Table Name: resser_pool_ticker
-- Description: A table containing a managed list of reserved ticker names.
-- For now they are grouped under the specific hash of the pool.
data ReservedPoolTicker = ReservedPoolTicker
  { reservedPoolTickerName :: !Text
  , reservedPoolTickerPoolHash :: !ByteString -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key ReservedPoolTicker = Id.ReservedPoolTickerId
instance DbInfo ReservedPoolTicker where
  uniqueFields _ = ["name"]

entityReservedPoolTickerDecoder :: D.Row (Entity ReservedPoolTicker)
entityReservedPoolTickerDecoder =
  Entity
    <$> Id.idDecoder Id.ReservedPoolTickerId
    <*> reservedPoolTickerDecoder

reservedPoolTickerDecoder :: D.Row ReservedPoolTicker
reservedPoolTickerDecoder =
  ReservedPoolTicker
    <$> D.column (D.nonNullable D.text) -- reservedPoolTickerName
    <*> D.column (D.nonNullable D.bytea) -- reservedPoolTickerPoolHash

reservedPoolTickerEncoder :: E.Params ReservedPoolTicker
reservedPoolTickerEncoder =
  mconcat
    [ reservedPoolTickerName >$< E.param (E.nonNullable E.text)
    , reservedPoolTickerPoolHash >$< E.param (E.nonNullable E.bytea)
    ]
