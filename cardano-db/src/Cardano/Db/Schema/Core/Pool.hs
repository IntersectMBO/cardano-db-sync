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
  dbLovelaceEncoder, HasDbInfo (..)
  )
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras (contrazip6)
import Cardano.Db.Statement.Helpers (manyEncoder)

-----------------------------------------------------------------------------------------------------------------------------------
-- POOLS
-- These tables manage stake pool-related data, including pool registration, updates, and retirements.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_hash
Description: A table containing information about pool hashes.
-}
data PoolHash = PoolHash
  { poolHash_Id :: !PoolHashId
  , poolHash_HashRaw :: !ByteString  -- unique hashRaw sqltype=hash28type
  , poolHash_View :: !Text
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolHash

poolHashDecoder :: D.Row PoolHash
poolHashDecoder =
  PoolHash
    <$> idDecoder PoolHashId -- poolHash_Id
    <*> D.column (D.nonNullable D.bytea) -- poolHash_HashRaw
    <*> D.column (D.nonNullable D.text) -- poolHash_View

poolHashEncoder :: E.Params PoolHash
poolHashEncoder =
  mconcat
    [ (getPoolHashId . poolHash_Id) >$< E.param (E.nonNullable E.int8) -- poolHash_Id
    , poolHash_HashRaw >$< E.param (E.nonNullable E.bytea) --poolHashHashRaw
    , poolHash_View >$< E.param (E.nonNullable E.text) -- poolHash_View
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_stat
Description: A table containing information about pool metadata.
-}
data PoolStat = PoolStat
  { poolStat_Id :: !PoolStatId
  , poolStat_PoolHashId :: !PoolHashId   -- noreference
  , poolStat_EpochNo :: !Word64          -- sqltype=word31type
  , poolStat_NumberOfBlocks :: !DbWord64 -- sqltype=word64type
  , poolStat_NumberOfDelegators :: !DbWord64 -- sqltype=word64type
  , poolStat_Stake :: !DbWord64          -- sqltype=word64type
  , poolStat_VotingPower :: !(Maybe DbWord64) -- sqltype=word64type
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolStat

poolStatDecoder :: D.Row PoolStat
poolStatDecoder =
  PoolStat
    <$> idDecoder PoolStatId -- poolStat_Id
    <*> idDecoder PoolHashId -- poolStat_PoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolStat_EpochNo
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStat_NumberOfBlocks
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStat_NumberOfDelegators
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStat_Stake
    <*> D.column (D.nullable $ DbWord64 . fromIntegral <$> D.int8) -- poolStat_VotingPower

poolStatEncoder :: E.Params PoolStat
poolStatEncoder =
  mconcat
    [ poolStat_Id >$< idEncoder getPoolStatId
    , poolStat_PoolHashId >$< idEncoder getPoolHashId
    , poolStat_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolStat_NumberOfBlocks >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStat_NumberOfDelegators >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStat_Stake >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , poolStat_VotingPower >$< E.param (E.nullable $ fromIntegral . unDbWord64 >$< E.int8)
    ]

poolStatEncoderMany :: E.Params ([PoolHashId], [Word64], [DbWord64], [DbWord64], [DbWord64], [Maybe DbWord64])
poolStatEncoderMany =
  contrazip6
    (manyEncoder $ E.nonNullable $ getPoolHashId >$< E.int8) -- poolHash_Id
    (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int4) -- epoch_no
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- number_of_blocks
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- number_of_delegators
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.numeric) -- stake
    (manyEncoder $ E.nullable $ fromIntegral . unDbWord64 >$< E.numeric) -- voting_power

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_update
Description: A table containing information about pool updates.
-}
data PoolUpdate = PoolUpdate
  { poolUpdate_Id :: !PoolUpdateId
  , poolUpdate_HashId :: !PoolHashId        -- noreference
  , poolUpdate_CertIndex :: !Word16
  , poolUpdate_VrfKeyHash :: !ByteString    -- sqltype=hash32type
  , poolUpdate_Pledge :: !DbLovelace        -- sqltype=lovelace
  , poolUpdate_RewardAddrId :: !StakeAddressId -- noreference
  , poolUpdate_ActiveEpochNo :: !Word64
  , poolUpdate_MetaId :: !(Maybe PoolMetadataRefId) -- noreference
  , poolUpdate_Margin :: !Double            -- sqltype=percentage????
  , poolUpdate_FixedCost :: !DbLovelace     -- sqltype=lovelace
  , poolUpdate_Deposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , poolUpdate_RegisteredTxId :: !TxId      -- noreference -- Slot number in which the pool was registered.
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolUpdate

poolUpdateDecoder :: D.Row PoolUpdate
poolUpdateDecoder =
  PoolUpdate
    <$> idDecoder PoolUpdateId -- poolUpdate_Id
    <*> idDecoder PoolHashId -- poolUpdate_HashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- poolUpdate_CertIndex (Word16)
    <*> D.column (D.nonNullable D.bytea) -- poolUpdate_VrfKeyHash
    <*> dbLovelaceDecoder -- poolUpdate_Pledge
    <*> idDecoder StakeAddressId -- poolUpdate_RewardAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolUpdate_ActiveEpochNo
    <*> maybeIdDecoder PoolMetadataRefId -- poolUpdate_MetaId
    <*> D.column (D.nonNullable D.float8) -- poolUpdate_Margin
    <*> dbLovelaceDecoder -- poolUpdate_FixedCost
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- poolUpdate_Deposit
    <*> idDecoder TxId -- poolUpdate_RegisteredTxId

poolUpdateEncoder :: E.Params PoolUpdate
poolUpdateEncoder =
  mconcat
    [ poolUpdate_Id >$< idEncoder getPoolUpdateId
    , poolUpdate_HashId >$< idEncoder getPoolHashId
    , poolUpdate_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolUpdate_VrfKeyHash >$< E.param (E.nonNullable E.bytea)
    , poolUpdate_Pledge >$< dbLovelaceEncoder
    , poolUpdate_RewardAddrId >$< idEncoder getStakeAddressId
    , poolUpdate_ActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , poolUpdate_MetaId >$< maybeIdEncoder getPoolMetadataRefId
    , poolUpdate_Margin >$< E.param (E.nonNullable E.float8)
    , poolUpdate_FixedCost >$< dbLovelaceEncoder
    , poolUpdate_Deposit >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , poolUpdate_RegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_metadata_ref
Description: A table containing references to pool metadata.
-}
data PoolMetadataRef = PoolMetadataRef
  { poolMetadataRef_Id :: !PoolMetadataRefId
  , poolMetadataRef_PoolId :: !PoolHashId -- noreference
  , poolMetadataRef_Url :: !PoolUrl      -- sqltype=varchar
  , poolMetadataRef_Hash :: !ByteString  -- sqltype=hash32type
  , poolMetadataRef_RegisteredTxId :: !TxId -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolMetadataRef

poolMetadataRefDecoder :: D.Row PoolMetadataRef
poolMetadataRefDecoder =
  PoolMetadataRef
    <$> idDecoder PoolMetadataRefId -- poolMetadataRef_Id
    <*> idDecoder PoolHashId -- poolMetadataRef_PoolId
    <*> D.column (D.nonNullable (PoolUrl <$> D.text))-- poolMetadataRef_Url
    <*> D.column (D.nonNullable D.bytea) -- poolMetadataRef_Hash
    <*> idDecoder TxId -- poolMetadataRef_RegisteredTxId

poolMetadataRefEncoder :: E.Params PoolMetadataRef
poolMetadataRefEncoder =
  mconcat
    [ poolMetadataRef_Id >$< idEncoder getPoolMetadataRefId
    , poolMetadataRef_PoolId >$< idEncoder getPoolHashId
    , poolMetadataRef_Url >$< E.param (E.nonNullable (unPoolUrl >$< E.text))
    , poolMetadataRef_Hash >$< E.param (E.nonNullable E.bytea)
    , poolMetadataRef_RegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_owner
Description: A table containing information about pool owners.
-}
data PoolOwner = PoolOwner
  { poolOwner_Id :: !PoolOwnerId
  , poolOwner_AddrId :: !StakeAddressId     -- noreference
  , poolOwner_PoolUpdateId :: !PoolUpdateId -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolOwner

poolOwnerDecoder :: D.Row PoolOwner
poolOwnerDecoder =
  PoolOwner
    <$> idDecoder PoolOwnerId      -- poolOwner_Id
    <*> idDecoder StakeAddressId   -- poolOwner_AddrId
    <*> idDecoder PoolUpdateId     -- poolOwner_PoolUpdateId

poolOwnerEncoder :: E.Params PoolOwner
poolOwnerEncoder =
  mconcat
    [ poolOwner_Id >$< idEncoder getPoolOwnerId
    , poolOwner_AddrId >$< idEncoder getStakeAddressId
    , poolOwner_PoolUpdateId >$< idEncoder getPoolUpdateId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_retire
Description: A table containing information about pool retirements.
-}
data PoolRetire = PoolRetire
  { poolRetire_Id :: !PoolRetireId
  , poolRetire_HashId :: !PoolHashId        -- noreference
  , poolRetire_CertIndex :: !Word16
  , poolRetire_AnnouncedTxId :: !TxId       -- noreference -- Slot number in which the pool announced it was retiring.
  , poolRetire_RetiringEpoch :: !Word64     -- sqltype=word31type -- Epoch number in which the pool will retire.
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolRetire

poolRetireDecoder :: D.Row PoolRetire
poolRetireDecoder =
  PoolRetire
    <$> idDecoder PoolRetireId -- poolRetire_Id
    <*> idDecoder PoolHashId -- poolRetire_HashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- poolRetire_CertIndex
    <*> idDecoder TxId -- poolRetire_AnnouncedTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- poolRetire_RetiringEpoch

poolRetireEncoder :: E.Params PoolRetire
poolRetireEncoder =
  mconcat
    [ poolRetire_Id >$< idEncoder getPoolRetireId
    , poolRetire_HashId >$< idEncoder getPoolHashId
    , poolRetire_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , poolRetire_AnnouncedTxId >$< idEncoder getTxId
    , poolRetire_RetiringEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pool_relay
Description: A table containing information about pool relays.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data PoolRelay = PoolRelay
  { poolRelay_Id :: !PoolRelayId
  , poolRelay_UpdateId :: !PoolUpdateId  -- noreference
  , poolRelay_Ipv4 :: !(Maybe Text)
  , poolRelay_Ipv6 :: !(Maybe Text)
  , poolRelay_DnsName :: !(Maybe Text)
  , poolRelay_DnsSrvName :: !(Maybe Text)
  , poolRelay_Port :: !(Maybe Word16)
  } deriving (Eq, Show, Generic)

instance HasDbInfo PoolRelay

poolRelayDecoder :: D.Row PoolRelay
poolRelayDecoder =
  PoolRelay
    <$> idDecoder PoolRelayId -- poolRelay_Id
    <*> idDecoder PoolUpdateId -- poolRelay_UpdateId
    <*> D.column (D.nullable D.text) -- poolRelay_Ipv4
    <*> D.column (D.nullable D.text) -- poolRelay_Ipv6
    <*> D.column (D.nullable D.text) -- poolRelay_DnsName
    <*> D.column (D.nullable D.text) -- poolRelay_DnsSrvName
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- poolRelay_Port

poolRelayEncoder :: E.Params PoolRelay
poolRelayEncoder =
  mconcat
    [ poolRelay_Id >$< idEncoder getPoolRelayId
    , poolRelay_UpdateId >$< idEncoder getPoolUpdateId
    , poolRelay_Ipv4 >$< E.param (E.nullable E.text)
    , poolRelay_Ipv6 >$< E.param (E.nullable E.text)
    , poolRelay_DnsName >$< E.param (E.nullable E.text)
    , poolRelay_DnsSrvName >$< E.param (E.nullable E.text)
    , poolRelay_Port >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delisted_pool
Description: A table containing a managed list of delisted pools.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data DelistedPool = DelistedPool
  { delistedPool_Id :: !DelistedPoolId
  , delistedPool_HashRaw :: !ByteString                      -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

instance HasDbInfo DelistedPool

delistedPoolDecoder :: D.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    <$> idDecoder DelistedPoolId -- delistedPool_Id
    <*> D.column (D.nonNullable D.bytea) -- delistedPool_HashRaw

delistedPoolEncoder :: E.Params DelistedPool
delistedPoolEncoder =
  mconcat
    [ delistedPool_Id >$< idEncoder getDelistedPoolId
    , delistedPool_HashRaw >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: resser_pool_ticker
Description: A table containing a managed list of reserved ticker names.
  For now they are grouped under the specific hash of the pool.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data ReservedPoolTicker = ReservedPoolTicker
  { reservedPoolTicker_Id :: !ReservedPoolTickerId
  , reservedPoolTicker_Name :: !Text
  , reservedPoolTicker_PoolHash :: !ByteString               -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

instance HasDbInfo ReservedPoolTicker

reservedPoolTickerDecoder :: D.Row ReservedPoolTicker
reservedPoolTickerDecoder =
  ReservedPoolTicker
    <$> idDecoder ReservedPoolTickerId -- reservedPoolTicker_Id
    <*> D.column (D.nonNullable D.text) -- reservedPoolTicker_Name
    <*> D.column (D.nonNullable D.bytea) -- reservedPoolTicker_PoolHash

reservedPoolTickerEncoder :: E.Params ReservedPoolTicker
reservedPoolTickerEncoder =
  mconcat
    [ reservedPoolTicker_Id >$< idEncoder getReservedPoolTickerId
    , reservedPoolTicker_Name >$< E.param (E.nonNullable E.text)
    , reservedPoolTicker_PoolHash >$< E.param (E.nonNullable E.bytea)
    ]
