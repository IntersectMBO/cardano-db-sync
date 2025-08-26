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

module Cardano.Db.Schema.Core.Base where

import Contravariant.Extras (contrazip4)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder, utcTimeAsTimestampEncoder)
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  DbLovelace (..),
  DbWord64 (..),
  ScriptPurpose,
  ScriptType,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  dbWord64ValueEncoder,
  maybeDbWord64Decoder,
  maybeDbWord64Encoder,
  scriptPurposeEncoder,
  scriptTypeEncoder,
 )

-- We use camelCase here in the Haskell schema definition and 'persistLowerCase'
-- specifies that all the table and column names are converted to lower snake case.

-- All NULL-able fields other than 'epochNo' are NULL for EBBs, whereas 'epochNo' is
-- only NULL for the genesis block.

-----------------------------------------------------------------------------------------------------------------------------------
-- BASE TABLES
-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.
-----------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: block
-- Description: Stores information about individual blocks in the blockchain, including their hash, size,
--   and the transactions they contain.
-----------------------------------------------------------------------------------------------------------------------------------
data Block = Block
  { blockHash :: !ByteString -- sqltype=hash32type
  , blockEpochNo :: !(Maybe Word64) -- sqltype=word31type
  , blockSlotNo :: !(Maybe Word64) -- sqltype=word63type
  , blockEpochSlotNo :: !(Maybe Word64) -- sqltype=word31type
  , blockBlockNo :: !(Maybe Word64) -- sqltype=word31type
  , blockPreviousId :: !(Maybe BlockId) -- noreference
  , blockSlotLeaderId :: !SlotLeaderId -- noreference
  , blockSize :: !Word64 -- sqltype=word31type
  , blockTime :: !UTCTime -- sqltype=timestamp
  , blockTxCount :: !Word64
  , blockProtoMajor :: !Word16 -- sqltype=word31type
  , blockProtoMinor :: !Word16 -- sqltype=word31type
  -- Shelley specific
  , blockVrfKey :: !(Maybe Text)
  , blockOpCert :: !(Maybe ByteString) -- sqltype=hash32type
  , blockOpCertCounter :: !(Maybe Word64) -- sqltype=hash63type
  }
  deriving (Eq, Show, Generic)

type instance Key Block = BlockId
instance DbInfo Block where
  uniqueFields _ = ["hash"]

entityBlockDecoder :: D.Row (Entity Block)
entityBlockDecoder =
  Entity
    <$> idDecoder BlockId
    <*> blockDecoder

blockDecoder :: D.Row Block
blockDecoder =
  Block
    <$> D.column (D.nonNullable D.bytea) -- blockHash
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockEpochNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockSlotNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockEpochSlotNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockBlockNo
    <*> maybeIdDecoder BlockId -- blockPreviousId
    <*> idDecoder SlotLeaderId -- blockSlotLeaderId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- blockSize
    <*> D.column (D.nonNullable utcTimeAsTimestampDecoder) -- blockTime
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- blockTxCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- blockProtoMajor
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- blockProtoMinor
    <*> D.column (D.nullable D.text) -- blockVrfKey
    <*> D.column (D.nullable D.bytea) -- blockOpCert
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockOpCertCounter

blockEncoder :: E.Params Block
blockEncoder =
  mconcat
    [ blockHash >$< E.param (E.nonNullable E.bytea)
    , blockEpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , blockSlotNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , blockEpochSlotNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , blockBlockNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , blockPreviousId >$< maybeIdEncoder getBlockId
    , blockSlotLeaderId >$< idEncoder getSlotLeaderId
    , blockSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , blockTime >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
    , blockTxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , blockProtoMajor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , blockProtoMinor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , blockVrfKey >$< E.param (E.nullable E.text)
    , blockOpCert >$< E.param (E.nullable E.bytea)
    , blockOpCertCounter >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: tx
-- Description: Contains data related to transactions, such as transaction ID, inputs, outputs, and metadata
data Tx = Tx
  { txHash :: !ByteString -- sqltype=hash32type
  , txBlockId :: !BlockId -- noreference -- This type is the primary key for the 'block' table.
  , txBlockIndex :: !Word64 -- sqltype=word31type -- The index of this transaction within the block.
  , txOutSum :: !DbLovelace -- sqltype=lovelace
  , txFee :: !DbLovelace -- sqltype=lovelace
  , txDeposit :: !(Maybe Int64) -- Needs to allow negaitve values.
  , txSize :: !Word64 -- sqltype=word31type
  -- New for Allega
  , txInvalidBefore :: !(Maybe DbWord64) -- sqltype=word64type
  , txInvalidHereafter :: !(Maybe DbWord64) -- sqltype=word64type
  -- New for Alonzo
  , txValidContract :: !Bool -- False if the contract is invalid, True otherwise.
  , txScriptSize :: !Word64 -- sqltype=word31type
  -- New for Conway
  , txTreasuryDonation :: !DbLovelace -- sqltype=lovelace default=0
  }
  deriving (Show, Eq, Generic)

type instance Key Tx = TxId
instance DbInfo Tx where
  uniqueFields _ = ["hash"]

entityTxDecoder :: D.Row (Entity Tx)
entityTxDecoder =
  Entity
    <$> idDecoder TxId
    <*> txDecoder

txDecoder :: D.Row Tx
txDecoder =
  Tx
    <$> D.column (D.nonNullable D.bytea) -- txHash
    <*> idDecoder BlockId -- txBlockId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txBlockIndex
    <*> dbLovelaceDecoder -- txOutSum
    <*> dbLovelaceDecoder -- txFee
    <*> D.column (D.nullable D.int8) -- txDeposit
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txSize
    <*> maybeDbWord64Decoder -- txInvalidBefore
    <*> maybeDbWord64Decoder -- txInvalidHereafter
    <*> D.column (D.nonNullable D.bool) -- txValidContract
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txScriptSize
    <*> dbLovelaceDecoder -- txTreasuryDonation

txEncoder :: E.Params Tx
txEncoder =
  mconcat
    [ txHash >$< E.param (E.nonNullable E.bytea)
    , txBlockId >$< idEncoder getBlockId
    , txBlockIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutSum >$< dbLovelaceEncoder
    , txFee >$< dbLovelaceEncoder
    , txDeposit >$< E.param (E.nullable E.int8)
    , txSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txInvalidBefore >$< maybeDbWord64Encoder
    , txInvalidHereafter >$< maybeDbWord64Encoder
    , txValidContract >$< E.param (E.nonNullable E.bool)
    , txScriptSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txTreasuryDonation >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: txmetadata
-- Description: Contains metadata associated with transactions, such as metadata ID, key, and date.
-----------------------------------------------------------------------------------------------------------------------------------
data TxMetadata = TxMetadata
  { txMetadataKey :: !DbWord64 -- sqltype=word64type
  , txMetadataJson :: !(Maybe Text) -- sqltype=jsonb
  , txMetadataBytes :: !ByteString -- sqltype=bytea
  , txMetadataTxId :: !TxId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key TxMetadata = TxMetadataId

instance DbInfo TxMetadata where
  jsonbFields _ = ["json"]
  unnestParamTypes _ =
    [ ("key", "numeric[]")
    , ("json", "text[]")
    , ("bytes", "bytea[]")
    , ("tx_id", "bigint[]")
    ]

txMetadataBulkEncoder :: E.Params ([DbWord64], [Maybe Text], [ByteString], [TxId])
txMetadataBulkEncoder =
  contrazip4
    (bulkEncoder $ E.nonNullable dbWord64ValueEncoder)
    (bulkEncoder $ E.nullable E.text)
    (bulkEncoder $ E.nonNullable E.bytea)
    (bulkEncoder $ E.nonNullable $ getTxId >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: txin
-- Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-----------------------------------------------------------------------------------------------------------------------------------
data TxIn = TxIn
  { txInTxInId :: !TxId -- The transaction where this is used as an input.
  , txInTxOutId :: !TxId -- The transaction where this was created as an output.
  , txInTxOutIndex :: !Word64 -- sqltype=txindex
  , txInRedeemerId :: !(Maybe RedeemerId)
  }
  deriving (Show, Eq, Generic)

type instance Key TxIn = TxInId

instance DbInfo TxIn where
  unnestParamTypes _ =
    [ ("tx_in_id", "bigint[]")
    , ("tx_out_id", "bigint[]")
    , ("tx_out_index", "bigint[]")
    , ("redeemer_id", "bigint[]")
    ]

txInDecoder :: D.Row TxIn
txInDecoder =
  TxIn
    <$> idDecoder TxId -- txInTxInId
    <*> idDecoder TxId -- txInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txInTxOutIndex
    <*> maybeIdDecoder RedeemerId -- txInRedeemerId

entityTxInEncoder :: E.Params (Entity TxIn)
entityTxInEncoder =
  mconcat
    [ entityKey >$< idEncoder getTxInId
    , entityVal >$< txInEncoder
    ]

txInEncoder :: E.Params TxIn
txInEncoder =
  mconcat
    [ txInTxInId >$< idEncoder getTxId
    , txInTxOutId >$< idEncoder getTxId
    , txInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txInRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

encodeTxInBulk :: E.Params ([TxId], [TxId], [Word64], [Maybe RedeemerId])
encodeTxInBulk =
  contrazip4
    (bulkEncoder $ E.nonNullable $ getTxId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ getTxId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nullable $ getRedeemerId >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: collateral_txin
-- Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-----------------------------------------------------------------------------------------------------------------------------------
data CollateralTxIn = CollateralTxIn
  { collateralTxInTxInId :: !TxId -- noreference -- The transaction where this is used as an input.
  , collateralTxInTxOutId :: !TxId -- noreference -- The transaction where this was created as an output.
  , collateralTxInTxOutIndex :: !Word64 -- sqltype=txindex
  }
  deriving (Show, Eq, Generic)

type instance Key CollateralTxIn = CollateralTxInId
instance DbInfo CollateralTxIn

collateralTxInEncoder :: E.Params CollateralTxIn
collateralTxInEncoder =
  mconcat
    [ collateralTxInTxInId >$< idEncoder getTxId
    , collateralTxInTxOutId >$< idEncoder getTxId
    , collateralTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: reference_txin
-- Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-----------------------------------------------------------------------------------------------------------------------------------
data ReferenceTxIn = ReferenceTxIn
  { referenceTxInTxInId :: !TxId -- noreference -- The transaction where this is used as an input.
  , referenceTxInTxOutId :: !TxId -- noreference -- The transaction where this was created as an output.
  , referenceTxInTxOutIndex :: !Word64 -- sqltype=txindex
  }
  deriving (Show, Eq, Generic)

type instance Key ReferenceTxIn = ReferenceTxInId
instance DbInfo ReferenceTxIn

referenceTxInEncoder :: E.Params ReferenceTxIn
referenceTxInEncoder =
  mconcat
    [ referenceTxInTxInId >$< idEncoder getTxId
    , referenceTxInTxOutId >$< idEncoder getTxId
    , referenceTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: reverse_index
-- Description: Provides a reverse lookup mechanism for transaction inputs, allowing efficient querying of the origin of funds.
-----------------------------------------------------------------------------------------------------------------------------------
data ReverseIndex = ReverseIndex
  { reverseIndexBlockId :: !BlockId -- noreference
  , reverseIndexMinIds :: !Text
  }
  deriving (Show, Eq, Generic)

type instance Key ReverseIndex = ReverseIndexId
instance DbInfo ReverseIndex

entityReverseIndexEncoder :: E.Params (Entity ReverseIndex)
entityReverseIndexEncoder =
  mconcat
    [ entityKey >$< idEncoder getReverseIndexId
    , entityVal >$< reverseIndexEncoder
    ]

reverseIndexEncoder :: E.Params ReverseIndex
reverseIndexEncoder =
  mconcat
    [ reverseIndexBlockId >$< idEncoder getBlockId
    , reverseIndexMinIds >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: txcbor
-- Description: Stores the raw CBOR (Concise Binary Object Representation) encoding of transactions, useful for validation
--   and serialization purposes.
-----------------------------------------------------------------------------------------------------------------------------------
data TxCbor = TxCbor
  { txCborTxId :: !TxId -- noreference
  , txCborBytes :: !ByteString -- sqltype=bytea
  }
  deriving (Show, Eq, Generic)

type instance Key TxCbor = TxCborId
instance DbInfo TxCbor

txCborEncoder :: E.Params TxCbor
txCborEncoder =
  mconcat
    [ txCborTxId >$< idEncoder getTxId
    , txCborBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: datum
-- Description: Contains the data associated with a transaction output, which can be used as input for a script.
-----------------------------------------------------------------------------------------------------------------------------------
data Datum = Datum
  { datumHash :: !ByteString -- sqltype=hash32type
  , datumTxId :: !TxId -- noreference
  , datumValue :: !(Maybe Text) -- sqltype=jsonb
  , datumBytes :: !ByteString -- sqltype=bytea
  }
  deriving (Eq, Show, Generic)

type instance Key Datum = DatumId
instance DbInfo Datum where
  uniqueFields _ = ["hash"]
  jsonbFields _ = ["value"]

datumEncoder :: E.Params Datum
datumEncoder =
  mconcat
    [ datumHash >$< E.param (E.nonNullable E.bytea)
    , datumTxId >$< idEncoder getTxId
    , datumValue >$< E.param (E.nullable E.text)
    , datumBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: script
-- Description: Contains the script associated with a transaction output, which can be used as input for a script.
-----------------------------------------------------------------------------------------------------------------------------------
data Script = Script
  { scriptTxId :: !TxId -- noreference
  , scriptHash :: !ByteString -- sqltype=hash28type
  , scriptType :: !ScriptType -- sqltype=scripttype
  , scriptJson :: !(Maybe Text) -- sqltype=jsonb
  , scriptBytes :: !(Maybe ByteString) -- sqltype=bytea
  , scriptSerialisedSize :: !(Maybe Word64) -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key Script = ScriptId

instance DbInfo Script where
  uniqueFields _ = ["hash"]
  jsonbFields _ = ["json"]
  enumFields _ = [("type", "scripttype")]

scriptEncoder :: E.Params Script
scriptEncoder =
  mconcat
    [ scriptTxId >$< idEncoder getTxId
    , scriptHash >$< E.param (E.nonNullable E.bytea)
    , scriptType >$< E.param (E.nonNullable scriptTypeEncoder)
    , scriptJson >$< E.param (E.nullable E.text)
    , scriptBytes >$< E.param (E.nullable E.bytea)
    , scriptSerialisedSize >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: redeemer
-- Description: Holds the redeemer data used to satisfy script conditions during transaction processing.
-----------------------------------------------------------------------------------------------------------------------------------

-- Unit step is in picosends, and `maxBound :: !Int64` picoseconds is over 100 days, so using
-- Word64/word63type is safe here. Similarly, `maxBound :: !Int64` if unit step would be an

-- * enormous* amount a memory which would cost a fortune.

data Redeemer = Redeemer
  { redeemerTxId :: !TxId -- noreference
  , redeemerUnitMem :: !Word64 -- sqltype=word63type
  , redeemerUnitSteps :: !Word64 -- sqltype=word63type
  , redeemerFee :: !(Maybe DbLovelace) -- sqltype=lovelace
  , redeemerPurpose :: !ScriptPurpose -- sqltype=scriptpurposetype
  , redeemerIndex :: !Word64 -- sqltype=word31type
  , redeemerScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  , redeemerRedeemerDataId :: !RedeemerDataId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key Redeemer = RedeemerId

instance DbInfo Redeemer where
  enumFields _ = [("purpose", "scriptpurposetype")]

redeemerEncoder :: E.Params Redeemer
redeemerEncoder =
  mconcat
    [ redeemerTxId >$< idEncoder getTxId
    , redeemerUnitMem >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerUnitSteps >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerFee >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , redeemerPurpose >$< E.param (E.nonNullable scriptPurposeEncoder)
    , redeemerIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerScriptHash >$< E.param (E.nullable E.bytea)
    , redeemerRedeemerDataId >$< idEncoder getRedeemerDataId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: redeemer_data
-- Description: Additional details about the redeemer, including its type and any associated metadata.
-----------------------------------------------------------------------------------------------------------------------------------
data RedeemerData = RedeemerData
  { redeemerDataHash :: !ByteString -- sqltype=hash32type
  , redeemerDataTxId :: !TxId -- noreference
  , redeemerDataValue :: !(Maybe Text) -- sqltype=jsonb
  , redeemerDataBytes :: !ByteString -- sqltype=bytea
  }
  deriving (Eq, Show, Generic)

type instance Key RedeemerData = RedeemerDataId
instance DbInfo RedeemerData where
  uniqueFields _ = ["hash"]
  jsonbFields _ = ["value"]

redeemerDataEncoder :: E.Params RedeemerData
redeemerDataEncoder =
  mconcat
    [ redeemerDataHash >$< E.param (E.nonNullable E.bytea)
    , redeemerDataTxId >$< idEncoder getTxId
    , redeemerDataValue >$< E.param (E.nullable E.text)
    , redeemerDataBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: extra_key_witness
-- Description: Contains additional key witnesses for transactions, which are used to validate the transaction's signature.
-----------------------------------------------------------------------------------------------------------------------------------
data ExtraKeyWitness = ExtraKeyWitness
  { extraKeyWitnessHash :: !ByteString -- sqltype=hash28type
  , extraKeyWitnessTxId :: !TxId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key ExtraKeyWitness = ExtraKeyWitnessId
instance DbInfo ExtraKeyWitness

extraKeyWitnessEncoder :: E.Params ExtraKeyWitness
extraKeyWitnessEncoder =
  mconcat
    [ extraKeyWitnessHash >$< E.param (E.nonNullable E.bytea)
    , extraKeyWitnessTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: slot_leader
-- Description:Contains information about the slot leader for a given block, including the slot leader's ID, hash, and description.
-----------------------------------------------------------------------------------------------------------------------------------

data SlotLeader = SlotLeader
  { slotLeaderHash :: !ByteString -- sqltype=hash28type
  , slotLeaderPoolHashId :: !(Maybe PoolHashId) -- This will be non-null when a block is mined by a pool
  , slotLeaderDescription :: !Text -- Description of the Slots leader
  }
  deriving (Eq, Show, Generic)

type instance Key SlotLeader = SlotLeaderId
instance DbInfo SlotLeader where
  uniqueFields _ = ["hash"]

slotLeaderEncoder :: E.Params SlotLeader
slotLeaderEncoder =
  mconcat
    [ slotLeaderHash >$< E.param (E.nonNullable E.bytea)
    , slotLeaderPoolHashId >$< Id.maybeIdEncoder Id.getPoolHashId
    , slotLeaderDescription >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- SYSTEM
-- These tables are used for database maintenance, versioning, and migrations.
-----------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: schema_version
-- Description: A table for schema versioning.
-----------------------------------------------------------------------------------------------------------------------------------
-- Schema versioning has three stages to best allow handling of schema migrations.
--    Stage 1: Set up PostgreSQL data types (using SQL 'DOMAIN' statements).
--    Stage 2: Persistent generated migrations.
--    Stage 3: Set up 'VIEW' tables (for use by other languages and applications).
-- This table should have a single row.
data SchemaVersion = SchemaVersion
  { schemaVersionStageOne :: !Int
  , schemaVersionStageTwo :: !Int
  , schemaVersionStageThree :: !Int
  }
  deriving (Eq, Show, Generic)

type instance Key SchemaVersion = SchemaVersionId
instance DbInfo SchemaVersion

schemaVersionDecoder :: D.Row SchemaVersion
schemaVersionDecoder =
  SchemaVersion
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageOne
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageTwo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageThree

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: meta
-- Description: A table containing metadata about the chain. There will probably only ever be one value in this table
-----------------------------------------------------------------------------------------------------------------------------------
data Meta = Meta
  { metaStartTime :: !UTCTime -- sqltype=timestamp
  , metaNetworkName :: !Text
  , metaVersion :: !Text
  }
  deriving (Show, Eq, Generic)

type instance Key Meta = MetaId
instance DbInfo Meta where
  uniqueFields _ = ["start_time"]

entityMetaDecoder :: D.Row (Entity Meta)
entityMetaDecoder =
  Entity
    <$> idDecoder MetaId
    <*> metaDecoder

metaDecoder :: D.Row Meta
metaDecoder =
  Meta
    <$> D.column (D.nonNullable utcTimeAsTimestampDecoder) -- metaStartTime
    <*> D.column (D.nonNullable D.text) -- metaNetworkName
    <*> D.column (D.nonNullable D.text) -- metaVersion

metaEncoder :: E.Params Meta
metaEncoder =
  mconcat
    [ metaStartTime >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
    , metaNetworkName >$< E.param (E.nonNullable E.text)
    , metaVersion >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: migration
-- Description: A table containing information about migrations.
-----------------------------------------------------------------------------------------------------------------------------------
data Withdrawal = Withdrawal
  { withdrawalAddrId :: !StakeAddressId
  , withdrawalAmount :: !DbLovelace
  , withdrawalRedeemerId :: !(Maybe RedeemerId)
  , withdrawalTxId :: !TxId
  }
  deriving (Eq, Show, Generic)

type instance Key Withdrawal = WithdrawalId
instance DbInfo Withdrawal

withdrawalDecoder :: D.Row Withdrawal
withdrawalDecoder =
  Withdrawal
    <$> idDecoder StakeAddressId -- withdrawalAddrId
    <*> dbLovelaceDecoder -- withdrawalAmount
    <*> maybeIdDecoder RedeemerId -- withdrawalRedeemerId
    <*> idDecoder TxId -- withdrawalTxId

withdrawalEncoder :: E.Params Withdrawal
withdrawalEncoder =
  mconcat
    [ withdrawalAddrId >$< idEncoder getStakeAddressId
    , withdrawalAmount >$< dbLovelaceEncoder
    , withdrawalRedeemerId >$< maybeIdEncoder getRedeemerId
    , withdrawalTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: extra_migrations
-- Description: = A table containing information about extra migrations.
-----------------------------------------------------------------------------------------------------------------------------------
data ExtraMigrations = ExtraMigrations
  { extraMigrationsToken :: !Text
  , extraMigrationsDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key ExtraMigrations = ExtraMigrationsId
instance DbInfo ExtraMigrations

extraMigrationsEncoder :: E.Params ExtraMigrations
extraMigrationsEncoder =
  mconcat
    [ extraMigrationsToken >$< E.param (E.nonNullable E.text)
    , extraMigrationsDescription >$< E.param (E.nullable E.text)
    ]
