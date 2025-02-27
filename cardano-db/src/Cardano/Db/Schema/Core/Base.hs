{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Cardano.Db.Schema.Core.Base where

import Cardano.Db.Schema.Orphans ()
import Hasql.Decoders as D
import Hasql.Encoders as E
import Cardano.Db.Schema.Ids
import Cardano.Db.Types (
  DbLovelace(..),
  DbWord64(..),
  ScriptPurpose,
  ScriptType,
  scriptPurposeDecoder,
  scriptPurposeEncoder,
  scriptTypeEncoder,
  scriptTypeDecoder,
  dbLovelaceDecoder,
  maybeDbWord64Decoder,
  dbLovelaceEncoder,
  maybeDbWord64Encoder, HasDbInfo (..),
 )
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16, Word64)
import Data.Functor.Contravariant
import GHC.Generics (Generic)
import Contravariant.Extras (contrazip4)
import Cardano.Db.Statement.Helpers (manyEncoder)
import qualified Data.List.NonEmpty as NE


-- We use camelCase here in the Haskell schema definition and 'persistLowerCase'
-- specifies that all the table and column names are converted to lower snake case.

-- All NULL-able fields other than 'epochNo' are NULL for EBBs, whereas 'epochNo' is
-- only NULL for the genesis block.

-----------------------------------------------------------------------------------------------------------------------------------
-- BASE TABLES
-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: block
Description: Stores information about individual blocks in the blockchain, including their hash, size,
  and the transactions they contain.
-}
data Block = Block
  { blockId :: !BlockId
  , blockHash :: !ByteString -- sqltype=hash32type
  , blockEpochNo :: !(Maybe Word64) -- sqltype=word31type
  , blockSlotNo :: !(Maybe Word64) -- sqltype=word63type
  , blockEpochSlotNo :: !(Maybe Word64) -- sqltype=word31type
  , blockBlockNo :: !(Maybe Word64) -- sqltype=word31type
  , blockPreviousId :: !(Maybe Int)  -- noreference
  , blockSlotLeaderId :: !SlotLeaderId  -- noreference
  , blockSize :: !Word64 -- sqltype=word31type
  , blockTime :: !UTCTime  -- sqltype=timestamp
  , blockTxCount :: !Word64
  , blockProtoMajor :: !Word16 -- sqltype=word31type
  , blockProtoMinor :: !Word16 -- sqltype=word31type
  -- Shelley specific
  , blockVrfKey :: !(Maybe Text)
  , blockOpCert :: !(Maybe ByteString) -- sqltype=hash32type
  , blockOpCertCounter :: !(Maybe Word64) -- sqltype=hash63type
  } deriving (Eq, Show, Generic)

instance HasDbInfo Block where
  tableName _ = "block"
  columnNames _ = NE.fromList
    [ "id", "hash", "epoch_no", "slot_no", "epoch_slot_no", "block_no", "previous_id"
    , "slot_leader_id", "size", "time", "tx_count", "proto_major", "proto_minor"
    , "vrf_key", "op_cert", "op_cert_counter"
    ]
  typeCasts _ = NE.fromList
    [ "int8[]", "bytea[]", "int4[]", "int8[]", "int4[]", "int4[]", "int8[]"
    , "int8[]", "int4[]", "timestampt[]", "int8[]", "int4[]", "int4[]"
    , "varchar[]", "bytea[]", "int8[]"
    ]

blockDecoder :: D.Row Block
blockDecoder =
  Block
    <$> idDecoder BlockId -- blockId
    <*> D.column (D.nonNullable D.bytea) -- blockHash
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockEpochNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockSlotNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockEpochSlotNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- blockBlockNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int4) -- blockPreviousId
    <*> idDecoder SlotLeaderId -- blockSlotLeaderId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- blockSize
    <*> D.column (D.nonNullable D.timestamptz) -- blockTime
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
    , blockPreviousId >$< E.param (E.nullable $ fromIntegral >$< E.int4)
    , blockSlotLeaderId >$< idEncoder getSlotLeaderId
    , blockSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , blockTime >$< E.param (E.nonNullable E.timestamptz)
    , blockTxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , blockProtoMajor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , blockProtoMinor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , blockVrfKey >$< E.param (E.nullable E.text)
    , blockOpCert >$< E.param (E.nullable E.bytea)
    , blockOpCertCounter >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx
Description: Contains data related to transactions, such as transaction ID, inputs, outputs, and metadata
-}
data Tx = Tx
  { txId :: !TxId
  , txHash :: !ByteString -- sqltype=hash32type
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
  } deriving (Show, Eq, Generic)

instance HasDbInfo Tx where
  tableName _ = "tx"
  columnNames _ = NE.fromList
    [ "id", "hash", "block_id", "block_index", "out_sum", "fee", "deposit", "size"
    , "invalid_before", "invalid_hereafter", "valid_contract", "script_size", "treasury_donation"
    ]
  typeCasts _ = NE.fromList
    [ "int8[]", "bytea[]", "int8[]", "int4[]", "numeric[]", "numeric[]", "int8[]", "int4[]"
    , "numeric[]", "numeric[]", "bool[]", "int4[]", "numeric[]"
    ]

txDecoder :: D.Row Tx
txDecoder =
  Tx
    <$> idDecoder TxId -- txId
    <*> D.column (D.nonNullable D.bytea) -- txHash
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
    [ txId >$< idEncoder getTxId
    , txHash >$< E.param (E.nonNullable E.bytea)
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
{-|
Table Name: tx_metadata
Description: Contains metadata associated with transactions, such as metadata ID, key, and date.
-}
data TxMetadata = TxMetadata
  { txMetadataId :: !TxMetadataId
  , txMetadataKey :: !DbWord64           -- sqltype=word64type
  , txMetadataJson :: !(Maybe Text)        -- sqltype=jsonb
  , txMetadataBytes :: !ByteString       -- sqltype=bytea
  , txMetadataTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo TxMetadata where
  tableName _ = "tx_metadata"
  columnNames _ = NE.fromList ["id", "key", "json", "bytes", "tx_id"]
  typeCasts _ = NE.fromList ["int8[]", "numeric[]", "jsonb[]", "bytea[]", "int8[]"]

txMetadataDecoder :: D.Row TxMetadata
txMetadataDecoder =
  TxMetadata
    <$> idDecoder TxMetadataId -- txMetadataId
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- txMetadataKey
    <*> D.column (D.nullable D.text) -- txMetadataJson
    <*> D.column (D.nonNullable D.bytea) -- txMetadataBytes
    <*> idDecoder TxId -- txMetadataTxId

txMetadataEncoder :: E.Params TxMetadata
txMetadataEncoder =
  mconcat
    [ -- txMetadataId >$< idEncoder getTxMetadataId
      txMetadataKey >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , txMetadataJson >$< E.param (E.nullable E.text)
    , txMetadataBytes >$< E.param (E.nonNullable E.bytea)
    , txMetadataTxId >$< idEncoder getTxId
    ]

txMetadataEncoderMany :: E.Params ([DbWord64], [Maybe Text], [ByteString], [TxId])
txMetadataEncoderMany =
  contrazip4
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nonNullable E.bytea)
    (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx_in
Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-}
data TxIn = TxIn
  { txInId :: !TxInId
  , txInTxInId :: !TxId -- The transaction where this is used as an input.
  , txInTxOutId :: !TxId -- The transaction where this was created as an output.
  , txInTxOutIndex :: !Word64 -- sqltype=txindex
  , txInRedeemerId :: !(Maybe RedeemerId)
  } deriving (Show, Eq, Generic)

instance HasDbInfo TxIn where
  tableName _ = "tx_in"
  columnNames _ = NE.fromList ["id", "tx_in_id", "tx_out_id", "tx_out_index", "redeemer_id"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "int8[]", "int2[]", "int8[]"]

txInDecoder :: D.Row TxIn
txInDecoder =
  TxIn
    <$> idDecoder TxInId -- txInId
    <*> idDecoder TxId -- txInTxInId
    <*> idDecoder TxId -- txInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txInTxOutIndex
    <*> maybeIdDecoder RedeemerId -- txInRedeemerId

txInEncoder :: E.Params TxIn
txInEncoder =
  mconcat
    [ -- txInId >$< idEncoder getTxInId
      txInTxInId >$< idEncoder getTxId
    , txInTxOutId >$< idEncoder getTxId
    , txInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txInRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

encodeTxInMany :: E.Params ([TxId], [TxId], [Word64], [Maybe RedeemerId])
encodeTxInMany = contrazip4
  (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
  (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
  (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
  (manyEncoder $ E.nullable $ getRedeemerId >$< E.int8)


-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: collateral_tx_in
Description:
-}
data CollateralTxIn = CollateralTxIn
  { collateralTxInId :: !CollateralTxInId -- noreference
  , collateralTxInTxInId :: !TxId         -- noreference -- The transaction where this is used as an input.
  , collateralTxInTxOutId :: !TxId        -- noreference -- The transaction where this was created as an output.
  , collateralTxInTxOutIndex :: !Word64   -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance HasDbInfo CollateralTxIn where
  tableName _ = "collateral_tx_in"
  columnNames _ = NE.fromList ["id", "tx_in_id", "tx_out_id", "tx_out_index"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "int8[]", "int2[]"]

collateralTxInDecoder :: D.Row CollateralTxIn
collateralTxInDecoder =
  CollateralTxIn
    <$> idDecoder CollateralTxInId -- collateralTxInId
    <*> idDecoder TxId -- collateralTxInTxInId
    <*> idDecoder TxId -- collateralTxInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxInTxOutIndex

collateralTxInEncoder :: E.Params CollateralTxIn
collateralTxInEncoder =
  mconcat
    [ collateralTxInId >$< idEncoder getCollateralTxInId
    , collateralTxInTxInId >$< idEncoder getTxId
    , collateralTxInTxOutId >$< idEncoder getTxId
    , collateralTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reference_tx_in
Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-}
data ReferenceTxIn = ReferenceTxIn
  { referenceTxInId :: !ReferenceTxInId -- noreference
  , referenceTxInTxInId :: !TxId        -- noreference -- The transaction where this is used as an input.
  , referenceTxInTxOutId :: !TxId       -- noreference -- The transaction where this was created as an output.
  , referenceTxInTxOutIndex :: !Word64  -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance HasDbInfo ReferenceTxIn where
  tableName _ = "reference_tx_in"
  columnNames _ = NE.fromList ["id", "tx_in_id", "tx_out_id", "tx_out_index"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "int8[]", "int2[]"]

referenceTxInDecoder :: D.Row ReferenceTxIn
referenceTxInDecoder =
  ReferenceTxIn
    <$> idDecoder ReferenceTxInId -- referenceTxInId
    <*> idDecoder TxId -- referenceTxInTxInId
    <*> idDecoder TxId -- referenceTxInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- referenceTxInTxOutIndex

referenceTxInEncoder :: E.Params ReferenceTxIn
referenceTxInEncoder =
  mconcat
    [ referenceTxInId >$< idEncoder getReferenceTxInId
    , referenceTxInTxInId >$< idEncoder getTxId
    , referenceTxInTxOutId >$< idEncoder getTxId
    , referenceTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reverse_index
Description: Provides a reverse lookup mechanism for transaction inputs, allowing efficient querying of the origin of funds.
-}
data ReverseIndex = ReverseIndex
  { reverseIndexId :: !ReverseIndexId -- noreference
  , reverseIndexBlockId :: !BlockId   -- noreference
  , reverseIndexMinIds :: !Text
  } deriving (Show, Eq, Generic)

instance HasDbInfo ReverseIndex where
  tableName _ = "reverse_index"
  columnNames _ = NE.fromList ["id", "block_id", "min_ids"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "varchar[]"]

reverseIndexDecoder :: D.Row ReverseIndex
reverseIndexDecoder =
  ReverseIndex
    <$> idDecoder ReverseIndexId -- reverseIndexId
    <*> idDecoder BlockId -- reverseIndexBlockId
    <*> D.column (D.nonNullable D.text) -- reverseIndexMinIds

reverseIndexEncoder :: E.Params ReverseIndex
reverseIndexEncoder =
  mconcat
    [ reverseIndexId >$< idEncoder getReverseIndexId
    , reverseIndexBlockId >$< idEncoder getBlockId
    , reverseIndexMinIds >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx_cbor
Description: Stores the raw CBOR (Concise Binary Object Representation) encoding of transactions, useful for validation
  and serialization purposes.
-}
data TxCbor = TxCbor
  { txCborId :: !TxCborId -- noreference
  , txCborTxId :: !TxId           -- noreference
  , txCborBytes :: !ByteString    -- sqltype=bytea
  } deriving (Show, Eq, Generic)

instance HasDbInfo TxCbor where
  tableName _ = "tx_cbor"
  columnNames _ = NE.fromList ["id", "tx_id", "bytes"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "bytea[]"]

txCborDecoder :: D.Row TxCbor
txCborDecoder =
  TxCbor
    <$> idDecoder TxCborId -- txCborId
    <*> idDecoder TxId -- txCborTxId
    <*> D.column (D.nonNullable D.bytea) -- txCborBytes

txCborEncoder :: E.Params TxCbor
txCborEncoder =
  mconcat
    [ txCborId >$< idEncoder getTxCborId
    , txCborTxId >$< idEncoder getTxId
    , txCborBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: datum
Description: Contains the data associated with a transaction output, which can be used as input for a script.
-}
data Datum = Datum
  { datumId :: !DatumId
  , datumHash :: !ByteString      -- sqltype=hash32type
  , datumTxId :: !TxId            -- noreference
  , datumValue :: !(Maybe Text)     -- sqltype=jsonb
  , datumBytes :: !ByteString     -- sqltype=bytea
  } deriving (Eq, Show, Generic)
-- UniqueDatum  hash

instance HasDbInfo Datum where
  tableName _ = "datum"
  columnNames _ = NE.fromList ["id", "hash", "tx_id", "value", "bytes"]
  typeCasts _ = NE.fromList ["int8[]", "bytea[]", "int8[]", "jsonb[]", "bytea[]"]

datumDecoder :: D.Row Datum
datumDecoder =
  Datum
    <$> idDecoder DatumId -- datumId
    <*> D.column (D.nonNullable D.bytea) -- datumHash
    <*> idDecoder TxId -- datumTxId
    <*> D.column (D.nullable D.text) -- datumValue
    <*> D.column (D.nonNullable D.bytea) -- datumBytes

datumEncoder :: E.Params Datum
datumEncoder =
  mconcat
    [ datumId >$< idEncoder getDatumId
    , datumHash >$< E.param (E.nonNullable E.bytea)
    , datumTxId >$< idEncoder getTxId
    , datumValue >$< E.param (E.nullable E.text)
    , datumBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: script
Description: Contains the script associated with a transaction output, which can be used as input for a script.
-}
data Script = Script
  { scriptId :: !ScriptId
  , scriptTxId :: !TxId           -- noreference
  , scriptHash :: !ByteString     -- sqltype=hash28type
  , scriptType :: !ScriptType     -- sqltype=scripttype
  , scriptJson :: !(Maybe Text)     -- sqltype=jsonb
  , scriptBytes :: !(Maybe ByteString) -- sqltype=bytea
  , scriptSerialisedSize :: !(Maybe Word64) -- sqltype=word31type
  } deriving (Eq, Show, Generic)
-- UniqueScript  hash

instance HasDbInfo Script where
  tableName _ = "script"
  columnNames _ = NE.fromList ["id", "tx_id", "hash", "type", "json", "bytes", "serialised_size"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "bytea[]", "scripttype[]", "jsonb[]", "bytea[]", "int4[]"]

scriptDecoder :: D.Row Script
scriptDecoder =
  Script
    <$> idDecoder ScriptId -- scriptId
    <*> idDecoder TxId -- scriptTxId
    <*> D.column (D.nonNullable D.bytea) -- scriptHash
    <*> D.column (D.nonNullable scriptTypeDecoder) -- scriptType
    <*> D.column (D.nullable D.text) -- scriptJson
    <*> D.column (D.nullable D.bytea) -- scriptBytes
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- scriptSerialisedSize

scriptEncoder :: E.Params Script
scriptEncoder =
  mconcat
    [ scriptId >$< idEncoder getScriptId
    , scriptTxId >$< idEncoder getTxId
    , scriptHash >$< E.param (E.nonNullable E.bytea)
    , scriptType >$< E.param (E.nonNullable scriptTypeEncoder)
    , scriptJson >$< E.param (E.nullable E.text)
    , scriptBytes >$< E.param (E.nullable E.bytea)
    , scriptSerialisedSize >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: redeemer
Description: Holds the redeemer data used to satisfy script conditions during transaction processing.
-}
-- Unit step is in picosends, and `maxBound :: !Int64` picoseconds is over 100 days, so using
-- Word64/word63type is safe here. Similarly, `maxBound :: !Int64` if unit step would be an
-- *enormous* amount a memory which would cost a fortune.
data Redeemer = Redeemer
  { redeemerId :: !RedeemerId
  , redeemerTxId :: !TxId                     -- noreference
  , redeemerUnitMem :: !Word64                -- sqltype=word63type
  , redeemerUnitSteps :: !Word64              -- sqltype=word63type
  , redeemerFee :: !(Maybe DbLovelace)          -- sqltype=lovelace
  , redeemerPurpose :: !ScriptPurpose         -- sqltype=scriptpurposetype
  , redeemerIndex :: !Word64                  -- sqltype=word31type
  , redeemerScriptHash :: !(Maybe ByteString)   -- sqltype=hash28type
  , redeemerRedeemerDataId :: !RedeemerDataId -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo Redeemer where
  tableName _ = "redeemer"
  columnNames _ = NE.fromList
    [ "id", "tx_id", "unit_mem", "unit_steps", "fee"
    , "purpose", "index", "script_hash", "redeemer_data_id"
    ]
  typeCasts _ = NE.fromList
    [ "int8[]", "int8[]", "int8[]", "int8[]", "numeric[]"
    , "scriptpurposetype[]", "int4[]", "bytea[]", "int8[]"
    ]

redeemerDecoder :: D.Row Redeemer
redeemerDecoder =
  Redeemer
    <$> idDecoder RedeemerId -- redeemerId
    <*> idDecoder TxId -- redeemerTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerUnitMem
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerUnitSteps
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- redeemerFee
    <*> D.column (D.nonNullable scriptPurposeDecoder) -- redeemerPurpose
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerIndex
    <*> D.column (D.nullable D.bytea) -- redeemerScriptHash
    <*> idDecoder RedeemerDataId -- redeemerRedeemerDataId

redeemerEncoder :: E.Params Redeemer
redeemerEncoder =
  mconcat
    [ redeemerId >$< idEncoder getRedeemerId
    , redeemerTxId >$< idEncoder getTxId
    , redeemerUnitMem >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerUnitSteps >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerFee >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , redeemerPurpose >$< E.param (E.nonNullable scriptPurposeEncoder)
    , redeemerIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemerScriptHash >$< E.param (E.nullable E.bytea)
    , redeemerRedeemerDataId >$< idEncoder getRedeemerDataId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: redeemer_data
Description: Additional details about the redeemer, including its type and any associated metadata.
-}
data RedeemerData = RedeemerData
  { redeemerDataId :: !RedeemerDataId
  , redeemerDataHash :: !ByteString  -- sqltype=hash32type
  , redeemerDataTxId :: !TxId        -- noreference
  , redeemerDataValue :: !(Maybe Text) -- sqltype=jsonb
  , redeemerDataBytes :: !ByteString -- sqltype=bytea
  } deriving (Eq, Show, Generic)
-- UniqueRedeemerData  hash

instance HasDbInfo RedeemerData where
  tableName _ = "redeemer_data"
  columnNames _ = NE.fromList ["id", "hash", "tx_id", "value", "bytes"]
  typeCasts _ = NE.fromList ["int8[]", "bytea[]", "int8[]", "jsonb[]", "bytea[]"]

redeemerDataDecoder :: D.Row RedeemerData
redeemerDataDecoder =
  RedeemerData
    <$> idDecoder RedeemerDataId -- redeemerDataId
    <*> D.column (D.nonNullable D.bytea) -- redeemerDataHash
    <*> idDecoder TxId -- redeemerDataTxId
    <*> D.column (D.nullable D.text) -- redeemerDataValue
    <*> D.column (D.nonNullable D.bytea) -- redeemerDataBytes

redeemerDataEncoder :: E.Params RedeemerData
redeemerDataEncoder =
  mconcat
    [ redeemerDataId >$< idEncoder getRedeemerDataId
    , redeemerDataHash >$< E.param (E.nonNullable E.bytea)
    , redeemerDataTxId >$< idEncoder getTxId
    , redeemerDataValue >$< E.param (E.nullable E.text)
    , redeemerDataBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: extra_key_witness
Description: Contains additional key witnesses for transactions, which are used to validate the transaction's signature.
-}
data ExtraKeyWitness = ExtraKeyWitness
  { extraKeyWitnessId :: !ExtraKeyWitnessId
  , extraKeyWitnessHash :: !ByteString -- sqltype=hash28type
  , extraKeyWitnessTxId :: !TxId       -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo ExtraKeyWitness where
  tableName _ = "extra_key_witness"
  columnNames _ = NE.fromList ["id", "hash", "tx_id"]
  typeCasts _ = NE.fromList ["int8[]", "bytea[]", "int8[]"]

extraKeyWitnessDecoder :: D.Row ExtraKeyWitness
extraKeyWitnessDecoder =
  ExtraKeyWitness
    <$> idDecoder ExtraKeyWitnessId -- extraKeyWitnessId
    <*> D.column (D.nonNullable D.bytea) -- extraKeyWitnessHash
    <*> idDecoder TxId -- extraKeyWitnessTxId

extraKeyWitnessEncoder :: E.Params ExtraKeyWitness
extraKeyWitnessEncoder =
  mconcat
    [ extraKeyWitnessId >$< idEncoder getExtraKeyWitnessId
    , extraKeyWitnessHash >$< E.param (E.nonNullable E.bytea)
    , extraKeyWitnessTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: slot_leader
Description:Contains information about the slot leader for a given block, including the slot leader's ID, hash, and description.
-}
data SlotLeader = SlotLeader
  { slotLeaderId :: !SlotLeaderId
  , slotLeaderHash :: !ByteString -- sqltype=hash28type
  , slotLeaderPoolHashId :: !(Maybe Int)  -- This will be non-null when a block is mined by a pool
  , slotLeaderDescription :: !Text -- Description of the Slots leader
  } deriving (Eq, Show, Generic)

instance HasDbInfo SlotLeader where
  tableName _ = "slot_leader"
  columnNames _ = NE.fromList ["id", "hash", "pool_hash_id", "description"]
  typeCasts _ = NE.fromList ["int8[]", "bytea[]", "int8[]", "varchar[]"]

slotLeaderDecoder :: D.Row SlotLeader
slotLeaderDecoder =
  SlotLeader
    <$> idDecoder SlotLeaderId -- slotLeaderId
    <*> D.column (D.nonNullable D.bytea) -- slotLeaderHash
    <*> D.column (D.nullable $ fromIntegral <$> D.int4) -- slotLeaderPoolHashId
    <*> D.column (D.nonNullable D.text) -- slotLeaderDescription

slotLeaderEncoder :: E.Params SlotLeader
slotLeaderEncoder =
  mconcat
    [ slotLeaderId >$< idEncoder getSlotLeaderId
    , slotLeaderHash >$< E.param (E.nonNullable E.bytea)
    , slotLeaderPoolHashId >$< E.param (E.nullable $ fromIntegral >$< E.int4)
    , slotLeaderDescription >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- SYSTEM
-- These tables are used for database maintenance, versioning, and migrations.
-----------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: schema_version
Description: A table for schema versioning.
-}
-----------------------------------------------------------------------------------------------------------------------------------
-- Schema versioning has three stages to best allow handling of schema migrations.
--    Stage 1: Set up PostgreSQL data types (using SQL 'DOMAIN' statements).
--    Stage 2: Persistent generated migrations.
--    Stage 3: Set up 'VIEW' tables (for use by other languages and applications).
-- This table should have a single row.
data SchemaVersion = SchemaVersion
  { schemaVersionId :: !SchemaVersionId -- noreference
  , schemaVersionStageOne :: !Int
  , schemaVersionStageTwo :: !Int
  , schemaVersionStageThree :: !Int
  } deriving (Eq, Show, Generic)

instance HasDbInfo SchemaVersion where
  tableName _ = "schema_version"
  columnNames _ = NE.fromList ["id", "stage_one", "stage_two", "stage_three"]
  typeCasts _ = NE.fromList ["int8[]", "int8[]", "int8[]", "int8[]"]

schemaVersionDecoder :: D.Row SchemaVersion
schemaVersionDecoder =
  SchemaVersion
    <$> idDecoder SchemaVersionId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageOne
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageTwo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageThree

schemaVersionEncoder :: E.Params SchemaVersion
schemaVersionEncoder =
  mconcat
    [ schemaVersionStageOne >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    , schemaVersionStageTwo >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    , schemaVersionStageThree >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: meta
Description: A table containing metadata about the chain. There will probably only ever be one value in this table
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Meta = Meta
  { metaId :: !MetaId -- noreference
  , metaStartTime :: !UTCTime       -- sqltype=timestamp
  , metaNetworkName :: !Text
  , metaVersion :: !Text
  } deriving (Show, Eq, Generic)

instance HasDbInfo Meta where
  tableName _ = "meta"
  columnNames _ = NE.fromList ["id", "start_time", "network_name", "version"]
  typeCasts _ = NE.fromList ["int8[]", "timestamp[]", "varchar[]", "varchar[]"]

metaDecoder :: D.Row Meta
metaDecoder =
  Meta
    <$> idDecoder MetaId -- metaId
    <*> D.column (D.nonNullable D.timestamptz) -- metaStartTime
    <*> D.column (D.nonNullable D.text) -- metaNetworkName
    <*> D.column (D.nonNullable D.text) -- metaVersion

metaEncoder :: E.Params Meta
metaEncoder =
  mconcat
    [ metaId >$< idEncoder getMetaId
    , metaStartTime >$< E.param (E.nonNullable E.timestamptz)
    , metaNetworkName >$< E.param (E.nonNullable E.text)
    , metaVersion >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: extra_migrations
Description: = A table containing information about extra migrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data ExtraMigrations = ExtraMigrations
  { extraMigrationsId :: !ExtraMigrationsId
  , extraMigrationsToken :: !Text
  , extraMigrationsDescription :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo ExtraMigrations where
  tableName _ = "extra_migrations"
  columnNames _ = NE.fromList ["id", "token", "description"]
  typeCasts _ = NE.fromList ["int8[]", "varchar[]", "varchar[]"]

extraMigrationsDecoder :: D.Row ExtraMigrations
extraMigrationsDecoder =
  ExtraMigrations
    <$> idDecoder ExtraMigrationsId -- extraMigrationsId
    <*> D.column (D.nonNullable D.text) -- extraMigrationsToken
    <*> D.column (D.nullable D.text) -- extraMigrationsDescription

extraMigrationsEncoder :: E.Params ExtraMigrations
extraMigrationsEncoder =
  mconcat
    [ extraMigrationsId >$< idEncoder getExtraMigrationsId
    , extraMigrationsToken >$< E.param (E.nonNullable E.text)
    , extraMigrationsDescription >$< E.param (E.nullable E.text)
    ]
