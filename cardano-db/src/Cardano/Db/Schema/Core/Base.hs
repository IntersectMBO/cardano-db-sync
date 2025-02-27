{-# LANGUAGE DeriveGeneric #-}

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
  { block_Id :: !BlockId
  , block_Hash :: !ByteString -- sqltype=hash32type
  , block_EpochNo :: !(Maybe Word64) -- sqltype=word31type
  , block_SlotNo :: !(Maybe Word64) -- sqltype=word63type
  , block_EpochSlotNo :: !(Maybe Word64) -- sqltype=word31type
  , block_BlockNo :: !(Maybe Word64) -- sqltype=word31type
  , block_PreviousId :: !(Maybe Int)  -- noreference
  , block_SlotLeaderId :: !SlotLeaderId  -- noreference
  , block_Size :: !Word64 -- sqltype=word31type
  , block_Time :: !UTCTime  -- sqltype=timestamp
  , block_TxCount :: !Word64
  , block_ProtoMajor :: !Word16 -- sqltype=word31type
  , block_ProtoMinor :: !Word16 -- sqltype=word31type
  -- Shelley specific
  , block_VrfKey :: !(Maybe Text)
  , block_OpCert :: !(Maybe ByteString) -- sqltype=hash32type
  , block_OpCertCounter :: !(Maybe Word64) -- sqltype=hash63type
  } deriving (Eq, Show, Generic)

instance HasDbInfo Block

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
    [ block_Hash >$< E.param (E.nonNullable E.bytea)
    , block_EpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , block_SlotNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , block_EpochSlotNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , block_BlockNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , block_PreviousId >$< E.param (E.nullable $ fromIntegral >$< E.int4)
    , block_SlotLeaderId >$< idEncoder getSlotLeaderId
    , block_Size >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , block_Time >$< E.param (E.nonNullable E.timestamptz)
    , block_TxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , block_ProtoMajor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , block_ProtoMinor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , block_VrfKey >$< E.param (E.nullable E.text)
    , block_OpCert >$< E.param (E.nullable E.bytea)
    , block_OpCertCounter >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx
Description: Contains data related to transactions, such as transaction ID, inputs, outputs, and meta_data
-}
data Tx = Tx
  { tx_Id :: !TxId
  , tx_Hash :: !ByteString -- sqltype=hash32type
  , tx_BlockId :: !BlockId -- noreference -- This type is the primary key for the 'block' table.
  , tx_BlockIndex :: !Word64 -- sqltype=word31type -- The index of this transaction within the block.
  , tx_OutSum :: !DbLovelace -- sqltype=lovelace
  , tx_Fee :: !DbLovelace -- sqltype=lovelace
  , tx_Deposit :: !(Maybe Int64) -- Needs to allow negaitve values.
  , tx_Size :: !Word64 -- sqltype=word31type
    -- New for Allega
  , tx_InvalidBefore :: !(Maybe DbWord64) -- sqltype=word64type
  , tx_InvalidHereafter :: !(Maybe DbWord64) -- sqltype=word64type
    -- New for Alonzo
  , tx_ValidContract :: !Bool -- False if the contract is invalid, True otherwise.
  , tx_ScriptSize :: !Word64 -- sqltype=word31type
    -- New for Conway
  , tx_TreasuryDonation :: !DbLovelace -- sqltype=lovelace default=0
  } deriving (Show, Eq, Generic)

instance HasDbInfo Tx

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
    [ tx_Id >$< idEncoder getTxId
    , tx_Hash >$< E.param (E.nonNullable E.bytea)
    , tx_BlockId >$< idEncoder getBlockId
    , tx_BlockIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , tx_OutSum >$< dbLovelaceEncoder
    , tx_Fee >$< dbLovelaceEncoder
    , tx_Deposit >$< E.param (E.nullable E.int8)
    , tx_Size >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , tx_InvalidBefore >$< maybeDbWord64Encoder
    , tx_InvalidHereafter >$< maybeDbWord64Encoder
    , tx_ValidContract >$< E.param (E.nonNullable E.bool)
    , tx_ScriptSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , tx_TreasuryDonation >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx_metadata
Description: Contains meta_data associated with transactions, such as meta_data ID, key, and date.
-}
data TxMetadata = TxMetadata
  { txMetadata_Id :: !TxMetadataId
  , txMetadata_Key :: !DbWord64           -- sqltype=word64type
  , txMetadata_Json :: !(Maybe Text)        -- sqltype=jsonb
  , txMetadata_Bytes :: !ByteString       -- sqltype=bytea
  , txMetadata_TxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo TxMetadata

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
    [ -- txMetadata_Id >$< idEncoder getTxMetadataId
      txMetadata_Key >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , txMetadata_Json >$< E.param (E.nullable E.text)
    , txMetadata_Bytes >$< E.param (E.nonNullable E.bytea)
    , txMetadata_TxId >$< idEncoder getTxId
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
  { txIn_Id :: !TxInId
  , txIn_TxInId :: !TxId -- The transaction where this is used as an input.
  , txIn_TxOutId :: !TxId -- The transaction where this was created as an output.
  , txIn_TxOutIndex :: !Word64 -- sqltype=txindex
  , txIn_RedeemerId :: !(Maybe RedeemerId)
  } deriving (Show, Eq, Generic)

instance HasDbInfo TxIn

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
      txIn_TxInId >$< idEncoder getTxId
    , txIn_TxOutId >$< idEncoder getTxId
    , txIn_TxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txIn_RedeemerId >$< maybeIdEncoder getRedeemerId
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
  { collateralTxIn_Id :: !CollateralTxInId -- noreference
  , collateralTxIn_TxInId :: !TxId         -- noreference -- The transaction where this is used as an input.
  , collateralTxIn_TxOutId :: !TxId        -- noreference -- The transaction where this was created as an output.
  , collateralTxIn_TxOutIndex :: !Word64   -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance HasDbInfo CollateralTxIn

collateralTxInDecoder :: D.Row CollateralTxIn
collateralTxInDecoder =
  CollateralTxIn
    <$> idDecoder CollateralTxInId -- collateralTxIn_Id
    <*> idDecoder TxId -- collateralTxIn_TxInId
    <*> idDecoder TxId -- collateralTxIn_TxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxIn_TxOutIndex

collateralTxInEncoder :: E.Params CollateralTxIn
collateralTxInEncoder =
  mconcat
    [ collateralTxIn_Id >$< idEncoder getCollateralTxInId
    , collateralTxIn_TxInId >$< idEncoder getTxId
    , collateralTxIn_TxOutId >$< idEncoder getTxId
    , collateralTxIn_TxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reference_tx_in
Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-}
data ReferenceTxIn = ReferenceTxIn
  { referenceTxIn_Id :: !ReferenceTxInId -- noreference
  , referenceTxIn_TxInId :: !TxId        -- noreference -- The transaction where this is used as an input.
  , referenceTxIn_TxOutId :: !TxId       -- noreference -- The transaction where this was created as an output.
  , referenceTxIn_TxOutIndex :: !Word64  -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance HasDbInfo ReferenceTxIn

referenceTxInDecoder :: D.Row ReferenceTxIn
referenceTxInDecoder =
  ReferenceTxIn
    <$> idDecoder ReferenceTxInId -- referenceTxIn_Id
    <*> idDecoder TxId -- referenceTxIn_TxInId
    <*> idDecoder TxId -- referenceTxIn_TxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- referenceTxIn_TxOutIndex

referenceTxInEncoder :: E.Params ReferenceTxIn
referenceTxInEncoder =
  mconcat
    [ referenceTxIn_Id >$< idEncoder getReferenceTxInId
    , referenceTxIn_TxInId >$< idEncoder getTxId
    , referenceTxIn_TxOutId >$< idEncoder getTxId
    , referenceTxIn_TxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reverse_index
Description: Provides a reverse lookup mechanism for transaction inputs, allowing efficient querying of the origin of funds.
-}
data ReverseIndex = ReverseIndex
  { reverseIndex_Id :: !ReverseIndexId -- noreference
  , reverseIndex_BlockId :: !BlockId   -- noreference
  , reverseIndex_MinIds :: !Text
  } deriving (Show, Eq, Generic)

instance HasDbInfo ReverseIndex

reverseIndexDecoder :: D.Row ReverseIndex
reverseIndexDecoder =
  ReverseIndex
    <$> idDecoder ReverseIndexId -- reverseIndex_Id
    <*> idDecoder BlockId -- reverseIndex_BlockId
    <*> D.column (D.nonNullable D.text) -- reverseIndex_MinIds

reverseIndexEncoder :: E.Params ReverseIndex
reverseIndexEncoder =
  mconcat
    [ reverseIndex_Id >$< idEncoder getReverseIndexId
    , reverseIndex_BlockId >$< idEncoder getBlockId
    , reverseIndex_MinIds >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: tx_cbor
Description: Stores the raw CBOR (Concise Binary Object Representation) encoding of transactions, useful for validation
  and serialization purposes.
-}
data TxCbor = TxCbor
  { txCbor_Id :: !TxCborId -- noreference
  , txCbor_TxId :: !TxId           -- noreference
  , txCbor_Bytes :: !ByteString    -- sqltype=bytea
  } deriving (Show, Eq, Generic)

instance HasDbInfo TxCbor

txCborDecoder :: D.Row TxCbor
txCborDecoder =
  TxCbor
    <$> idDecoder TxCborId -- txCbor_Id
    <*> idDecoder TxId -- txCbor_TxId
    <*> D.column (D.nonNullable D.bytea) -- txCbor_Bytes

txCborEncoder :: E.Params TxCbor
txCborEncoder =
  mconcat
    [ txCbor_Id >$< idEncoder getTxCborId
    , txCbor_TxId >$< idEncoder getTxId
    , txCbor_Bytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: datum_
Description: Contains the data associated with a transaction output, which can be used as input for a script_.
-}
data Datum = Datum
  { datum_Id :: !DatumId
  , datum_Hash :: !ByteString      -- sqltype=hash32type
  , datum_TxId :: !TxId            -- noreference
  , datum_Value :: !(Maybe Text)     -- sqltype=jsonb
  , datum_Bytes :: !ByteString     -- sqltype=bytea
  } deriving (Eq, Show, Generic)
-- UniqueDatum  hash

instance HasDbInfo Datum

datumDecoder :: D.Row Datum
datumDecoder =
  Datum
    <$> idDecoder DatumId -- datum_Id
    <*> D.column (D.nonNullable D.bytea) -- datum_Hash
    <*> idDecoder TxId -- datum_TxId
    <*> D.column (D.nullable D.text) -- datum_Value
    <*> D.column (D.nonNullable D.bytea) -- datum_Bytes

datumEncoder :: E.Params Datum
datumEncoder =
  mconcat
    [ datum_Id >$< idEncoder getDatumId
    , datum_Hash >$< E.param (E.nonNullable E.bytea)
    , datum_TxId >$< idEncoder getTxId
    , datum_Value >$< E.param (E.nullable E.text)
    , datum_Bytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: script
Description: Contains the script associated with a transaction output, which can be used as input for a script.
-}
data Script = Script
  { script_Id :: !ScriptId
  , script_TxId :: !TxId           -- noreference
  , script_Hash :: !ByteString     -- sqltype=hash28type
  , script_Type :: !ScriptType     -- sqltype=scripttype
  , script_Json :: !(Maybe Text)     -- sqltype=jsonb
  , script_Bytes :: !(Maybe ByteString) -- sqltype=bytea
  , script_SerialisedSize :: !(Maybe Word64) -- sqltype=word31type
  } deriving (Eq, Show, Generic)
-- UniqueScript  hash

instance HasDbInfo Script

scriptDecoder :: D.Row Script
scriptDecoder =
  Script
    <$> idDecoder ScriptId -- script_Id
    <*> idDecoder TxId -- script_TxId
    <*> D.column (D.nonNullable D.bytea) -- script_Hash
    <*> D.column (D.nonNullable scriptTypeDecoder) -- script_Type
    <*> D.column (D.nullable D.text) -- script_Json
    <*> D.column (D.nullable D.bytea) -- script_Bytes
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- script_SerialisedSize

scriptEncoder :: E.Params Script
scriptEncoder =
  mconcat
    [ script_Id >$< idEncoder getScriptId
    , script_TxId >$< idEncoder getTxId
    , script_Hash >$< E.param (E.nonNullable E.bytea)
    , script_Type >$< E.param (E.nonNullable scriptTypeEncoder)
    , script_Json >$< E.param (E.nullable E.text)
    , script_Bytes >$< E.param (E.nullable E.bytea)
    , script_SerialisedSize >$< E.param (E.nullable $ fromIntegral >$< E.int8)
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
  { redeemer_Id :: !RedeemerId
  , redeemer_TxId :: !TxId                     -- noreference
  , redeemer_UnitMem :: !Word64                -- sqltype=word63type
  , redeemer_UnitSteps :: !Word64              -- sqltype=word63type
  , redeemer_Fee :: !(Maybe DbLovelace)          -- sqltype=lovelace
  , redeemer_Purpose :: !ScriptPurpose         -- sqltype=scriptpurposetype
  , redeemer_Index :: !Word64                  -- sqltype=word31type
  , redeemer_ScriptHash :: !(Maybe ByteString)   -- sqltype=hash28type
  , redeemer_RedeemerDataId :: !RedeemerDataId -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo Redeemer

redeemerDecoder :: D.Row Redeemer
redeemerDecoder =
  Redeemer
    <$> idDecoder RedeemerId -- redeemer_Id
    <*> idDecoder TxId -- redeemer_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemer_UnitMem
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemer_UnitSteps
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- redeemer_Fee
    <*> D.column (D.nonNullable scriptPurposeDecoder) -- redeemer_Purpose
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemer_Index
    <*> D.column (D.nullable D.bytea) -- redeemer_ScriptHash
    <*> idDecoder RedeemerDataId -- redeemer_RedeemerDataId

redeemerEncoder :: E.Params Redeemer
redeemerEncoder =
  mconcat
    [ redeemer_Id >$< idEncoder getRedeemerId
    , redeemer_TxId >$< idEncoder getTxId
    , redeemer_UnitMem >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemer_UnitSteps >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemer_Fee >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , redeemer_Purpose >$< E.param (E.nonNullable scriptPurposeEncoder)
    , redeemer_Index >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , redeemer_ScriptHash >$< E.param (E.nullable E.bytea)
    , redeemer_RedeemerDataId >$< idEncoder getRedeemerDataId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: redeemer__data
Description: Additional details about the redeemer_, including its type and any associated meta_data.
-}
data RedeemerData = RedeemerData
  { redeemerData_Id :: !RedeemerDataId
  , redeemerData_Hash :: !ByteString  -- sqltype=hash32type
  , redeemerData_TxId :: !TxId        -- noreference
  , redeemerData_Value :: !(Maybe Text) -- sqltype=jsonb
  , redeemerData_Bytes :: !ByteString -- sqltype=bytea
  } deriving (Eq, Show, Generic)
-- UniqueRedeemerData  hash

instance HasDbInfo RedeemerData

redeemerDataDecoder :: D.Row RedeemerData
redeemerDataDecoder =
  RedeemerData
    <$> idDecoder RedeemerDataId -- redeemerData_Id
    <*> D.column (D.nonNullable D.bytea) -- redeemerData_Hash
    <*> idDecoder TxId -- redeemerData_TxId
    <*> D.column (D.nullable D.text) -- redeemerData_Value
    <*> D.column (D.nonNullable D.bytea) -- redeemerData_Bytes

redeemerDataEncoder :: E.Params RedeemerData
redeemerDataEncoder =
  mconcat
    [ redeemerData_Id >$< idEncoder getRedeemerDataId
    , redeemerData_Hash >$< E.param (E.nonNullable E.bytea)
    , redeemerData_TxId >$< idEncoder getTxId
    , redeemerData_Value >$< E.param (E.nullable E.text)
    , redeemerData_Bytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: extra_key_witness
Description: Contains additional key witnesses for transactions, which are used to validate the transaction's signature.
-}
data ExtraKeyWitness = ExtraKeyWitness
  { extraKeyWitness_Id :: !ExtraKeyWitnessId
  , extraKeyWitness_Hash :: !ByteString -- sqltype=hash28type
  , extraKeyWitness_TxId :: !TxId       -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo ExtraKeyWitness

extraKeyWitnessDecoder :: D.Row ExtraKeyWitness
extraKeyWitnessDecoder =
  ExtraKeyWitness
    <$> idDecoder ExtraKeyWitnessId -- extraKeyWitness_Id
    <*> D.column (D.nonNullable D.bytea) -- extraKeyWitness_Hash
    <*> idDecoder TxId -- extraKeyWitness_TxId

extraKeyWitnessEncoder :: E.Params ExtraKeyWitness
extraKeyWitnessEncoder =
  mconcat
    [ extraKeyWitness_Id >$< idEncoder getExtraKeyWitnessId
    , extraKeyWitness_Hash >$< E.param (E.nonNullable E.bytea)
    , extraKeyWitness_TxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: slot_leader
Description:Contains information about the slot leader for a given block, including the slot leader's ID, hash, and description.
-}
data SlotLeader = SlotLeader
  { slotLeader_Id :: !SlotLeaderId
  , slotLeader_Hash :: !ByteString -- sqltype=hash28type
  , slotLeader_PoolHashId :: !(Maybe Int)  -- This will be non-null when a block is mined by a pool
  , slotLeader_Description :: !Text -- Description of the Slots leader
  } deriving (Eq, Show, Generic)

instance HasDbInfo SlotLeader

slotLeaderDecoder :: D.Row SlotLeader
slotLeaderDecoder =
  SlotLeader
    <$> idDecoder SlotLeaderId -- slotLeader_Id
    <*> D.column (D.nonNullable D.bytea) -- slotLeader_Hash
    <*> D.column (D.nullable $ fromIntegral <$> D.int4) -- slotLeader_PoolHashId
    <*> D.column (D.nonNullable D.text) -- slotLeader_Description

slotLeaderEncoder :: E.Params SlotLeader
slotLeaderEncoder =
  mconcat
    [ slotLeader_Id >$< idEncoder getSlotLeaderId
    , slotLeader_Hash >$< E.param (E.nonNullable E.bytea)
    , slotLeader_PoolHashId >$< E.param (E.nullable $ fromIntegral >$< E.int4)
    , slotLeader_Description >$< E.param (E.nonNullable E.text)
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
  { schemaVersion_Id :: !SchemaVersionId -- noreference
  , schemaVersion_StageOne :: !Int
  , schemaVersion_StageTwo :: !Int
  , schemaVersion_StageThree :: !Int
  } deriving (Eq, Show, Generic)

instance HasDbInfo SchemaVersion

schemaVersionDecoder :: D.Row SchemaVersion
schemaVersionDecoder =
  SchemaVersion
    <$> idDecoder SchemaVersionId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersion_StageOne
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersion_StageTwo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersion_StageThree

schemaVersionEncoder :: E.Params SchemaVersion
schemaVersionEncoder =
  mconcat
    [ schemaVersion_StageOne >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    , schemaVersion_StageTwo >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    , schemaVersion_StageThree >$< E.param (E.nonNullable $ fromIntegral >$< E.int4)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: meta
Description: A table containing metadata about the chain. There will probably only ever be one value in this table
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Meta = Meta
  { meta_Id :: !MetaId -- noreference
  , meta_StartTime :: !UTCTime       -- sqltype=timestamp
  , meta_NetworkName :: !Text
  , meta_Version :: !Text
  } deriving (Show, Eq, Generic)

instance HasDbInfo Meta

metaDecoder :: D.Row Meta
metaDecoder =
  Meta
    <$> idDecoder MetaId -- meta_Id
    <*> D.column (D.nonNullable D.timestamptz) -- meta_StartTime
    <*> D.column (D.nonNullable D.text) -- meta_NetworkName
    <*> D.column (D.nonNullable D.text) -- meta_Version

metaEncoder :: E.Params Meta
metaEncoder =
  mconcat
    [ meta_Id >$< idEncoder getMetaId
    , meta_StartTime >$< E.param (E.nonNullable E.timestamptz)
    , meta_NetworkName >$< E.param (E.nonNullable E.text)
    , meta_Version >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: extra_migrations
Description: = A table containing information about extra migrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data ExtraMigrations = ExtraMigrations
  { extraMigrations_Id :: !ExtraMigrationsId
  , extraMigrations_Token :: !Text
  , extraMigrations_Description :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo ExtraMigrations

extraMigrationsDecoder :: D.Row ExtraMigrations
extraMigrationsDecoder =
  ExtraMigrations
    <$> idDecoder ExtraMigrationsId -- extraMigrations_Id
    <*> D.column (D.nonNullable D.text) -- extraMigrations_Token
    <*> D.column (D.nullable D.text) -- extraMigrations_Description

extraMigrationsEncoder :: E.Params ExtraMigrations
extraMigrationsEncoder =
  mconcat
    [ extraMigrations_Id >$< idEncoder getExtraMigrationsId
    , extraMigrations_Token >$< E.param (E.nonNullable E.text)
    , extraMigrations_Description >$< E.param (E.nullable E.text)
    ]
