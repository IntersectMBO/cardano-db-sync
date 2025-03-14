{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Statement.Function.Core (manyEncoder)
import Cardano.Db.Statement.Types (DbInfo(..), Key, Entity (..))
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
  maybeDbWord64Encoder
  )

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
  { blockHash :: !ByteString -- sqltype=hash32type
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

instance DbInfo Block where
  uniqueFields _ = ["hash"]

type instance Key Block = BlockId

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

entityBlockEncoder :: E.Params (Entity Block)
entityBlockEncoder =
  mconcat
    [ entityKey >$< idEncoder getBlockId
    , entityVal >$< blockEncoder
    ]

blockEncoder :: E.Params Block
blockEncoder =
  mconcat
    [ blockEpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
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
  } deriving (Show, Eq, Generic)

instance DbInfo Tx where
  uniqueFields _ = ["hash"]

type instance Key Tx = TxId

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

entityTxEncoder :: E.Params (Entity Tx)
entityTxEncoder =
  mconcat
    [ entityKey >$< idEncoder getTxId
    , entityVal >$< txEncoder
    ]

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
{-|
Table Name: txmetadata
Description: Contains metadata associated with transactions, such as metadata ID, key, and date.
-}
data TxMetadata = TxMetadata
  { txMetadataKey :: !DbWord64           -- sqltype=word64type
  , txMetadataJson :: !(Maybe Text)        -- sqltype=jsonb
  , txMetadataBytes :: !ByteString       -- sqltype=bytea
  , txMetadataTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo TxMetadata

type instance Key TxMetadata = TxMetadataId

entityTxMetadataDecoder :: D.Row (Entity TxMetadata)
entityTxMetadataDecoder =
  Entity
    <$> idDecoder TxMetadataId
    <*> txMetadataDecoder

txMetadataDecoder :: D.Row TxMetadata
txMetadataDecoder =
  TxMetadata
    <$> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- txMetadataKey
    <*> D.column (D.nullable D.text) -- txMetadataJson
    <*> D.column (D.nonNullable D.bytea) -- txMetadataBytes
    <*> idDecoder TxId -- txMetadataTxId

entityTxMetadataEncoder :: E.Params (Entity TxMetadata)
entityTxMetadataEncoder =
  mconcat
    [ entityKey >$< idEncoder getTxMetadataId
    , entityVal >$< txMetadataEncoder
    ]

txMetadataEncoder :: E.Params TxMetadata
txMetadataEncoder =
  mconcat
    [ txMetadataKey >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , txMetadataJson >$< E.param (E.nullable E.text)
    , txMetadataBytes >$< E.param (E.nonNullable E.bytea)
    , txMetadataTxId >$< idEncoder getTxId
    ]

txMetadataBulkEncoder :: E.Params ([DbWord64], [Maybe Text], [ByteString], [TxId])
txMetadataBulkEncoder =
  contrazip4
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nonNullable E.bytea)
    (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: txin
Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-}
data TxIn = TxIn
  { txInTxInId :: !TxId -- The transaction where this is used as an input.
  , txInTxOutId :: !TxId -- The transaction where this was created as an output.
  , txInTxOutIndex :: !Word64 -- sqltype=txindex
  , txInRedeemerId :: !(Maybe RedeemerId)
  } deriving (Show, Eq, Generic)

instance DbInfo TxIn

type instance Key TxIn = TxInId

entityTxInDecoder :: D.Row (Entity TxIn)
entityTxInDecoder =
  Entity
    <$> idDecoder TxInId
    <*> txInDecoder

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
encodeTxInBulk = contrazip4
  (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
  (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
  (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
  (manyEncoder $ E.nullable $ getRedeemerId >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: collateral_txin
Description:
-}
data CollateralTxIn = CollateralTxIn
  { collateralTxInTxInId :: !TxId         -- noreference -- The transaction where this is used as an input.
  , collateralTxInTxOutId :: !TxId        -- noreference -- The transaction where this was created as an output.
  , collateralTxInTxOutIndex :: !Word64   -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance DbInfo CollateralTxIn

type instance Key CollateralTxIn = CollateralTxInId

entityCollateralTxInDecoder :: D.Row (Entity CollateralTxIn)
entityCollateralTxInDecoder =
  Entity
    <$> idDecoder CollateralTxInId
    <*> collateralTxInDecoder

collateralTxInDecoder :: D.Row CollateralTxIn
collateralTxInDecoder =
  CollateralTxIn
    <$> idDecoder TxId -- collateralTxInTxInId
    <*> idDecoder TxId -- collateralTxInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxInTxOutIndex

entityCollateralTxInEncoder :: E.Params (Entity CollateralTxIn)
entityCollateralTxInEncoder =
  mconcat
    [ entityKey >$< idEncoder getCollateralTxInId
    , entityVal >$< collateralTxInEncoder
    ]

collateralTxInEncoder :: E.Params CollateralTxIn
collateralTxInEncoder =
  mconcat
    [ collateralTxInTxInId >$< idEncoder getTxId
    , collateralTxInTxOutId >$< idEncoder getTxId
    , collateralTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reference_txin
Description: Represents the input side of a transaction, linking to previous transaction outputs being spent
-}
data ReferenceTxIn = ReferenceTxIn
  { referenceTxInTxInId :: !TxId        -- noreference -- The transaction where this is used as an input.
  , referenceTxInTxOutId :: !TxId       -- noreference -- The transaction where this was created as an output.
  , referenceTxInTxOutIndex :: !Word64  -- sqltype=txindex
  } deriving (Show, Eq, Generic)

instance DbInfo ReferenceTxIn

type instance Key ReferenceTxIn = ReferenceTxInId

entityReferenceTxInDecoder :: D.Row (Entity ReferenceTxIn)
entityReferenceTxInDecoder =
  Entity
    <$> idDecoder ReferenceTxInId
    <*> referenceTxInDecoder

referenceTxInDecoder :: D.Row ReferenceTxIn
referenceTxInDecoder =
  ReferenceTxIn
    <$> idDecoder TxId -- referenceTxInTxInId
    <*> idDecoder TxId -- referenceTxInTxOutId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- referenceTxInTxOutIndex

entityReferenceTxInEncoder :: E.Params (Entity ReferenceTxIn)
entityReferenceTxInEncoder =
  mconcat
    [ entityKey >$< idEncoder getReferenceTxInId
    , entityVal >$< referenceTxInEncoder
    ]

referenceTxInEncoder :: E.Params ReferenceTxIn
referenceTxInEncoder =
  mconcat
    [ referenceTxInTxInId >$< idEncoder getTxId
    , referenceTxInTxOutId >$< idEncoder getTxId
    , referenceTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reverse_index
Description: Provides a reverse lookup mechanism for transaction inputs, allowing efficient querying of the origin of funds.
-}
data ReverseIndex = ReverseIndex
  { reverseIndexBlockId :: !BlockId   -- noreference
  , reverseIndexMinIds :: !Text
  } deriving (Show, Eq, Generic)

instance DbInfo ReverseIndex

type instance Key ReverseIndex = ReverseIndexId

entityReverseIndexDecoder :: D.Row (Entity ReverseIndex)
entityReverseIndexDecoder =
  Entity
    <$> idDecoder ReverseIndexId
    <*> reverseIndexDecoder

reverseIndexDecoder :: D.Row ReverseIndex
reverseIndexDecoder =
  ReverseIndex
    <$> idDecoder BlockId -- reverseIndexBlockId
    <*> D.column (D.nonNullable D.text) -- reverseIndexMinIds

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
{-|
Table Name: txcbor
Description: Stores the raw CBOR (Concise Binary Object Representation) encoding of transactions, useful for validation
  and serialization purposes.
-}
data TxCbor = TxCbor
  { txCborTxId :: !TxId           -- noreference
  , txCborBytes :: !ByteString    -- sqltype=bytea
  } deriving (Show, Eq, Generic)

instance DbInfo TxCbor

type instance Key TxCbor = TxCborId

entityTxCborDecoder :: D.Row (Entity TxCbor)
entityTxCborDecoder =
  Entity
    <$> idDecoder TxCborId
    <*> txCborDecoder

txCborDecoder :: D.Row TxCbor
txCborDecoder =
  TxCbor
    <$> idDecoder TxId -- txCborTxId
    <*> D.column (D.nonNullable D.bytea) -- txCborBytes

entityTxCborEncoder :: E.Params (Entity TxCbor)
entityTxCborEncoder =
  mconcat
    [ entityKey >$< idEncoder getTxCborId
    , entityVal >$< txCborEncoder
    ]

txCborEncoder :: E.Params TxCbor
txCborEncoder =
  mconcat
    [ txCborTxId >$< idEncoder getTxId
    , txCborBytes >$< E.param (E.nonNullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: datum
Description: Contains the data associated with a transaction output, which can be used as input for a script.
-}
data Datum = Datum
  { datumHash :: !ByteString      -- sqltype=hash32type
  , datumTxId :: !TxId            -- noreference
  , datumValue :: !(Maybe Text)     -- sqltype=jsonb
  , datumBytes :: !ByteString     -- sqltype=bytea
  } deriving (Eq, Show, Generic)

instance DbInfo Datum where
  uniqueFields _ = ["hash"]

type instance Key Datum = DatumId

entityDatumDecoder :: D.Row (Entity Datum)
entityDatumDecoder =
  Entity
    <$> idDecoder DatumId
    <*> datumDecoder

datumDecoder :: D.Row Datum
datumDecoder =
  Datum
    <$> D.column (D.nonNullable D.bytea) -- datumHash
    <*> idDecoder TxId -- datumTxId
    <*> D.column (D.nullable D.text) -- datumValue
    <*> D.column (D.nonNullable D.bytea) -- datumBytes

entityDatumEncoder :: E.Params (Entity Datum)
entityDatumEncoder =
  mconcat
    [ entityKey >$< idEncoder getDatumId
    , entityVal >$< datumEncoder
    ]

datumEncoder :: E.Params Datum
datumEncoder =
  mconcat
    [ datumHash >$< E.param (E.nonNullable E.bytea)
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
  { scriptTxId :: !TxId           -- noreference
  , scriptHash :: !ByteString     -- sqltype=hash28type
  , scriptType :: !ScriptType     -- sqltype=scripttype
  , scriptJson :: !(Maybe Text)     -- sqltype=jsonb
  , scriptBytes :: !(Maybe ByteString) -- sqltype=bytea
  , scriptSerialisedSize :: !(Maybe Word64) -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance DbInfo Script where
  uniqueFields _ = ["hash"]

type instance Key Script = ScriptId

entityScriptDecoder :: D.Row (Entity Script)
entityScriptDecoder =
  Entity
    <$> idDecoder ScriptId
    <*> scriptDecoder

scriptDecoder :: D.Row Script
scriptDecoder =
  Script
    <$> idDecoder TxId -- scriptTxId
    <*> D.column (D.nonNullable D.bytea) -- scriptHash
    <*> D.column (D.nonNullable scriptTypeDecoder) -- scriptType
    <*> D.column (D.nullable D.text) -- scriptJson
    <*> D.column (D.nullable D.bytea) -- scriptBytes
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- scriptSerialisedSize

entityScriptEncoder :: E.Params (Entity Script)
entityScriptEncoder =
  mconcat
    [ entityKey >$< idEncoder getScriptId
    , entityVal >$< scriptEncoder
    ]

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
{-|
Table Name: redeemer
Description: Holds the redeemer data used to satisfy script conditions during transaction processing.
-}
-- Unit step is in picosends, and `maxBound :: !Int64` picoseconds is over 100 days, so using
-- Word64/word63type is safe here. Similarly, `maxBound :: !Int64` if unit step would be an
-- *enormous* amount a memory which would cost a fortune.
data Redeemer = Redeemer
  { redeemerTxId :: !TxId                     -- noreference
  , redeemerUnitMem :: !Word64                -- sqltype=word63type
  , redeemerUnitSteps :: !Word64              -- sqltype=word63type
  , redeemerFee :: !(Maybe DbLovelace)          -- sqltype=lovelace
  , redeemerPurpose :: !ScriptPurpose         -- sqltype=scriptpurposetype
  , redeemerIndex :: !Word64                  -- sqltype=word31type
  , redeemerScriptHash :: !(Maybe ByteString)   -- sqltype=hash28type
  , redeemerRedeemerDataId :: !RedeemerDataId -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo Redeemer

type instance Key Redeemer = RedeemerId

entityRedeemerDecoder :: D.Row (Entity Redeemer)
entityRedeemerDecoder =
  Entity
    <$> idDecoder RedeemerId
    <*> redeemerDecoder

redeemerDecoder :: D.Row Redeemer
redeemerDecoder =
  Redeemer
    <$> idDecoder TxId -- redeemerTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerUnitMem
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerUnitSteps
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- redeemerFee
    <*> D.column (D.nonNullable scriptPurposeDecoder) -- redeemerPurpose
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- redeemerIndex
    <*> D.column (D.nullable D.bytea) -- redeemerScriptHash
    <*> idDecoder RedeemerDataId -- redeemerRedeemerDataId

entityRedeemerEncoder :: E.Params (Entity Redeemer)
entityRedeemerEncoder =
  mconcat
    [ entityKey >$< idEncoder getRedeemerId
    , entityVal >$< redeemerEncoder
    ]

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
{-|
Table Name: redeemer_data
Description: Additional details about the redeemer, including its type and any associated metadata.
-}
data RedeemerData = RedeemerData
  { redeemerDataHash :: !ByteString  -- sqltype=hash32type
  , redeemerDataTxId :: !TxId        -- noreference
  , redeemerDataValue :: !(Maybe Text) -- sqltype=jsonb
  , redeemerDataBytes :: !ByteString -- sqltype=bytea
  } deriving (Eq, Show, Generic)

instance DbInfo RedeemerData where
  uniqueFields _ = ["hash"]

type instance Key RedeemerData = RedeemerDataId

entityRedeemerDataDecoder :: D.Row (Entity RedeemerData)
entityRedeemerDataDecoder =
  Entity
    <$> idDecoder RedeemerDataId
    <*> redeemerDataDecoder

redeemerDataDecoder :: D.Row RedeemerData
redeemerDataDecoder =
  RedeemerData
    <$> D.column (D.nonNullable D.bytea) -- redeemerDataHash
    <*> idDecoder TxId -- redeemerDataTxId
    <*> D.column (D.nullable D.text) -- redeemerDataValue
    <*> D.column (D.nonNullable D.bytea) -- redeemerDataBytes

entityRedeemerDataEncoder :: E.Params (Entity RedeemerData)
entityRedeemerDataEncoder =
  mconcat
    [ entityKey >$< idEncoder getRedeemerDataId
    , entityVal >$< redeemerDataEncoder
    ]

redeemerDataEncoder :: E.Params RedeemerData
redeemerDataEncoder =
  mconcat
    [ redeemerDataHash >$< E.param (E.nonNullable E.bytea)
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
  { extraKeyWitnessHash :: !ByteString -- sqltype=hash28type
  , extraKeyWitnessTxId :: !TxId       -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo ExtraKeyWitness

type instance Key ExtraKeyWitness = ExtraKeyWitnessId

entityExtraKeyWitnessDecoder :: D.Row (Entity ExtraKeyWitness)
entityExtraKeyWitnessDecoder =
  Entity
    <$> idDecoder ExtraKeyWitnessId
    <*> extraKeyWitnessDecoder

extraKeyWitnessDecoder :: D.Row ExtraKeyWitness
extraKeyWitnessDecoder =
  ExtraKeyWitness
    <$> D.column (D.nonNullable D.bytea) -- extraKeyWitnessHash
    <*> idDecoder TxId -- extraKeyWitnessTxId

entityExtraKeyWitnessEncoder :: E.Params (Entity ExtraKeyWitness)
entityExtraKeyWitnessEncoder =
  mconcat
    [ entityKey >$< idEncoder getExtraKeyWitnessId
    , entityVal >$< extraKeyWitnessEncoder
    ]

extraKeyWitnessEncoder :: E.Params ExtraKeyWitness
extraKeyWitnessEncoder =
  mconcat
    [ extraKeyWitnessHash >$< E.param (E.nonNullable E.bytea)
    , extraKeyWitnessTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: slot_leader
Description:Contains information about the slot leader for a given block, including the slot leader's ID, hash, and description.
-}
data SlotLeader = SlotLeader
  { slotLeaderHash :: !ByteString -- sqltype=hash28type
  , slotLeaderPoolHashId :: !(Maybe Int)  -- This will be non-null when a block is mined by a pool
  , slotLeaderDescription :: !Text -- Description of the Slots leader
  } deriving (Eq, Show, Generic)

instance DbInfo SlotLeader where
  uniqueFields _ = ["hash"]

type instance Key SlotLeader = SlotLeaderId

entitySlotLeaderDecoder :: D.Row (Entity SlotLeader)
entitySlotLeaderDecoder =
  Entity
    <$> idDecoder SlotLeaderId
    <*> slotLeaderDecoder

slotLeaderDecoder :: D.Row SlotLeader
slotLeaderDecoder =
  SlotLeader
    <$> D.column (D.nonNullable D.bytea) -- slotLeaderHash
    <*> D.column (D.nullable $ fromIntegral <$> D.int4) -- slotLeaderPoolHashId
    <*> D.column (D.nonNullable D.text) -- slotLeaderDescription

entitySlotLeaderEncoder :: E.Params (Entity SlotLeader)
entitySlotLeaderEncoder =
  mconcat
    [ entityKey >$< idEncoder getSlotLeaderId
    , entityVal >$< slotLeaderEncoder
    ]

slotLeaderEncoder :: E.Params SlotLeader
slotLeaderEncoder =
  mconcat
    [ slotLeaderHash >$< E.param (E.nonNullable E.bytea)
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
  { schemaVersionStageOne :: !Int
  , schemaVersionStageTwo :: !Int
  , schemaVersionStageThree :: !Int
  } deriving (Eq, Show, Generic)

instance DbInfo SchemaVersion

type instance Key SchemaVersion = SchemaVersionId

entitySchemaVersionDecoder :: D.Row (Entity SchemaVersion)
entitySchemaVersionDecoder =
  Entity
    <$> idDecoder SchemaVersionId
    <*> schemaVersionDecoder

schemaVersionDecoder :: D.Row SchemaVersion
schemaVersionDecoder =
  SchemaVersion
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageOne
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageTwo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int4) -- schemaVersionStageThree

entitySchemaVersionEncoder :: E.Params (Entity SchemaVersion)
entitySchemaVersionEncoder =
  mconcat
    [ entityKey >$< idEncoder getSchemaVersionId
    , entityVal >$< schemaVersionEncoder
    ]

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
  { metaStartTime :: !UTCTime       -- sqltype=timestamp
  , metaNetworkName :: !Text
  , metaVersion :: !Text
  } deriving (Show, Eq, Generic)

instance DbInfo Meta where
  uniqueFields _ = ["start_time"]

type instance Key Meta = MetaId

entityMetaDecoder :: D.Row (Entity Meta)
entityMetaDecoder =
  Entity
    <$> idDecoder MetaId
    <*> metaDecoder

metaDecoder :: D.Row Meta
metaDecoder =
  Meta
    <$> D.column (D.nonNullable D.timestamptz) -- metaStartTime
    <*> D.column (D.nonNullable D.text) -- metaNetworkName
    <*> D.column (D.nonNullable D.text) -- metaVersion

entityMetaEncoder :: E.Params (Entity Meta)
entityMetaEncoder =
  mconcat
    [ entityKey >$< idEncoder getMetaId
    , entityVal >$< metaEncoder
    ]

metaEncoder :: E.Params Meta
metaEncoder =
  mconcat
    [ metaStartTime >$< E.param (E.nonNullable E.timestamptz)
    , metaNetworkName >$< E.param (E.nonNullable E.text)
    , metaVersion >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: migration
Description: A table containing information about migrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Withdrawal = Withdrawal
  { withdrawalAddrId :: !StakeAddressId
  , withdrawalAmount :: !DbLovelace
  , withdrawalRedeemerId :: !(Maybe RedeemerId)
  , withdrawalTxId :: !TxId
  } deriving (Eq, Show, Generic)

instance DbInfo Withdrawal

type instance Key Withdrawal = WithdrawalId

entityWithdrawalDecoder :: D.Row (Entity Withdrawal)
entityWithdrawalDecoder =
  Entity
    <$> idDecoder WithdrawalId
    <*> withdrawalDecoder

withdrawalDecoder :: D.Row Withdrawal
withdrawalDecoder =
  Withdrawal
    <$> idDecoder StakeAddressId -- withdrawalAddrId
    <*> dbLovelaceDecoder -- withdrawalAmount
    <*> maybeIdDecoder RedeemerId -- withdrawalRedeemerId
    <*> idDecoder TxId -- withdrawalTxId

entityWithdrawalEncoder :: E.Params (Entity Withdrawal)
entityWithdrawalEncoder =
  mconcat
    [ entityKey >$< idEncoder getWithdrawalId
    , entityVal >$< withdrawalEncoder
    ]

withdrawalEncoder :: E.Params Withdrawal
withdrawalEncoder =
  mconcat
    [ withdrawalAddrId >$< idEncoder getStakeAddressId
    , withdrawalAmount >$< dbLovelaceEncoder
    , withdrawalRedeemerId >$< maybeIdEncoder getRedeemerId
    , withdrawalTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: extra_migrations
Description: = A table containing information about extra migrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data ExtraMigrations = ExtraMigrations
  { extraMigrationsToken :: !Text
  , extraMigrationsDescription :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance DbInfo ExtraMigrations

type instance Key ExtraMigrations = ExtraMigrationsId

entityExtraMigrationsDecoder :: D.Row (Entity ExtraMigrations)
entityExtraMigrationsDecoder =
  Entity
    <$> idDecoder ExtraMigrationsId
    <*> extraMigrationsDecoder

extraMigrationsDecoder :: D.Row ExtraMigrations
extraMigrationsDecoder =
  ExtraMigrations
    <$> D.column (D.nonNullable D.text) -- extraMigrationsToken
    <*> D.column (D.nullable D.text) -- extraMigrationsDescription

entityExtraMigrationsEncoder :: E.Params (Entity ExtraMigrations)
entityExtraMigrationsEncoder =
  mconcat
    [ entityKey >$< idEncoder getExtraMigrationsId
    , entityVal >$< extraMigrationsEncoder
    ]

extraMigrationsEncoder :: E.Params ExtraMigrations
extraMigrationsEncoder =
  mconcat
    [ extraMigrationsToken >$< E.param (E.nonNullable E.text)
    , extraMigrationsDescription >$< E.param (E.nullable E.text)
    ]
