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

module Cardano.Db.Schema.BaseSchema where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Types (
  PoolUrl,
 )
import Cardano.Db.Types (
  AnchorType,
  DbInt65,
  DbLovelace,
  DbWord64,
  GovActionType,
  RewardSource,
  ScriptPurpose,
  ScriptType,
  SyncState,
  Vote,
  VoteUrl,
  VoterRole,
 )
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word16, Word64)
-- import Database.Persist.Class (Unique)
-- import Database.Persist.Documentation (deriveShowFields, document, (#), (--^))
-- import Database.Persist.EntityDef.Internal (EntityDef (..))
import GHC.Generics (Generic)

import Hasql.Decoders as Decode
import Hasql.Encoders as Encode

-- We use camelCase here in the Haskell schema definition and 'persistLowerCase'
-- specifies that all the table and column names are converted to lower snake case.

-- All NULL-able fields other than 'epochNo' are NULL for EBBs, whereas 'epochNo' is
-- only NULL for the genesis block.

-----------------------------------------------------------------------------------------------------------------------------------
-- CORE
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

newtype BlockId = BlockId { getBlockId :: Int64 }
  deriving (Eq, Show, Ord)

blockDecoder :: Decode.Row Block
blockDecoder =
  Block
    { blockId = BlockId <$> Decode.column Decode.int8
    , blockHash = Decode.column Decode.byteString
    , blockEpochNo = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , blockSlotNo = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , blockEpochSlotNo = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , blockBlockNo = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , blockPreviousId = Decode.column (Decode.nullable Decode.int4)
    , blockSlotLeaderId = SlotLeaderId <$> Decode.column Decode.int8
    , blockSize = fromIntegral <$> Decode.column Decode.int8
    , blockTime = Decode.column Decode.timestamptz
    , blockTxCount = fromIntegral <$> Decode.column Decode.int8
    , blockProtoMajor = fromIntegral <$> Decode.column Decode.int2
    , blockProtoMinor = fromIntegral <$> Decode.column Decode.int2
    , blockVrfKey = Decode.column (Decode.nullable Decode.text)
    , blockOpCert = Decode.column (Decode.nullable Decode.byteString)
    , blockOpCertCounter = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    }

blockEncoder :: Block -> Encode.Params
blockEncoder block =
 Encode.params $
      contramap blockHash Encode.byteString
   <> contramap (fmap fromIntegral . blockEpochNo) (Encode.nullable Encode.int8)
   <> contramap (fmap fromIntegral . blockSlotNo) (Encode.nullable Encode.int8)
   <> contramap (fmap fromIntegral . blockEpochSlotNo) (Encode.nullable Encode.int8)
   <> contramap (fmap fromIntegral . blockBlockNo) (Encode.nullable Encode.int8)
   <> contramap blockPreviousId (Encode.nullable Encode.int4)
   <> contramap (getSlotLeaderId . blockSlotLeaderId) Encode.int8
   <> contramap (fromIntegral . blockSize) Encode.int8
   <> contramap blockTime Encode.timestamptz
   <> contramap (fromIntegral . blockTxCount) Encode.int8
   <> contramap (fromIntegral . blockProtoMajor) Encode.int2
   <> contramap (fromIntegral . blockProtoMinor) Encode.int2
   <> contramap blockVrfKey (Encode.nullable Encode.text)
   <> contramap blockOpCert (Encode.nullable Encode.byteString)
   <> contramap (fmap fromIntegral . blockOpCertCounter) (Encode.nullable Encode.int8)

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

newtype TxId = TxId { getTxId :: Int64 }
  deriving (Eq, Show, Ord)

txDecoder :: Decode.Row Tx
txDecoder =
  Tx
    { txId = TxId <$> Decode.column Decode.int8
    , txHash = Decode.column Decode.byteString
    , txBlockId = BlockId <$> Decode.column Decode.int8
    , txBlockIndex = fromIntegral <$> Decode.column Decode.int8
    , txOutSum = DbLovelace <$> Decode.column Decode.int8
    , txFee = DbLovelace <$> Decode.column Decode.int8
    , txDeposit = Decode.column (Decode.nullable Decode.int8)
    , txSize = fromIntegral <$> Decode.column Decode.int8
    , txInvalidBefore = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , txInvalidHereafter = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , txValidContract = Decode.column Decode.bool
    , txScriptSize = fromIntegral <$> Decode.column Decode.int8
    , txTreasuryDonation = DbLovelace <$> Decode.column Decode.int8
    }

txEncoder :: Tx -> Encode.Params
txEncoder tx =
 Encode.params $
      contramap txHash Encode.byteString
   <> contramap (getBlockId . txBlockId) Encode.int8
   <> contramap (fromIntegral . txBlockIndex) Encode.int8
   <> contramap (unDbLovelace . txOutSum) Encode.int8
   <> contramap (unDbLovelace . txFee) Encode.int8
   <> contramap txDeposit (Encode.nullable Encode.int8)
   <> contramap (fromIntegral . txSize) Encode.int8
   <> contramap (fmap unDbWord64 . txInvalidBefore) (Encode.nullable Encode.int8)
   <> contramap (fmap unDbWord64 . txInvalidHereafter) (Encode.nullable Encode.int8)
   <> contramap txValidContract Encode.bool
   <> contramap (fromIntegral . txScriptSize) Encode.int8
   <> contramap (unDbLovelace . txTreasuryDonation) Encode.int8

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

newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int64 }
  deriving (Eq, Show, Ord)

txMetadataDecoder :: Decode.Row TxMetadata
txMetadataDecoder =
  TxMetadata
    { txMetadataId = TxMetadataId <$> Decode.column Decode.int8
    , txMetadataKey = DbWord64 <$> Decode.column Decode.int8
    , txMetadataJson = Decode.column (Decode.nullable Decode.text)
    , txMetadataBytes = Decode.column Decode.bytea
    , txMetadataTxId = TxId <$> Decode.column Decode.int8
    }

txMetadataEncoder :: TxMetadata -> Encode.Params
txMetadataEncoder txMetadata =
  Encode.params $ mconcat
    [ Encode.param (getTxMetadataId $ txMetadataId txMetadata)
    , Encode.param (getDbWord64 $ txMetadataKey txMetadata)
    , Encode.param (txMetadataJson txMetadata)
    , Encode.param (txMetadataBytes txMetadata)
    , Encode.param (getTxId $ txMetadataTxId txMetadata)
    ]

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

newtype TxInId = TxInId { getTxInId :: Int64 }
  deriving (Eq, Show, Ord)

txInDecoder :: Decode.Row TxIn
txInDecoder =
 TxIn
   { txInId = TxInId <$> Decode.column Decode.int8
  , txInTxInId = TxId <$> Decode.column Decode.int8
  , txInTxOutId = TxId <$> Decode.column Decode.int8
  , txInTxOutIndex = fromIntegral <$> Decode.column Decode.int8
  , txInRedeemerId = fmap RedeemerId <$> Decode.column (Decode.nullable Decode.int8)
   }

txInEncoder :: TxIn -> Encode.Params
txInEncoder txIn =
 Encode.params $
      contramap (getTxId . txInTxInId) Encode.int8 -- txInTxInId
   <> contramap (getTxId . txInTxOutId) Encode.int8 -- txInTxOutId
   <> contramap (fromIntegral . txInTxOutIndex) Encode.int8 -- txInTxOutIndex
   <> contramap (fmap getRedeemerId . txInRedeemerId) (Encode.nullable Encode.int8) -- txInRedeemerId

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

newtype CollateralTxInId = CollateralTxInId { getCollateralTxInId :: Int64 }
  deriving (Eq, Show, Ord)

collateralTxInDecoder :: Decode.Row CollateralTxIn
collateralTxInDecoder =
  CollateralTxIn
    { collateralTxInId = CollateralTxInId <$> Decode.column Decode.int8
    , collateralTxInTxInId = TxId <$> Decode.column Decode.int8
    , collateralTxInTxOutId = TxId <$> Decode.column Decode.int8
    , collateralTxInTxOutIndex = fromIntegral <$> Decode.column Decode.int8
    }

collateralTxInEncoder :: CollateralTxIn -> Encode.Params
collateralTxInEncoder collateralTxIn =
  Encode.params $ mconcat
    [ Encode.param $ getCollateralTxInId $ collateralTxInId collateralTxIn -- collateralTxInId
    , Encode.param $ getTxId $ collateralTxInTxInId collateralTxIn -- collateralTxInTxInId
    , Encode.param $ getTxId $ collateralTxInTxOutId collateralTxIn -- collateralTxInTxOutId
    , Encode.param $ fromIntegral $ collateralTxInTxOutIndex collateralTxIn -- collateralTxInTxOutIndex
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

newtype ReferenceTxInId = ReferenceTxInId { getReferenceTxInId :: Int64 }
  deriving (Eq, Show, Ord)

referenceTxInDecoder :: Decode.Row ReferenceTxIn
referenceTxInDecoder =
  ReferenceTxIn
   { referenceTxInId = ReferenceTxInId <$> Decode.column Decode.int8
    , referenceTxInTxInId = TxId <$> Decode.column Decode.int8
    , referenceTxInTxOutId = TxId <$> Decode.column Decode.int8

   }

referenceTxInEncoder :: ReferenceTxIn -> Encode.Params
referenceTxInEncoder referenceTxIn =
  Encode.params $
       contramap (getReferenceTxInId . referenceTxInId) Encode.int8 -- referenceTxInId
    <> contramap (getTxId . referenceTxInTxInId) Encode.int8 -- referenceTxInTxInId
    <> contramap (getTxId . referenceTxInTxOutId) Encode.int8 -- referenceTxInTxOutId
    <> contramap (fromIntegral . referenceTxInTxOutIndex) Encode.int8 -- referenceTxInTxOutIndex

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

newtype ReverseIndexId = ReverseIndexId { getReverseIndexId :: Int64 }
  deriving (Eq, Show, Ord)

reverseIndexDecoder :: Decode.Row ReverseIndex
reverseIndexDecoder =
  ReverseIndex
    { reverseIndexId = ReverseIndexId <$> Decode.column Decode.int8
    , reverseIndexBlockId = BlockId <$> Decode.column Decode.int8
    , reverseIndexMinIds = Decode.column Decode.text
    }

reverseIndexEncoder :: ReverseIndex -> Encode.Params
reverseIndexEncoder reverseIndex =
  Encode.params $
       contramap (getReverseIndexId . reverseIndexId) Encode.int8 -- reverseIndexId
    <> contramap (getBlockId . reverseIndexBlockId) Encode.int8 -- reverseIndexBlockId
    <> contramap reverseIndexMinIds Encode.text -- reverseIndexMinIds

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

newtype TxCborId = TxCborId { getTxCborId :: Int64 }
  deriving (Eq, Show, Ord)

txCborDecoder :: Decode.Row TxCbor
txCborDecoder =
  TxCbor
    {txCborId = TxCborId <$> Decode.column Decode.int8
    , txCborTxId = TxId <$> Decode.column Decode.int8
    , txCborBytes = Decode.column Decode.bytea
    , txCborBytes = Decode.column Decode.bytea
    }

txCborEncoder :: TxCbor -> Encode.Params
txCborEncoder txCbor =
  Encode.params $
       contramap (getTxCborId . txCborId) Encode.int8 --txCborId
    <> contramap (getTxId . txCborTxId) Encode.int8 -- txCborTxId
    <> contramap txCborBytes Encode.bytea -- txCborBytes

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

newtype DatumId = DatumId { getDatumId :: Int64 }
  deriving (Eq, Show, Ord)

datumDecoder :: Decode.Row Datum
datumDecoder =
  Datum
  { datumId = DatumId <$> Decode.column Decode.int8
  , datumHash = Decode.column Decode.bytea
  , datumTxId = TxId <$> Decode.column Decode.int8
  , datumValue = Decode.column (Decode.nullable Decode.text)
  , datumBytes = Decode.column Decode.bytea
  }

datumEncoder :: Datum -> Encode.Params
datumEncoder datum =
  Encode.params $
       contramap (getDatumId . datumId) Encode.int8 -- datumId
    <> contramap datumHash Encode.bytea -- datumHash
    <> contramap (getTxId . datumTxId) Encode.int8 -- datumTxId
    <> contramap datumValue (Encode.nullable Encode.text) -- datumValue
    <> contramap datumBytes Encode.bytea -- datumBytes

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

newtype ScriptId = ScriptId { getScriptId :: Int64 }
  deriving (Eq, Show, Ord)

scriptDecoder :: Decode.Row Script
scriptDecoder =
  Script
    { scriptId = ScriptId <$> Decode.column Decode.int8
    , scriptTxId = TxId <$> Decode.column Decode.int8
    , scriptHash = Decode.column Decode.bytea
    , scriptType = scriptTypeFromText <$> Decode.column Decode.text
    , scriptJson = Decode.column (Decode.nullable Decode.text)
    , scriptBytes = Decode.column (Decode.nullable Decode.bytea)
    , scriptSerialisedSize = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    }

scriptEncoder :: Script -> Encode.Params
scriptEncoder script =
  Encode.params $
       contramap (getScriptId . scriptId) Encode.int8 -- scriptId
    <> contramap (getTxId . scriptTxId) Encode.int8 -- scriptTxId
    <> contramap scriptHash Encode.bytea -- scriptHash
    <> contramap (scriptTypeToText . scriptType) Encode.text -- scriptType
    <> contramap scriptJson (Encode.nullable Encode.text) -- scriptJson
    <> contramap scriptBytes (Encode.nullable Encode.bytea) -- scriptBytes
    <> contramap (fmap fromIntegral . scriptSerialisedSize) (Encode.nullable Encode.int8) -- scriptSerialisedSize

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

newtype RedeemerId = RedeemerId { getRedeemerId :: Int64 }
  deriving (Eq, Show, Ord)

redeemerDecoder :: Decode.Row Redeemer
redeemerDecoder =
  Redeemer
    {redeemerId = RedeemerId <$> Decode.column Decode.int8
    , redeemerTxId = TxId <$> Decode.column Decode.int8
    , redeemerUnitMem = fromIntegral <$> Decode.column Decode.int8
    , redeemerUnitSteps = fromIntegral <$> Decode.column Decode.int8
    , redeemerFee = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , redeemerPurpose = scriptPurposeFromText <$> Decode.column Decode.text
    , redeemerIndex = fromIntegral <$> Decode.column Decode.int8
    , redeemerScriptHash = Decode.column (Decode.nullable Decode.bytea)
    , redeemerRedeemerDataId = RedeemerDataId <$> Decode.column Decode.int8
    }

redeemerEncoder :: Redeemer -> Encode.Params
redeemerEncoder redeemer =
  Encode.params $
       contramap (getRedeemerId . redeemerId) Encode.int8 -- redeemerId
    <> contramap (getTxId . redeemerTxId) Encode.int8 -- redeemerTxId
    <> contramap (fromIntegral . redeemerUnitMem) Encode.int8 -- redeemerUnitMem
    <> contramap (fromIntegral . redeemerUnitSteps) Encode.int8 -- redeemerUnitSteps
    <> contramap (fmap getDbLovelace . redeemerFee) (Encode.nullable Encode.int8) -- redeemerFee
    <> contramap (scriptPurposeFromText . redeemerPurpose) Encode.text -- redeemerPurpose
    <> contramap (fromIntegral . redeemerIndex) Encode.int8 -- redeemerIndex
    <> contramap redeemerScriptHash (Encode.nullable Encode.bytea) -- redeemerScriptHash
    <> contramap (getRedeemerDataId . redeemerRedeemerDataId) Encode.int8 -- redeemerRedeemerDataId

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

newtype RedeemerDataId = RedeemerDataId { getRedeemerDataId :: Int64 }
  deriving (Eq, Show, Ord)

redeemerDataDecoder :: Decode.Row RedeemerData
redeemerDataDecoder =
  RedeemerData
    { redeemerDataId = RedeemerDataId <$> Decode.column Decode.int8
    , redeemerDataHash = Decode.column Decode.bytea
    , redeemerDataTxId = TxId <$> Decode.column Decode.int8
    , redeemerDataValue = Decode.column (Decode.nullable Decode.text)
    , redeemerDataBytes = Decode.column Decode.bytea
    }

redeemerDataEncoder :: RedeemerData -> Encode.Params
redeemerDataEncoder redeemerData =
  Encode.params $
       contramap (getRedeemerDataId . redeemerDataId) Encode.int8 -- redeemerDataId
    <> contramap redeemerDataHash Encode.bytea -- redeemerDataHash
    <> contramap (getTxId . redeemerDataTxId) Encode.int8 -- redeemerDataTxId
    <> contramap redeemerDataValue (Encode.nullable Encode.text) -- redeemerDataValue
    <> contramap redeemerDataBytes Encode.bytea -- redeemerDataBytes

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

newtype ExtraKeyWitnessId = ExtraKeyWitnessId { getExtraKeyWitnessId :: Int64 }
  deriving (Eq, Show, Ord)

extraKeyWitnessDecoder :: Decode.Row ExtraKeyWitness
extraKeyWitnessDecoder =
  ExtraKeyWitness
    { extraKeyWitnessId = ExtraKeyWitnessId <$> Decode.column Decode.int8
    , extraKeyWitnessHash = Decode.column Decode.bytea
    , extraKeyWitnessTxId = TxId <$> Decode.column Decode.int8
    }

extraKeyWitnessEncoder :: ExtraKeyWitness -> Encode.Params
extraKeyWitnessEncoder extraKeyWitness =
  Encode.params $
       contramap (getExtraKeyWitnessId . extraKeyWitnessId) Encode.int8 -- extraKeyWitnessId
    <> contramap extraKeyWitnessHash Encode.bytea -- extraKeyWitnessHash
    <> contramap (getTxId . extraKeyWitnessTxId) Encode.int8 -- extraKeyWitnessTxId

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

newtype SlotLeaderId = SlotLeaderId { getSlotLeaderId :: Int64 }
  deriving (Eq, Show, Ord)

slotLeaderDecoder :: Decode.Row SlotLeader
slotLeaderDecoder =
  SlotLeader
    {
      slotLeaderId = SlotLeaderId <$> Decode.column Decode.int8
    , slotLeaderHash = Decode.column Decode.bytea
    , slotLeaderPoolHashId = Decode.column (Decode.nullable Decode.int4)
    , slotLeaderDescription = Decode.column Decode.tex
    }

slotLeaderEncoder :: SlotLeader -> Encode.Params
slotLeaderEncoder slotLeader =
  Encode.params $
       contramap (getSlotLeaderId . slotLeaderId) Encode.int8 -- slotLeaderId
    <> contramap slotLeaderHash Encode.bytea -- slotLeaderHash
    <> contramap slotLeaderPoolHashId (Encode.nullable Encode.int4) -- slotLeaderPoolHashId
    <> contramap slotLeaderDescription Encode.text -- slotLeaderDescription


-----------------------------------------------------------------------------------------------------------------------------------
-- EPOCH AND PROTOCOL PARAMETER
-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch
Description: The Epoch table is an aggregation of data in the 'Block' table, but is kept in this form
  because having it as a 'VIEW' is incredibly slow and inefficient.
  The 'outsum' type in the PostgreSQL world is 'bigint >= 0' so it will error out if an
  overflow (sum of tx outputs in an epoch) is detected. 'maxBound :: !Int` is big enough to
  hold 204 times the total Lovelace distribution. The chance of that much being transacted
  in a single epoch is relatively low.
-}
data Epoch = Epoch
  { epochId :: !EpochId
  , epochOutSum :: !Word128          -- sqltype=word128type
  , epochFees :: !DbLovelace         -- sqltype=lovelace
  , epochTxCount :: !Word64          -- sqltype=word31type
  , epochBlkCount :: !Word64         -- sqltype=word31type
  , epochNo :: !Word64               -- sqltype=word31type
  , epochStartTime :: !UTCTime       -- sqltype=timestamp
  , epochEndTime :: !UTCTime         -- sqltype=timestamp
  } deriving (Eq, Show, Generic)

newtype EpochId = EpochId { getEpochId :: Int64 }
  deriving (Eq, Show, Ord)

epochDecoder :: Decode.Row Epoch
epochDecoder =
  Epoch
    { epochId = EpochId <$> Decode.column Decode.int8
    , epochOutSum = fromIntegral <$> Decode.column Decode.int8
    , epochFees = DbLovelace <$> Decode.column Decode.int8
    , epochTxCount = fromIntegral <$> Decode.column Decode.int8
    , epochBlkCount = fromIntegral <$> Decode.column Decode.int8
    , epochNo = fromIntegral <$> Decode.column Decode.int8
    , epochStartTime = Decode.column Decode.timestamptz
    , epochEndTime = Decode.column Decode.timestamptz
    }

epochEncoder :: Epoch -> Encode.Params
epochEncoder epoch =
  Encode.params $
       contramap (getEpochId . epochId) Encode.int8 -- epochId
    <> contramap (fromIntegral . epochOutSum) Encode.int8 -- epochOutSum
    <> contramap (getDbLovelace . epochFees) Encode.int8 -- epochFees
    <> contramap (fromIntegral . epochTxCount) Encode.int8 -- epochTxCount
    <> contramap (fromIntegral . epochBlkCount) Encode.int8 -- epochBlkCount
    <> contramap (fromIntegral . epochNo) Encode.int8 -- epochNo
    <> contramap epochStartTime Encode.timestamptz -- epochStartTime
    <> contramap epochEndTime Encode.timestamptz -- epochEndTime

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_param
Description: Stores parameters relevant to each epoch, such as the number of slots per epoch or the block size limit.
-}
data EpochParam = EpochParam
  { epochParamId :: !EpochParamId
  , epochParamEpochNo :: !Word64               -- sqltype=word31type
  , epochParamMinFeeA :: !Word64               -- sqltype=word31type
  , epochParamMinFeeB :: !Word64               -- sqltype=word31type
  , epochParamMaxBlockSize :: !Word64          -- sqltype=word31type
  , epochParamMaxTxSize :: !Word64             -- sqltype=word31type
  , epochParamMaxBhSize :: !Word64             -- sqltype=word31type
  , epochParamKeyDeposit :: !DbLovelace        -- sqltype=lovelace
  , epochParamPoolDeposit :: !DbLovelace       -- sqltype=lovelace
  , epochParamMaxEpoch :: !Word64              -- sqltype=word31type
  , epochParamOptimalPoolCount :: !Word64      -- sqltype=word31type
  , epochParamInfluence :: !Double
  , epochParamMonetaryExpandRate :: !Double
  , epochParamTreasuryGrowthRate :: !Double
  , epochParamDecentralisation :: !Double
  , epochParamExtraEntropy :: !(Maybe ByteString) -- sqltype=hash32type
  , epochParamProtocolMajor :: !Word16         -- sqltype=word31type
  , epochParamProtocolMinor :: !Word16         -- sqltype=word31type
  , epochParamMinUtxoValue :: !DbLovelace      -- sqltype=lovelace
  , epochParamMinPoolCost :: !DbLovelace       -- sqltype=lovelace

  , epochParamNonce :: !(Maybe ByteString)       -- sqltype=hash32type

  , epochParamCoinsPerUtxoSize :: !(Maybe DbLovelace) -- sqltype=lovelace
  , epochParamCostModelId :: !(Maybe CostModelId)   -- noreference
  , epochParamPriceMem :: !(Maybe Double)
  , epochParamPriceStep :: !(Maybe Double)
  , epochParamMaxTxExMem :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParamMaxTxExSteps :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParamMaxBlockExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxBlockExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxValSize :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParamCollateralPercent :: !(Maybe Word16) -- sqltype=word31type
  , epochParamMaxCollateralInputs :: !(Maybe Word16) -- sqltype=word31type
  , epochParamPvtMotionNoConfidence :: !(Maybe Double)
  , epochParamPvtCommitteeNormal :: !(Maybe Double)
  , epochParamPvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParamPvtHardForkInitiation :: !(Maybe Double)
  , epochParamPvtppSecurityGroup :: !(Maybe Double)

  , epochParamDvtMotionNoConfidence :: !(Maybe Double)
  , epochParamDvtCommitteeNormal :: !(Maybe Double)
  , epochParamDvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParamDvtUpdateToConstitution :: !(Maybe Double)
  , epochParamDvtHardForkInitiation :: !(Maybe Double)
  , epochParamDvtPPNetworkGroup :: !(Maybe Double)
  , epochParamDvtPPEconomicGroup :: !(Maybe Double)
  , epochParamDvtPPTechnicalGroup :: !(Maybe Double)
  , epochParamDvtPPGovGroup :: !(Maybe Double)
  , epochParamDvtTreasuryWithdrawal :: !(Maybe Double)

  , epochParamCommitteeMinSize :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamCommitteeMaxTermLength :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamGovActionLifetime :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamGovActionDeposit :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParamDrepDeposit :: !(Maybe DbWord64)       -- sqltype=word64type
  , epochParamDrepActivity :: !(Maybe DbWord64)      -- sqltype=word64type
  , epochParamMinFeeRefScriptCostPerByte :: !(Maybe Double)
  , epochParamBlockId :: !BlockId                 -- noreference -- The first block where these parameters are valid.
  } deriving (Eq, Show, Generic)

newtype EpochParamId = EpochParamId { getEpochParamId :: Int64 }
  deriving (Eq, Show, Ord)

epochParamDecoder :: Decode.Row EpochParam
epochParamDecoder =
  EpochParam
    { epochParamId = EpochParamId <$> Decode.column Decode.int8
    , epochParamEpochNo = fromIntegral <$> Decode.column Decode.int8
    , epochParamMinFeeA = fromIntegral <$> Decode.column Decode.int8
    , epochParamMinFeeB = fromIntegral <$> Decode.column Decode.int8
    , epochParamMaxBlockSize = fromIntegral <$> Decode.column Decode.int8
    , epochParamMaxTxSize = fromIntegral <$> Decode.column Decode.int8
    , epochParamMaxBhSize = fromIntegral <$> Decode.column Decode.int8
    , epochParamKeyDeposit = DbLovelace <$> Decode.column Decode.int8
    , epochParamPoolDeposit = DbLovelace <$> Decode.column Decode.int8
    , epochParamMaxEpoch = fromIntegral <$> Decode.column Decode.int8
    , epochParamOptimalPoolCount = fromIntegral <$> Decode.column Decode.int8
    , epochParamInfluence = Decode.column Decode.float8
    , epochParamMonetaryExpandRate = Decode.column Decode.float8
    , epochParamTreasuryGrowthRate = Decode.column Decode.float8
    , epochParamDecentralisation = Decode.column Decode.float8
    , epochParamExtraEntropy = Decode.column (Decode.nullable Decode.bytea)
    , epochParamProtocolMajor = fromIntegral <$> Decode.column Decode.int2
    , epochParamProtocolMinor = fromIntegral <$> Decode.column Decode.int2
    , epochParamMinUtxoValue = DbLovelace <$> Decode.column Decode.int8
    , epochParamMinPoolCost = DbLovelace <$> Decode.column Decode.int8

    , epochParamNonce = Decode.column (Decode.nullable Decode.bytea)

    , epochParamCoinsPerUtxoSize = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamCostModelId = fmap CostModelId <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamPriceMem = Decode.column (Decode.nullable Decode.float8)
    , epochParamPriceStep = Decode.column (Decode.nullable Decode.float8)
    , epochParamMaxTxExMem = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamMaxTxExSteps = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamMaxBlockExMem = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamMaxBlockExSteps = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamMaxValSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamCollateralPercent = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , epochParamMaxCollateralInputs = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , epochParamPvtMotionNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , epochParamPvtCommitteeNormal = Decode.column (Decode.nullable Decode.float8)
    , epochParamPvtCommitteeNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , epochParamPvtHardForkInitiation = Decode.column (Decode.nullable Decode.float8)
    , epochParamPvtppSecurityGroup = Decode.column (Decode.nullable Decode.float8)

    , epochParamDvtMotionNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtCommitteeNormal = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtCommitteeNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtUpdateToConstitution = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtHardForkInitiation = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtPPNetworkGroup = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtPPEconomicGroup = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtPPTechnicalGroup = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtPPGovGroup = Decode.column (Decode.nullable Decode.float8)
    , epochParamDvtTreasuryWithdrawal = Decode.column (Decode.nullable Decode.float8)

    , epochParamCommitteeMinSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamCommitteeMaxTermLength = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamGovActionLifetime = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamGovActionDeposit = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamDrepDeposit = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamDrepActivity = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , epochParamMinFeeRefScriptCostPerByte = Decode.column Decode.float8
    , epochParamBlockId = BlockId <$> Decode.column Decode.int8
    }

epochParamEncoder :: EpochParam -> Encode.Params
epochParamEncoder epochParam =
  Encode.params $
       contramap (getEpochParamId . epochParamId) Encode.int8 -- epochParamId
    <> contramap (fromIntegral . epochParamEpochNo) Encode.int8 -- epochParamEpochNo
    <> contramap (fromIntegral . epochParamMinFeeA) Encode.int8 -- epochParamMinFeeA
    <> contramap (fromIntegral . epochParamMinFeeB) Encode.int8 -- epochParamMinFeeB
    <> contramap (fromIntegral . epochParamMaxBlockSize) Encode.int8 -- epochParamMaxBlockSize
    <> contramap (fromIntegral . epochParamMaxTxSize) Encode.int8 -- epochParamMaxTxSize
    <> contramap (fromIntegral . epochParamMaxBhSize) Encode.int8 -- epochParamMaxBhSize
    <> contramap (getDbLovelace . epochParamKeyDeposit) Encode.int8 -- epochParamKeyDeposit
    <> contramap (getDbLovelace . epochParamPoolDeposit) Encode.int8 -- epochParamPoolDeposit
    <> contramap (fromIntegral . epochParamMaxEpoch) Encode.int8 -- epochParamMaxEpoch
    <> contramap (fromIntegral . epochParamOptimalPoolCount) Encode.int8 -- epochParamOptimalPoolCount
    <> contramap epochParamInfluence Encode.float8 -- epochParamInfluence
    <> contramap epochParamMonetaryExpandRate Encode.float8 -- epochParamMonetaryExpandRate
    <> contramap epochParamTreasuryGrowthRate Encode.float8 -- epochParamTreasuryGrowthRate
    <> contramap epochParamDecentralisation Encode.float8 -- epochParamDecentralisation
    <> contramap epochParamExtraEntropy (Encode.nullable Encode.bytea) -- epochParamExtraEntropy
    <> contramap (fromIntegral . epochParamProtocolMajor) Encode.int2 -- epochParamProtocolMajor
    <> contramap (fromIntegral . epochParamProtocolMinor) Encode.int2 -- epochParamProtocolMinor
    <> contramap (getDbLovelace . epochParamMinUtxoValue) Encode.int8 -- epochParamMinUtxoValue
    <> contramap (getDbLovelace . epochParamMinPoolCost) Encode.int8 -- epochParamMinPoolCost
    <> contramap epochParamNonce (Encode.nullable Encode.bytea) -- epochParamNonce
    <> contramap (fmap getDbLovelace . epochParamCoinsPerUtxoSize) (Encode.nullable Encode.int8) -- epochParamCoinsPerUtxoSize
    <> contramap (fmap getCostModelId . epochParamCostModelId) (Encode.nullable Encode.int8) -- epochParamCostModelId
    <> contramap epochParamPriceMem (Encode.nullable Encode.float8) -- epochParamPriceMem
    <> contramap epochParamPriceStep (Encode.nullable Encode.float8) -- epochParamPriceStep
    <> contramap (fmap getDbWord64 . epochParamMaxTxExMem) (Encode.nullable Encode.int8) -- epochParamMaxTxExMem
    <> contramap (fmap getDbWord64 . epochParamMaxTxExSteps) (Encode.nullable Encode.int8) -- epochParamMaxTxExSteps
    <> contramap (fmap getDbWord64 . epochParamMaxBlockExMem) (Encode.nullable Encode.int8) -- epochParamMaxBlockExMem
    <> contramap (fmap getDbWord64 . epochParamMaxBlockExSteps) (Encode.nullable Encode.int8) -- epochParamMaxBlockExSteps
    <> contramap (fmap getDbWord64 . epochParamMaxValSize) (Encode.nullable Encode.int8) -- epochParamMaxValSize
    <> contramap (fmap fromIntegral . epochParamCollateralPercent) (Encode.nullable Encode.int2) -- epochParamCollateralPercent
    <> contramap (fmap fromIntegral . epochParamMaxCollateralInputs) (Encode.nullable Encode.int2) -- epochParamMaxCollateralInputs
    <> contramap epochParamPvtMotionNoConfidence (Encode.nullable Encode.float8) -- epochParamPvtMotionNoConfidence
    <> contramap epochParamPvtCommitteeNormal (Encode.nullable Encode.float8) -- epochParamPvtCommitteeNormal
    <> contramap epochParamPvtCommitteeNoConfidence (Encode.nullable Encode.float8) -- epochParamPvtCommitteeNoConfidence
    <> contramap epochParamPvtHardForkInitiation (Encode.nullable Encode.float8) -- epochParamPvtHardForkInitiation
    <> contramap epochParamPvtppSecurityGroup (Encode.nullable Encode.float8) -- epochParamPvtppSecurityGroup
    <> contramap epochParamDvtMotionNoConfidence (Encode.nullable Encode.float8) -- epochParamDvtMotionNoConfidence
    <> contramap epochParamDvtCommitteeNormal (Encode.nullable Encode.float8) -- epochParamDvtCommitteeNormal
    <> contramap epochParamDvtCommitteeNoConfidence (Encode.nullable Encode.float8) -- epochParamDvtCommitteeNoConfidence
    <> contramap epochParamDvtUpdateToConstitution (Encode.nullable Encode.float8) -- epochParamDvtUpdateToConstitution
    <> contramap epochParamDvtHardForkInitiation (Encode.nullable Encode.float8) -- epochParamDvtHardForkInitiation
    <> contramap epochParamDvtPPNetworkGroup (Encode.nullable Encode.float8) -- epochParamDvtPPNetworkGroup
    <> contramap epochParamDvtPPEconomicGroup (Encode.nullable Encode.float8) -- epochParamDvtPPEconomicGroup
    <> contramap epochParamDvtPPTechnicalGroup (Encode.nullable Encode.float8) -- epochParamDvtPPTechnicalGroup
    <> contramap epochParamDvtPPGovGroup (Encode.nullable Encode.float8) -- epochParamDvtPPGovGroup
    <> contramap epochParamDvtTreasuryWithdrawal (Encode.nullable Encode.float8) -- epochParamDvtTreasuryWithdrawal
    <> contramap (fmap getDbWord64 . epochParamCommitteeMinSize) (Encode.nullable Encode.int8) -- epochParamCommitteeMinSize
    <> contramap (fmap getDbWord64 . epochParamCommitteeMaxTermLength) (Encode.nullable Encode.int8) -- epochParamCommitteeMaxTermLength
    <> contramap (fmap getDbWord64 . epochParamGovActionLifetime) (Encode.nullable Encode.int8) -- epochParamGovActionLifetime
    <> contramap (fmap getDbWord64 . epochParamGovActionDeposit) (Encode.nullable Encode.int8) -- epochParamGovActionDeposit
    <> contramap (fmap getDbWord64 . epochParamDrepDeposit) (Encode.nullable Encode.int8) -- epochParamDrepDeposit
    <> contramap (fmap getDbWord64 . epochParamDrepActivity) (Encode.nullable Encode.int8) -- epochParamDrepActivity
    <> contramap epochParamMinFeeRefScriptCostPerByte (Encode.nullable Encode.float8) -- epochParamMinFeeRefScriptCostPerByte
    <> contramap (getBlockId . epochParamBlockId) Encode.int8 -- epochParamBlockId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_state
Description: Contains the state of the blockchain at the end of each epoch, including the committee, constitution, and no-confidence votes.
-}
data EpochState = EpochState
  { epochStateId :: !EpochStateId
  , epochStateCommitteeId :: !(Maybe CommitteeId)         -- noreference
  , epochStateNoConfidenceId :: !(Maybe GovActionProposalId) -- noreference
  , epochStateConstitutionId :: !(Maybe ConstitutionId)   -- noreference
  , epochStateEpochNo :: !Word64                        -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype EpochStateId = EpochStateId { getEpochStateId :: Int64 }
  deriving (Eq, Show, Ord)

epochStateDecoder :: Decode.Row EpochState
epochStateDecoder =
  EpochState
    { epochStateId = EpochStateId <$> Decode.column Decode.int8
    , epochStateCommitteeId = fmap CommitteeId <$> Decode.column (Decode.nullable Decode.int8)
    , epochStateNoConfidenceId = fmap GovActionProposalId <$> Decode.column (Decode.nullable Decode.int8)
    , epochStateConstitutionId = fmap ConstitutionId <$> Decode.column (Decode.nullable Decode.int8)
    , epochStateEpochNo = fromIntegral <$> Decode.column Decode.int8
    }

epochStateEncoder :: EpochState -> Encode.Params
epochStateEncoder epochState =
  Encode.params $
       contramap (getEpochStateId . epochStateId) Encode.int8 -- epochStateId
    <> contramap (fmap getCommitteeId . epochStateCommitteeId) (Encode.nullable Encode.int8) -- epochStateCommitteeId
    <> contramap (fmap getGovActionProposalId . epochStateNoConfidenceId) (Encode.nullable Encode.int8) -- epochStateNoConfidenceId
    <> contramap (fmap getConstitutionId . epochStateConstitutionId) (Encode.nullable Encode.int8) -- epochStateConstitutionId
    <> contramap (fromIntegral . epochStateEpochNo) Encode.int8 -- epochStateEpochNo

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_sync_time
Description: Tracks synchronization times for epochs, ensuring nodes are in consensus regarding the current state.
-}
data EpochSyncTime = EpochSyncTime
  { epochSyncTimeId :: !EpochSyncTimeId
  , epochSyncTimeNo :: !Word64         -- sqltype=word31type
  , epochSyncTimeSeconds :: !Word64    -- sqltype=word63type
  , epochSyncTimeState :: !SyncState   -- sqltype=syncstatetype
  } deriving (Show, Eq, Generic)
-- UniqueEpochSyncTime no

newtype EpochSyncTimeId = EpochSyncTimeId { getEpochSyncTimeId :: Int64 }
  deriving (Eq, Show, Ord)

epochSyncTimeDecoder :: Decode.Row EpochSyncTime
epochSyncTimeDecoder =
  EpochSyncTime
    { epochSyncTimeId = EpochSyncTimeId <$> Decode.column Decode.int8
    , epochSyncTimeNo = fromIntegral <$> Decode.column Decode.int8
    , epochSyncTimeSeconds = fromIntegral <$> Decode.column Decode.int8
    , epochSyncTimeState = Decode.column (Decode.enum syncStateFromText)
    }

epochSyncTimeEncoder :: EpochSyncTime -> Encode.Params
epochSyncTimeEncoder epochSyncTime =
  Encode.params $
       contramap (getEpochSyncTimeId . epochSyncTimeId) Encode.int8 -- epochSyncTimeId
    <> contramap (fromIntegral . epochSyncTimeNo) Encode.int8 -- epochSyncTimeNo
    <> contramap (fromIntegral . epochSyncTimeSeconds) Encode.int8 -- epochSyncTimeSeconds
    <> contramap (syncStateToText . epochSyncTimeState) Encode.text -- epochSyncTimeState

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: ada_pots
Description: A table with all the different types of total balances.
  This is only populated for the Shelley and later eras, and only on epoch boundaries.
  The treasury and rewards fields will be correct for the whole epoch, but all other
  fields change block by block.
-}
data AdaPots = AdaPots
  { adaPotsId :: !AdaPotsId
  , adaPotsSlotNo :: !Word64         -- sqltype=word63type
  , adaPotsEpochNo :: !Word64        -- sqltype=word31type
  , adaPotsTreasury :: !DbLovelace   -- sqltype=lovelace
  , adaPotsReserves :: !DbLovelace   -- sqltype=lovelace
  , adaPotsRewards :: !DbLovelace    -- sqltype=lovelace
  , adaPotsUtxo :: !DbLovelace       -- sqltype=lovelace
  , adaPotsDepositsStake :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsDrep :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsProposal :: !DbLovelace -- sqltype=lovelace
  , adaPotsFees :: !DbLovelace       -- sqltype=lovelace
  , adaPotsBlockId :: !BlockId       -- noreference
  } deriving (Eq)

newtype AdaPotsId = AdaPotsId { getAdaPotsId :: Int64 }
  deriving (Eq, Show, Ord)

adaPotsDecoder :: Decode.Row AdaPots
adaPotsDecoder =
  AdaPots
    { adaPotsId = AdaPotsId <$> Decode.column Decode.int8
    , adaPotsSlotNo = fromIntegral <$> Decode.column Decode.int8
    , adaPotsEpochNo = fromIntegral <$> Decode.column Decode.int8
    , adaPotsTreasury = DbLovelace <$> Decode.column Decode.int8
    , adaPotsReserves = DbLovelace <$> Decode.column Decode.int8
    , adaPotsRewards = DbLovelace <$> Decode.column Decode.int8
    , adaPotsUtxo = DbLovelace <$> Decode.column Decode.int8
    , adaPotsDepositsStake = DbLovelace <$> Decode.column Decode.int8
    , adaPotsDepositsDrep = DbLovelace <$> Decode.column Decode.int8
    , adaPotsDepositsProposal = DbLovelace <$> Decode.column Decode.int8
    , adaPotsFees = DbLovelace <$> Decode.column Decode.int8
    , adaPotsBlockId = BlockId <$> Decode.column Decode.int8
    }

adaPotsEncoder :: AdaPots -> Encode.Params
adaPotsEncoder adaPots =
  Encode.params $
       contramap (getAdaPotsId . adaPotsId) Encode.int8 -- adaPotsId
    <> contramap (fromIntegral . adaPotsSlotNo) Encode.int8 -- adaPotsSlotNo
    <> contramap (fromIntegral . adaPotsEpochNo) Encode.int8 -- adaPotsEpochNo
    <> contramap (getDbLovelace . adaPotsTreasury) Encode.int8 -- adaPotsTreasury
    <> contramap (getDbLovelace . adaPotsReserves) Encode.int8 -- adaPotsReserves
    <> contramap (getDbLovelace . adaPotsRewards) Encode.int8 -- adaPotsRewards
    <> contramap (getDbLovelace . adaPotsUtxo) Encode.int8 -- adaPotsUtxo
    <> contramap (getDbLovelace . adaPotsDepositsStake) Encode.int8 -- adaPotsDepositsStake
    <> contramap (getDbLovelace . adaPotsDepositsDrep) Encode.int8 -- adaPotsDepositsDrep
    <> contramap (getDbLovelace . adaPotsDepositsProposal) Encode.int8 -- adaPotsDepositsProposal
    <> contramap (getDbLovelace . adaPotsFees) Encode.int8 -- adaPotsFees
    <> contramap (getBlockId . adaPotsBlockId) Encode.int8 -- adaPotsBlockId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pot_transfer
Description: Records transfers between different pots (e.g., from the rewards pot to the treasury pot).
-}
data PotTransfer = PotTransfer
  { potTransferId :: !PotTransferId
  , potTransferCertIndex :: !Word16
  , potTransferTreasury :: !DbInt65    -- sqltype=int65type
  , potTransferReserves :: !DbInt65    -- sqltype=int65type
  , potTransferTxId :: !TxId           -- noreference
  } deriving (Show, Eq, Generic)

newtype PotTransferId = PotTransferId { getPotTransferId :: Int64 }
  deriving (Eq, Show, Ord)

potTransferDecoder :: Decode.Row PotTransfer
potTransferDecoder =
  PotTransfer
    { potTransferId = PotTransferId <$> Decode.column Decode.int8
    , potTransferCertIndex = fromIntegral <$> Decode.column Decode.int2
    , potTransferTreasury = DbInt65 <$> Decode.column Decode.int8
    , potTransferReserves = DbInt65 <$> Decode.column Decode.int8
    , potTransferTxId = TxId <$> Decode.column Decode.int8
    }

potTransferEncoder :: PotTransfer -> Encode.Params
potTransferEncoder potTransfer =
  Encode.params $
       contramap (getPotTransferId . potTransferId) Encode.int8 -- potTransferId
    <> contramap (fromIntegral . potTransferCertIndex) Encode.int2 -- potTransferCertIndex
    <> contramap (getDbInt65 . potTransferTreasury) Encode.int8 -- potTransferTreasury
    <> contramap (getDbInt65 . potTransferReserves) Encode.int8 -- potTransferReserves
    <> contramap (getTxId . potTransferTxId) Encode.int8 -- potTransferTxId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury
Description: Holds funds allocated to the treasury, which can be used for network upgrades or other community initiatives.
-}
data Treasury = Treasury
  { treasuryId :: !TreasuryId
  , treasuryAddrId :: !StakeAddressId -- noreference
  , treasuryCertIndex :: !Word16
  , treasuryAmount :: !DbInt65        -- sqltype=int65type
  , treasuryTxId :: !TxId             -- noreference
  } deriving (Show, Eq, Generic)

newtype TreasuryId = TreasuryId { getTreasuryId :: Int64 }
  deriving (Eq, Show, Ord)

treasuryDecoder :: Decode.Row Treasury
treasuryDecoder =
  Treasury
    { treasuryId = TreasuryId <$> Decode.column Decode.int8
    , treasuryAddrId = StakeAddressId <$> Decode.column Decode.int8
    , treasuryCertIndex = fromIntegral <$> Decode.column Decode.int2
    , treasuryAmount = DbInt65 <$> Decode.column Decode.int8
    , treasuryTxId = TxId <$> Decode.column Decode.int8
    }

treasuryEncoder :: Treasury -> Encode.Params
treasuryEncoder treasury =
  Encode.params $
       contramap (getTreasuryId . treasuryId) Encode.int8 -- treasuryId
    <> contramap (getStakeAddressId . treasuryAddrId) Encode.int8 -- treasuryAddrId
    <> contramap (fromIntegral . treasuryCertIndex) Encode.int2 -- treasuryCertIndex
    <> contramap (getDbInt65 . treasuryAmount) Encode.int8 -- treasuryAmount
    <> contramap (getTxId . treasuryTxId) Encode.int8 -- treasuryTxId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reserve
Description: Stores reserves set aside by the protocol to stabilize the cryptocurrency's value or fund future activities.
-}
data Reserve = Reserve
  { reserveId :: !ReserveId
  , reserveAddrId :: !StakeAddressId  -- noreference
  , reserveCertIndex :: !Word16
  , reserveAmount :: !DbInt65         -- sqltype=int65type
  , reserveTxId :: !TxId              -- noreference
  } deriving (Show, Eq, Generic)

newtype ReserveId = ReserveId { getReserveId :: Int64 }
  deriving (Eq, Show, Ord)

reserveDecoder :: Decode.Row Reserve
reserveDecoder =
  Reserve
    { reserveId = ReserveId <$> Decode.column Decode.int8
    , reserveAddrId = StakeAddressId <$> Decode.column Decode.int8
    , reserveCertIndex = fromIntegral <$> Decode.column Decode.int2
    , reserveAmount = DbInt65 <$> Decode.column Decode.int8
    , reserveTxId = TxId <$> Decode.column Decode.int8
    }

reserveEncoder :: Reserve -> Encode.Params
reserveEncoder reserve =
  Encode.params $
       contramap (getReserveId . reserveId) Encode.int8 -- reserveId
    <> contramap (getStakeAddressId . reserveAddrId) Encode.int8 -- reserveAddrId
    <> contramap (fromIntegral . reserveCertIndex) Encode.int2 -- reserveCertIndex
    <> contramap (getDbInt65 . reserveAmount) Encode.int8 -- reserveAmount
    <> contramap (getTxId . reserveTxId) Encode.int8 -- reserveTxId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: cost_model
Description: Defines the cost model used for estimating transaction fees, ensuring efficient resource allocation on the network.
-}
data CostModel = CostModel
  { costModelId :: !CostModelId
  , costModelHash :: !ByteString    -- sqltype=hash32type
  , costModelCosts :: !Text         -- sqltype=jsonb
  } deriving (Eq, Show, Generic)
-- uniqueCostModel  hash

newtype CostModelId = CostModelId { getCostModelId :: Int64 }
  deriving (Eq, Show, Ord)

costModelDecoder :: Decode.Row CostModel
costModelDecoder =
  CostModel
    { costModelId = CostModelId <$> Decode.column Decode.int8
    , costModelHash = Decode.column Decode.bytea
    , costModelCosts = Decode.column Decode.text
    }

costModelEncoder :: CostModel -> Encode.Params
costModelEncoder costModel =
  Encode.params $
       contramap (getCostModelId . costModelId) Encode.int8 -- costModelId
    <> contramap costModelHash Encode.bytea -- costModelHash
    <> contramap costModelCosts Encode.text -- costModelCosts


-----------------------------------------------------------------------------------------------------------------------------------
-- GOVERNANCE AND VOTING
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_hash
Description: Stores hashes of DRep (Decentralized Reputation) records, which are used in governance processes.
-}
data DrepHash = DrepHash
  { drepHashId :: !DrepHashId
  , drepHashRaw :: !(Maybe ByteString) -- sqltype=hash28type
  , drepHashView :: !Text
  , drepHashHasScript :: !Bool
  } deriving (Eq, Show, Generic)

newtype DrepHashId = DrepHashId { getDrepHashId :: Int64 }
  deriving (Eq, Show, Ord)

drepHashDecoder :: Decode.Row DrepHash
drepHashDecoder =
  DrepHash
    { drepHashId = DrepHashId <$> Decode.column Decode.int8
    , drepHashRaw = Decode.column (Decode.nullable Decode.bytea)
    , drepHashView = Decode.column Decode.text
    , drepHashHasScript = Decode.column Decode.bool
    }

drepHashEncoder :: DrepHash -> Encode.Params
drepHashEncoder drepHash =
  Encode.params $
       contramap (getDrepHashId . drepHashId) Encode.int8 -- drepHashId
    <> contramap drepHashRaw (Encode.nullable Encode.bytea) -- drepHashRaw
    <> contramap drepHashView Encode.text -- drepHashView
    <> contramap drepHashHasScript Encode.bool -- drepHashHasScript

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_registration
Description: Contains details about the registration of DReps, including their public keys and other identifying information.
-}
data DrepRegistration = DrepRegistration
  { drepRegistrationId :: !DrepRegistrationId
  , drepRegistrationTxId :: !TxId          -- noreference
  , drepRegistrationCertIndex :: !Word16
  , drepRegistrationDeposit :: !(Maybe Int64)
  , drepRegistrationVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , drepRegistrationDrepHashId :: !DrepHashId   -- noreference
  } deriving (Eq, Show, Generic)

newtype DrepRegistrationId = DrepRegistrationId { getDrepRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

drepRegistrationDecoder :: Decode.Row DrepRegistration
drepRegistrationDecoder =
  DrepRegistration
    { drepRegistrationId = DrepRegistrationId <$> Decode.column Decode.int8
    , drepRegistrationTxId = TxId <$> Decode.column Decode.int8
    , drepRegistrationCertIndex = fromIntegral <$> Decode.column Decode.int2
    , drepRegistrationDeposit = Decode.column (Decode.nullable Decode.int8)
    , drepRegistrationVotingAnchorId = fmap VotingAnchorId <$> Decode.column (Decode.nullable Decode.int8)
    , drepRegistrationDrepHashId = DrepHashId <$> Decode.column Decode.int8
    }

drepRegistrationEncoder :: DrepRegistration -> Encode.Params
drepRegistrationEncoder drepRegistration =
  Encode.params $
       contramap (getDrepRegistrationId . drepRegistrationId) Encode.int8 -- drepRegistrationId
    <> contramap (getTxId . drepRegistrationTxId) Encode.int8 -- drepRegistrationTxId
    <> contramap (fromIntegral . drepRegistrationCertIndex) Encode.int2 -- drepRegistrationCertIndex
    <> contramap drepRegistrationDeposit (Encode.nullable Encode.int8) -- drepRegistrationDeposit
    <> contramap (fmap getVotingAnchorId . drepRegistrationVotingAnchorId) (Encode.nullable Encode.int8) -- drepRegistrationVotingAnchorId
    <> contramap (getDrepHashId . drepRegistrationDrepHashId) Encode.int8 -- drepRegistrationDrepHashId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_distr
Description: Contains information about the distribution of DRep tokens, including the amount distributed and the epoch in which the distribution occurred.
-}
data DrepDistr = DrepDistr
  { drepDistrId :: !DrepDistrId
  , drepDistrHashId :: !DrepHashId         -- noreference
  , drepDistrAmount :: !Word64
  , drepDistrEpochNo :: !Word64            -- sqltype=word31type
  , drepDistrActiveUntil :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype DrepDistrId = DrepDistrId { getDrepDistrId :: Int64 }
  deriving (Eq, Show, Ord)

drepDistrDecoder :: Decode.Row DrepDistr
drepDistrDecoder =
  DrepDistr
    { drepDistrId = DrepDistrId <$> Decode.column Decode.int8
    , drepDistrHashId = DrepHashId <$> Decode.column Decode.int8
    , drepDistrAmount = fromIntegral <$> Decode.column Decode.int8
    , drepDistrEpochNo = fromIntegral <$> Decode.column Decode.int8
    , drepDistrActiveUntil = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    }

drepDistrEncoder :: DrepDistr -> Encode.Params
drepDistrEncoder drepDistr =
  Encode.params $
       contramap (getDrepDistrId . drepDistrId) Encode.int8 -- drepDistrId
    <> contramap (getDrepHashId . drepDistrHashId) Encode.int8 -- drepDistrHashId
    <> contramap (fromIntegral . drepDistrAmount) Encode.int8 -- drepDistrAmount
    <> contramap (fromIntegral . drepDistrEpochNo) Encode.int8 -- drepDistrEpochNo
    <> contramap (fmap fromIntegral . drepDistrActiveUntil) (Encode.nullable Encode.int8) -- drepDistrActiveUntil

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delegation_vote
Description: Tracks votes cast by stakeholders to delegate their voting rights to other entities within the governance framework.
-}
data DelegationVote = DelegationVote
  { delegationVoteId :: !DelegationVoteId
  , delegationVoteAddrId :: !StakeAddressId -- noreference
  , delegationVoteCertIndex :: !Word16
  , delegationVoteDrepHashId :: !DrepHashId -- noreference
  , delegationVoteTxId :: !TxId             -- noreference
  , delegationVoteRedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

newtype DelegationVoteId = DelegationVoteId { getDelegationVoteId :: Int64 }
  deriving (Eq, Show, Ord)

delegationVoteDecoder :: Decode.Row DelegationVote
delegationVoteDecoder =
  DelegationVote
    { delegationVoteId = DelegationVoteId <$> Decode.column Decode.int8
    , delegationVoteAddrId = StakeAddressId <$> Decode.column Decode.int8
    , delegationVoteCertIndex = fromIntegral <$> Decode.column Decode.int2
    , delegationVoteDrepHashId = DrepHashId <$> Decode.column Decode.int8
    , delegationVoteTxId = TxId <$> Decode.column Decode.int8
    , delegationVoteRedeemerId = fmap RedeemerId <$> Decode.column (Decode.nullable Decode.int8)
    }

delegationVoteEncoder :: DelegationVote -> Encode.Params
delegationVoteEncoder delegationVote =
  Encode.params $
       contramap (getDelegationVoteId . delegationVoteId) Encode.int8 -- delegationVoteId
    <> contramap (getStakeAddressId . delegationVoteAddrId) Encode.int8 -- delegationVoteAddrId
    <> contramap (fromIntegral . delegationVoteCertIndex) Encode.int2 -- delegationVoteCertIndex
    <> contramap (getDrepHashId . delegationVoteDrepHashId) Encode.int8 -- delegationVoteDrepHashId
    <> contramap (getTxId . delegationVoteTxId) Encode.int8 -- delegationVoteTxId
    <> contramap (fmap getRedeemerId . delegationVoteRedeemerId) (Encode.nullable Encode.int8) -- delegationVoteRedeemerId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: gov_action_proposal
Description: Contains proposals for governance actions, including the type of action, the amount of the deposit, and the expiration date.
-}
data GovActionProposal = GovActionProposal
  { govActionProposalId :: !GovActionProposalId
  , govActionProposalTxId :: !TxId           -- noreference
  , govActionProposalIndex :: !Word64
  , govActionProposalPrevGovActionProposal :: !(Maybe GovActionProposalId) -- noreference
  , govActionProposalDeposit :: !DbLovelace  -- sqltype=lovelace
  , govActionProposalReturnAddress :: !StakeAddressId -- noreference
  , govActionProposalExpiration :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , govActionProposalType :: !GovActionType   -- sqltype=govactiontype
  , govActionProposalDescription :: !Text     -- sqltype=jsonb
  , govActionProposalParamProposal :: !(Maybe ParamProposalId) -- noreference
  , govActionProposalRatifiedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalEnactedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalDroppedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalExpiredEpoch :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype GovActionProposalId = GovActionProposalId { getGovActionProposalId :: Int64 }
  deriving (Eq, Show, Ord)

govActionProposalDecoder :: Decode.Row GovActionProposal
govActionProposalDecoder =
  GovActionProposal
    { govActionProposalId = GovActionProposalId <$> Decode.column Decode.int8
    , govActionProposalTxId = TxId <$> Decode.column Decode.int8
    , govActionProposalIndex = fromIntegral <$> Decode.column Decode.int8
    , govActionProposalPrevGovActionProposal = fmap GovActionProposalId <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalDeposit = DbLovelace <$> Decode.column Decode.int8
    , govActionProposalReturnAddress = StakeAddressId <$> Decode.column Decode.int8
    , govActionProposalExpiration = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalVotingAnchorId = fmap VotingAnchorId <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalType = Decode.column (Decode.enum govActionTypeFromText)
    , govActionProposalDescription = Decode.column Decode.text
    , govActionProposalParamProposal = fmap ParamProposalId <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalRatifiedEpoch = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalEnactedEpoch = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalDroppedEpoch = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , govActionProposalExpiredEpoch = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    }

govActionProposalEncoder :: GovActionProposal -> Encode.Params
govActionProposalEncoder govActionProposal =
  Encode.params $ mconcat
    [ Encode.param $ getGovActionProposalId $ govActionProposalId govActionProposal -- govActionProposalId
    , Encode.param $ getTxId $ govActionProposalTxId govActionProposal -- govActionProposalTxId
    , Encode.param $ fromIntegral $ govActionProposalIndex govActionProposal -- govActionProposalIndex
    , Encode.param (getGovActionProposalId <$> govActionProposalPrevGovActionProposal govActionProposal) -- govActionProposalPrevGovActionProposal
    , Encode.param $ getDbLovelace $ govActionProposalDeposit govActionProposal -- govActionProposalDeposit
    , Encode.param $ getStakeAddressId $ govActionProposalReturnAddress govActionProposal -- govActionProposalReturnAddress
    , Encode.param (fromIntegral <$> govActionProposalExpiration govActionProposal) -- govActionProposalExpiration
    , Encode.param (getVotingAnchorId <$> govActionProposalVotingAnchorId govActionProposal) -- govActionProposalVotingAnchorId
    , Encode.param $ govActionTypeToText $ govActionProposalType govActionProposal -- govActionProposalType
    , Encode.param $ govActionProposalDescription govActionProposal -- govActionProposalDescription
    , Encode.param (getParamProposalId <$> govActionProposalParamProposal govActionProposal) -- govActionProposalParamProposal
    , Encode.param (fromIntegral <$> govActionProposalRatifiedEpoch govActionProposal) -- govActionProposalRatifiedEpoch
    , Encode.param (fromIntegral <$> govActionProposalEnactedEpoch govActionProposal) -- govActionProposalEnactedEpoch
    , Encode.param (fromIntegral <$> govActionProposalDroppedEpoch govActionProposal) -- govActionProposalDroppedEpoch
    , Encode.param (fromIntegral <$> govActionProposalExpiredEpoch govActionProposal) -- govActionProposalExpiredEpoch
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_procedure
Description: Defines the procedures and rules governing the voting process, including quorum requirements and tallying mechanisms.
-}
data VotingProcedure = VotingProcedure
  { votingProcedureId :: !VotingProcedureId
  , votingProcedureTxId :: !TxId                    -- noreference
  , votingProcedureIndex :: !Word16
  , votingProcedureGovActionProposalId :: !GovActionProposalId -- noreference
  , votingProcedureVoterRole :: !VoterRole           -- sqltype=voterrole
  , votingProcedureCommitteeVoter :: !(Maybe CommitteeHashId) -- noreference
  , votingProcedureDrepVoter :: !(Maybe DrepHashId)    -- noreference
  , votingProcedurePoolVoter :: !(Maybe PoolHashId)    -- noreference
  , votingProcedureVote :: !Vote                     -- sqltype=vote
  , votingProcedureVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , votingProcedureInvalid :: !(Maybe EventInfoId)    -- noreference
  } deriving (Eq, Show, Generic)

newtype VotingProcedureId = VotingProcedureId { getVotingProcedureId :: Int64 }
  deriving (Eq, Show, Ord)

votingProcedureDecoder :: Decode.Row VotingProcedure
votingProcedureDecoder =
  VotingProcedure
    { votingProcedureId = VotingProcedureId <$> Decode.column Decode.int8
    , votingProcedureTxId = TxId <$> Decode.column Decode.int8
    , votingProcedureIndex = fromIntegral <$> Decode.column Decode.int2
    , votingProcedureGovActionProposalId = GovActionProposalId <$> Decode.column Decode.int8
    , votingProcedureVoterRole = Decode.column (Decode.enum voterRoleFromText)
    , votingProcedureCommitteeVoter = fmap CommitteeHashId <$> Decode.column (Decode.nullable Decode.int8)
    , votingProcedureDrepVoter = fmap DrepHashId <$> Decode.column (Decode.nullable Decode.int8)
    , votingProcedurePoolVoter = fmap PoolHashId <$> Decode.column (Decode.nullable Decode.int8)
    , votingProcedureVote = Decode.column (Decode.enum voteFromText)
    , votingProcedureVotingAnchorId = fmap VotingAnchorId <$> Decode.column (Decode.nullable Decode.int8)
    , votingProcedureInvalid = fmap EventInfoId <$> Decode.column (Decode.nullable Decode.int8)
    }


votingProcedureEncoder :: VotingProcedure -> Encode.Params
votingProcedureEncoder votingProcedure =
  Encode.params $
       contramap (getVotingProcedureId . votingProcedureId) Encode.int8 -- votingProcedureId
    <> contramap (getTxId . votingProcedureTxId) Encode.int8 -- votingProcedureTxId
    <> contramap (fromIntegral . votingProcedureIndex) Encode.int2 -- votingProcedureIndex
    <> contramap (getGovActionProposalId . votingProcedureGovActionProposalId) Encode.int8 -- votingProcedureGovActionProposalId
    <> contramap (voterRoleToText . votingProcedureVoterRole) Encode.text -- votingProcedureVoterRole
    <> contramap (fmap getCommitteeHashId . votingProcedureCommitteeVoter) (Encode.nullable Encode.int8) -- votingProcedureCommitteeVoter
    <> contramap (fmap getDrepHashId . votingProcedureDrepVoter) (Encode.nullable Encode.int8) -- votingProcedureDrepVoter
    <> contramap (fmap getPoolHashId . votingProcedurePoolVoter) (Encode.nullable Encode.int8) -- votingProcedurePoolVoter
    <> contramap (voteToText . votingProcedureVote) Encode.text -- votingProcedureVote
    <> contramap (fmap getVotingAnchorId . votingProcedureVotingAnchorId) (Encode.nullable Encode.int8) -- votingProcedureVotingAnchorId
    <> contramap (fmap getEventInfoId . votingProcedureInvalid) (Encode.nullable Encode.int8) -- votingProcedureInvalid

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_anchor
Description: Acts as an anchor point for votes, ensuring they are securely recorded and linked to specific proposals.
-}
data VotingAnchor = VotingAnchor
  { votingAnchorId :: !VotingAnchorId
  , votingAnchorBlockId :: !BlockId       -- noreference
  , votingAnchorDataHash :: !ByteString
  , votingAnchorUrl :: !VoteUrl           -- sqltype=varchar
  , votingAnchorType :: !AnchorType       -- sqltype=anchorType
  } deriving (Eq, Show, Generic)
-- UniqueVotingAnchor  dataHash url type

newtype VotingAnchorId = VotingAnchorId { getVotingAnchorId :: Int64 }
  deriving (Eq, Show, Ord)

votingAnchorDecoder :: Decode.Row VotingAnchor
votingAnchorDecoder =
  VotingAnchor
    { votingAnchorId = VotingAnchorId <$> Decode.column Decode.int8
    , votingAnchorBlockId = BlockId <$> Decode.column Decode.int8
    , votingAnchorDataHash = Decode.column Decode.bytea
    , votingAnchorUrl = VoteUrl <$> Decode.column Decode.text
    , votingAnchorType = Decode.column (Decode.enum anchorTypeFromText)
    }

votingAnchorEncoder :: VotingAnchor -> Encode.Params
votingAnchorEncoder votingAnchor =
  Encode.params $
       contramap (getVotingAnchorId . votingAnchorId) Encode.int8 -- votingAnchorId
    <> contramap (getBlockId . votingAnchorBlockId) Encode.int8 -- votingAnchorBlockId
    <> contramap votingAnchorDataHash Encode.bytea -- votingAnchorDataHash
    <> contramap (getVoteUrl . votingAnchorUrl) Encode.text -- votingAnchorUrl
    <> contramap (anchorTypeToText . votingAnchorType) Encode.text -- votingAnchorType

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: constitution
Description: Holds the on-chain constitution, which defines the rules and principles of the blockchain's governance system.
-}
data Constitution = Constitution
  { constitutionId :: !ConstitutionId
  , constitutionGovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , constitutionVotingAnchorId :: !VotingAnchorId                -- noreference
  , constitutionScriptHash :: !(Maybe ByteString)                  -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

newtype ConstitutionId = ConstitutionId { getConstitutionId :: Int64 }
  deriving (Eq, Show, Ord)

constitutionDecoder :: Decode.Row Constitution
constitutionDecoder =
  Constitution
   { constitutionId = ConstitutionId <$> Decode.column Decode.int8
    , constitutionGovActionProposalId = fmap GovActionProposalId <$> Decode.column (Decode.nullable Decode.int8)
    , constitutionVotingAnchorId = VotingAnchorId <$> Decode.column Decode.int8
    , constitutionScriptHash = Decode.column (Decode.nullable Decode.bytea)
   }

constitutionEncoder :: Constitution -> Encode.Params
constitutionEncoder constitution =
  Encode.params $
       contramap (getConstitutionId . constitutionId) Encode.int8 -- constitutionId
    <> contramap (fmap getGovActionProposalId . constitutionGovActionProposalId) (Encode.nullable Encode.int8) -- constitutionGovActionProposalId
    <> contramap (getVotingAnchorId . constitutionVotingAnchorId) Encode.int8 -- constitutionVotingAnchorId
    <> contramap constitutionScriptHash (Encode.nullable Encode.bytea) -- constitutionScriptHash

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee
Description: Contains information about the committee, including the quorum requirements and the proposal being considered.
-}
data Committee = Committee
  { committeeId :: !CommitteeId
  , committeeGovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , committeeQuorumNumerator :: !Word64
  , committeeQuorumDenominator :: !Word64
  } deriving (Eq, Show, Generic)

newtype CommitteeId = CommitteeId { getCommitteeId :: Int64 }
  deriving (Eq, Show, Ord)

committeeDecoder :: Decode.Row Committee
committeeDecoder =
  Committee
    { committeeId = CommitteeId <$> Decode.column Decode.int8
    , committeeGovActionProposalId = fmap GovActionProposalId <$> Decode.column (Decode.nullable Decode.int8)
    , committeeQuorumNumerator = fromIntegral <$> Decode.column Decode.int8
    , committeeQuorumDenominator = fromIntegral <$> Decode.column Decode.int8
    }

committeeEncoder :: Committee -> Encode.Params
committeeEncoder committee =
  Encode.params $
       contramap (getCommitteeId . committeeId) Encode.int8 -- committeeId
    <> contramap (fmap getGovActionProposalId . committeeGovActionProposalId) (Encode.nullable Encode.int8) -- committeeGovActionProposalId
    <> contramap (fromIntegral . committeeQuorumNumerator) Encode.int8 -- committeeQuorumNumerator
    <> contramap (fromIntegral . committeeQuorumDenominator) Encode.int8 -- committeeQuorumDenominator

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_hash
Description: Stores hashes of committee records, which are used in governance processes.
-}
data CommitteeHash = CommitteeHash
  { committeeHashId :: !CommitteeHashId
  , committeeHashRaw :: !ByteString    -- sqltype=hash28type
  , committeeHashHasScript :: !Bool
  } deriving (Eq, Show, Generic)
-- UniqueCommitteeHash  raw hasScript

newtype CommitteeHashId = CommitteeHashId { getCommitteeHashId :: Int64 }
  deriving (Eq, Show, Ord)

committeeHashDecoder :: Decode.Row CommitteeHash
committeeHashDecoder =
  CommitteeHash
    { committeeHashId = CommitteeHashId <$> Decode.column Decode.int8
    , committeeHashRaw = Decode.column Decode.bytea
    , committeeHashHasScript = Decode.column Decode.bool
    }

committeeHashEncoder :: CommitteeHash -> Encode.Params
committeeHashEncoder committeeHash =
  Encode.params $
       contramap (getCommitteeHashId . committeeHashId) Encode.int8 -- committeeHashId
    <> contramap committeeHashRaw Encode.bytea -- committeeHashRaw
    <> contramap committeeHashHasScript Encode.bool -- committeeHashHasScript

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_member
Description: Contains information about committee members.
-}
data CommitteeMember = CommitteeMember
  { committeeMemberId :: !CommitteeMemberId
  , committeeMemberCommitteeId :: !CommitteeId          -- OnDeleteCascade -- here intentionally we use foreign keys
  , committeeMemberCommitteeHashId :: !CommitteeHashId  -- noreference
  , committeeMemberExpirationEpoch :: !Word64           -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype CommitteeMemberId = CommitteeMemberId { getCommitteeMemberId :: Int64 }
  deriving (Eq, Show, Ord)

committeeMemberDecoder :: Decode.Row CommitteeMember
committeeMemberDecoder =
 CommitteeMember
  { committeeMemberId = CommitteeMemberId <$> Decode.column Decode.int8
  , committeeMemberCommitteeId = CommitteeId <$> Decode.column Decode.int8
  , committeeMemberCommitteeHashId = CommitteeHashId <$> Decode.column Decode.int8
  , committeeMemberExpirationEpoch = fromIntegral <$> Decode.column Decode.int8
  }

committeeMemberEncoder :: CommitteeMember -> Encode.Params
committeeMemberEncoder member =
 Encode.params $
      contramap (getCommitteeId . committeeMemberCommitteeId) Encode.int8
   <> contramap (getCommitteeHashId . committeeMemberCommitteeHashId) Encode.int8
   <> contramap (fromIntegral . committeeMemberExpirationEpoch) Encode.int8

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_registration
Description: Contains information about the registration of committee members, including their public keys and other identifying information.
-}
data CommitteeRegistration = CommitteeRegistration
  { committeeRegistrationId :: !CommitteeRegistrationId
  , committeeRegistrationTxId :: !TxId -- noreference
  , committeeRegistrationCertIndex :: !Word16
  , committeeRegistrationColdKeyId :: !CommitteeHashId -- noreference
  , committeeRegistrationHotKeyId :: !CommitteeHashId  -- noreference
  } deriving (Eq, Show, Generic)

newtype CommitteeRegistrationId = CommitteeRegistrationId { getCommitteeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

committeeRegistrationDecoder :: Decode.Row CommitteeRegistration
committeeRegistrationDecoder =
  CommitteeRegistration {
    committeeRegistrationId = CommitteeRegistrationId <$> Decode.column Decode.int8,
    committeeRegistrationTxId = TxId <$> Decode.column Decode.int8,
    committeeRegistrationCertIndex = fromIntegral <$> Decode.column Decode.int2,
    committeeRegistrationColdKeyId = CommitteeHashId <$> Decode.column Decode.int8,
    committeeRegistrationHotKeyId = CommitteeHashId <$> Decode.column Decode.int8
    }

committeeRegistrationEncoder :: CommitteeRegistration -> Encode.Params
committeeRegistrationEncoder reg =
  Encode.params $ mconcat
    [ Encode.param $ getTxId $ committeeRegistrationTxId reg -- committeeRegistrationTxId
    , Encode.param $ fromIntegral $ committeeRegistrationCertIndex reg -- committeeRegistrationCertIndex
    , Encode.param $ getCommitteeHashId $ committeeRegistrationColdKeyId reg -- committeeRegistrationColdKeyId
    , Encode.param $ getCommitteeHashId $ committeeRegistrationHotKeyId reg -- committeeRegistrationHotKeyId
    ]

{-|
Table Name: committee_de_registration
Description: Contains information about the deregistration of committee members, including their public keys and other identifying information.
-}
data CommitteeDeRegistration = CommitteeDeRegistration
  { committeeDeRegistrationId :: !CommitteeDeRegistrationId
  , committeeDeRegistrationTxId :: !TxId -- noreference
  , committeeDeRegistrationCertIndex :: !Word16
  , committeeDeRegistrationColdKeyId :: !CommitteeHashId -- noreference
  , committeeDeRegistrationVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  } deriving (Eq, Show, Generic)

newtype CommitteeDeRegistrationId = CommitteeDeRegistrationId { getCommitteeDeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

committeeDeRegistrationDecoder :: Decode.Row CommitteeDeRegistration
committeeDeRegistrationDecoder =
  CommitteeDeRegistration
    { committeeDeRegistrationId = CommitteeDeRegistrationId <$> Decode.column Decode.int8
    , committeeDeRegistrationTxId = TxId <$> Decode.column Decode.int8
    , committeeDeRegistrationCertIndex = fromIntegral <$> Decode.column Decode.int2
    , committeeDeRegistrationColdKeyId = CommitteeHashId <$> Decode.column Decode.int8
    , committeeDeRegistrationVotingAnchorId = fmap VotingAnchorId <$> Decode.column (Decode.nullable Decode.int8)
    }

committeeDeRegistrationEncoder :: CommitteeDeRegistration -> Encode.Params
committeeDeRegistrationEncoder reg =
  Encode.params $ mconcat
    [ Encode.param $ getCommitteeDeRegistrationId $ committeeDeRegistrationId reg -- committeeDeRegistrationId
    , Encode.param $ getTxId $ committeeDeRegistrationTxId reg -- committeeDeRegistrationTxId
    , Encode.param $ fromIntegral $ committeeDeRegistrationCertIndex reg -- committeeDeRegistrationCertIndex
    , Encode.param $ getCommitteeHashId $ committeeDeRegistrationColdKeyId reg -- committeeDeRegistrationColdKeyId
    , Encode.param (getVotingAnchorId <$> committeeDeRegistrationVotingAnchorId reg) -- committeeDeRegistrationVotingAnchorId
    ]

{-|
Table Name: param_proposal
Description: Contains proposals for changes to the protocol parameters, including the proposed values and the expiration date.
-}
data ParamProposal = ParamProposal
  { paramProposalId :: !ParamProposalId
  , paramProposalEpochNo :: !(Maybe Word64)                -- sqltype=word31type
  , paramProposalKey :: !(Maybe ByteString)                -- sqltype=hash28type
  , paramProposalMinFeeA :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposalMinFeeB :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposalMaxBlockSize :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMaxTxSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposalMaxBhSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposalKeyDeposit :: !(Maybe DbLovelace)         -- sqltype=lovelace
  , paramProposalPoolDeposit :: !(Maybe DbLovelace)        -- sqltype=lovelace
  , paramProposalMaxEpoch :: !(Maybe DbWord64)             -- sqltype=word64type
  , paramProposalOptimalPoolCount :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalInfluence :: !(Maybe Double)
  , paramProposalMonetaryExpandRate :: !(Maybe Double)
  , paramProposalTreasuryGrowthRate :: !(Maybe Double)
  , paramProposalDecentralisation :: !(Maybe Double)
  , paramProposalEntropy :: !(Maybe ByteString)            -- sqltype=hash32type
  , paramProposalProtocolMajor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposalProtocolMinor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposalMinUtxoValue :: !(Maybe DbLovelace)       -- sqltype=lovelace

  , paramProposalMinPoolCost :: !(Maybe DbLovelace)        -- sqltype=lovelace
  , paramProposalCoinsPerUtxoSize :: !(Maybe DbLovelace)   -- sqltype=lovelace
  , paramProposalCostModelId :: !(Maybe CostModelId)       -- noreference
  , paramProposalPriceMem :: !(Maybe Double)
  , paramProposalPriceStep :: !(Maybe Double)
  , paramProposalMaxTxExMem :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposalMaxTxExSteps :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMaxBlockExMem :: !(Maybe DbWord64)        -- sqltype=word64type
  , paramProposalMaxBlockExSteps :: !(Maybe DbWord64)      -- sqltype=word64type
  , paramProposalMaxValSize :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposalCollateralPercent :: !(Maybe Word16)      -- sqltype=word31type
  , paramProposalMaxCollateralInputs :: !(Maybe Word16)    -- sqltype=word31type

  , paramProposalPvtMotionNoConfidence :: !(Maybe Double)
  , paramProposalPvtCommitteeNormal :: !(Maybe Double)
  , paramProposalPvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposalPvtHardForkInitiation :: !(Maybe Double)
  , paramProposalPvtppSecurityGroup :: !(Maybe Double)
  , paramProposalDvtMotionNoConfidence :: !(Maybe Double)
  , paramProposalDvtCommitteeNormal :: !(Maybe Double)
  , paramProposalDvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposalDvtUpdateToConstitution :: !(Maybe Double)
  , paramProposalDvtHardForkInitiation :: !(Maybe Double)
  , paramProposalDvtPPNetworkGroup :: !(Maybe Double)
  , paramProposalDvtPPEconomicGroup :: !(Maybe Double)
  , paramProposalDvtPPTechnicalGroup :: !(Maybe Double)
  , paramProposalDvtPPGovGroup :: !(Maybe Double)
  , paramProposalDvtTreasuryWithdrawal :: !(Maybe Double)

  , paramProposalCommitteeMinSize :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalCommitteeMaxTermLength :: !(Maybe DbWord64) --
  , paramProposalGovActionLifetime :: !(Maybe DbWord64)    -- sqltype=word64type
  , paramProposalGovActionDeposit :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalDrepDeposit :: !(Maybe DbWord64)          -- sqltype=word64type
  , paramProposalDrepActivity :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMinFeeRefScriptCostPerByte :: !(Maybe Double)

  , paramProposalRegisteredTxId :: !TxId                 -- noreference
  } deriving (Show, Eq, Generic)

newtype ParamProposalId = ParamProposalId { getParamProposalId :: Int64 }
  deriving (Eq, Show, Ord)

paramProposalDecoder :: Decode.Row ParamProposal
paramProposalDecoder =
  ParamProposal
    { paramProposalId = ParamProposalId <$> Decode.column Decode.int8
    , paramProposalEpochNo = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalKey = Decode.column (Decode.nullable Decode.bytea)
    , paramProposalMinFeeA = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMinFeeB = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxBlockSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxTxSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxBhSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalKeyDeposit = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalPoolDeposit = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxEpoch = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalOptimalPoolCount = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalInfluence = Decode.column (Decode.nullable Decode.float8)
    , paramProposalMonetaryExpandRate = Decode.column (Decode.nullable Decode.float8)
    , paramProposalTreasuryGrowthRate = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDecentralisation = Decode.column (Decode.nullable Decode.float8)
    , paramProposalEntropy = Decode.column (Decode.nullable Decode.bytea)
    , paramProposalProtocolMajor = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , paramProposalProtocolMinor = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , paramProposalMinUtxoValue = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMinPoolCost = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalCoinsPerUtxoSize = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalCostModelId = fmap CostModelId <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalPriceMem = Decode.column (Decode.nullable Decode.float8)
    , paramProposalPriceStep = Decode.column (Decode.nullable Decode.float8)
    , paramProposalMaxTxExMem = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxTxExSteps = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxBlockExMem = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxBlockExSteps = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMaxValSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalCollateralPercent = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , paramProposalMaxCollateralInputs = fmap fromIntegral <$> Decode.column (Decode.nullable Decode.int2)
    , paramProposalPvtMotionNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , paramProposalPvtCommitteeNormal = Decode.column (Decode.nullable Decode.float8)
    , paramProposalPvtCommitteeNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , paramProposalPvtHardForkInitiation = Decode.column (Decode.nullable Decode.float8)
    , paramProposalPvtppSecurityGroup = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtMotionNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtCommitteeNormal = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtCommitteeNoConfidence = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtUpdateToConstitution = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtHardForkInitiation = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtPPNetworkGroup = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtPPEconomicGroup = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtPPTechnicalGroup = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtPPGovGroup = Decode.column (Decode.nullable Decode.float8)
    , paramProposalDvtTreasuryWithdrawal = Decode.column (Decode.nullable Decode.float8)
    , paramProposalCommitteeMinSize = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalCommitteeMaxTermLength = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalGovActionLifetime = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalGovActionDeposit = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalDrepDeposit = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalDrepActivity = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    , paramProposalMinFeeRefScriptCostPerByte = Decode.column (Decode.nullable Decode.float8)
    , paramProposalRegisteredTxId = TxId <$> Decode.column Decode.int8
    }

paramProposalEncoder :: ParamProposal -> Encode.Params
paramProposalEncoder paramProposal =
  Encode.params $
    mconcat
      [ Encode.param $ getParamProposalId $ paramProposalId paramProposal -- paramProposalId
      , Encode.param (fromIntegral <$> paramProposalEpochNo paramProposal) -- paramProposalEpochNo
      , Encode.param $ paramProposalKey paramProposal -- paramProposalKey
      , Encode.param (getDbWord64 <$> paramProposalMinFeeA paramProposal) -- paramProposalMinFeeA
      , Encode.param (getDbWord64 <$> paramProposalMinFeeB paramProposal) -- paramProposalMinFeeB
      , Encode.param (getDbWord64 <$> paramProposalMaxBlockSize paramProposal) -- paramProposalMaxBlockSize
      , Encode.param (getDbWord64 <$> paramProposalMaxTxSize paramProposal) -- paramProposalMaxTxSize
      , Encode.param (getDbWord64 <$> paramProposalMaxBhSize paramProposal) -- paramProposalMaxBhSize
      , Encode.param (getDbLovelace <$> paramProposalKeyDeposit paramProposal) -- paramProposalKeyDeposit
      , Encode.param (getDbLovelace <$> paramProposalPoolDeposit paramProposal) -- paramProposalPoolDeposit
      , Encode.param (getDbWord64 <$> paramProposalMaxEpoch paramProposal) -- paramProposalMaxEpoch
      , Encode.param (getDbWord64 <$> paramProposalOptimalPoolCount paramProposal) -- paramProposalOptimalPoolCount
      , Encode.param $ paramProposalInfluence paramProposal -- paramProposalInfluence
      , Encode.param $ paramProposalMonetaryExpandRate paramProposal -- paramProposalMonetaryExpandRate
      , Encode.param $ paramProposalTreasuryGrowthRate paramProposal -- paramProposalTreasuryGrowthRate
      , Encode.param $ paramProposalDecentralisation paramProposal -- paramProposalDecentralisation
      , Encode.param $ paramProposalEntropy paramProposal -- paramProposalEntropy
      , Encode.param (fromIntegral <$> paramProposalProtocolMajor paramProposal) -- paramProposalProtocolMajor
      , Encode.param (fromIntegral <$> paramProposalProtocolMinor paramProposal) -- paramProposalProtocolMinor
      , Encode.param (getDbLovelace <$> paramProposalMinUtxoValue paramProposal) -- paramProposalMinUtxoValue
      , Encode.param (getDbLovelace <$> paramProposalMinPoolCost paramProposal) -- paramProposalMinPoolCost
      , Encode.param (getDbLovelace <$> paramProposalCoinsPerUtxoSize paramProposal) -- paramProposalCoinsPerUtxoSize
      , Encode.param (getCostModelId <$> paramProposalCostModelId paramProposal) -- paramProposalCostModelId
      , Encode.param $ paramProposalPriceMem paramProposal -- paramProposalPriceMem
      , Encode.param $ paramProposalPriceStep paramProposal -- paramProposalPriceStep
      , Encode.param (getDbWord64 <$> paramProposalMaxTxExMem paramProposal) -- paramProposalMaxTxExMem
      , Encode.param (getDbWord64 <$> paramProposalMaxTxExSteps paramProposal) -- paramProposalMaxTxExSteps
      , Encode.param (getDbWord64 <$> paramProposalMaxBlockExMem paramProposal) -- paramProposalMaxBlockExMem
      , Encode.param (getDbWord64 <$> paramProposalMaxBlockExSteps paramProposal) -- paramProposalMaxBlockExSteps
      , Encode.param (getDbWord64 <$> paramProposalMaxValSize paramProposal) -- paramProposalMaxValSize
      , Encode.param (fromIntegral <$> paramProposalCollateralPercent paramProposal) -- paramProposalCollateralPercent
      , Encode.param (fromIntegral <$> paramProposalMaxCollateralInputs paramProposal) -- paramProposalMaxCollateralInputs
      , Encode.param $ paramProposalPvtMotionNoConfidence paramProposal -- paramProposalPvtMotionNoConfidence
      , Encode.param $ paramProposalPvtCommitteeNormal paramProposal -- paramProposalPvtCommitteeNormal
      , Encode.param $ paramProposalPvtCommitteeNoConfidence paramProposal -- paramProposalPvtCommitteeNoConfidence
      , Encode.param $ paramProposalPvtHardForkInitiation paramProposal -- paramProposalPvtHardForkInitiation
      , Encode.param $ paramProposalPvtppSecurityGroup paramProposal -- paramProposalPvtppSecurityGroup
      , Encode.param $ paramProposalDvtMotionNoConfidence paramProposal -- paramProposalDvtMotionNoConfidence
      , Encode.param $ paramProposalDvtCommitteeNormal paramProposal -- paramProposalDvtCommitteeNormal
      , Encode.param $ paramProposalDvtCommitteeNoConfidence paramProposal -- paramProposalDvtCommitteeNoConfidence
      , Encode.param $ paramProposalDvtUpdateToConstitution paramProposal -- paramProposalDvtUpdateToConstitution
      , Encode.param $ paramProposalDvtHardForkInitiation paramProposal -- paramProposalDvtHardForkInitiation
      , Encode.param $ paramProposalDvtPPNetworkGroup paramProposal -- paramProposalDvtPPNetworkGroup
      , Encode.param $ paramProposalDvtPPEconomicGroup paramProposal -- paramProposalDvtPPEconomicGroup
      , Encode.param $ paramProposalDvtPPTechnicalGroup paramProposal -- paramProposalDvtPPTechnicalGroup
      , Encode.param $ paramProposalDvtPPGovGroup paramProposal -- paramProposalDvtPPGovGroup
      , Encode.param $ paramProposalDvtTreasuryWithdrawal paramProposal -- paramProposalDvtTreasuryWithdrawal
      , Encode.param (getDbWord64 <$> paramProposalCommitteeMinSize paramProposal) -- paramProposalCommitteeMinSize
      , Encode.param (getDbWord64 <$> paramProposalCommitteeMaxTermLength paramProposal) -- paramProposalCommitteeMaxTermLength
      , Encode.param (getDbWord64 <$> paramProposalGovActionLifetime paramProposal) -- paramProposalGovActionLifetime
      , Encode.param (getDbWord64 <$> paramProposalGovActionDeposit paramProposal) -- paramProposalGovActionDeposit
      , Encode.param (getDbWord64 <$> paramProposalDrepDeposit paramProposal) -- paramProposalDrepDeposit
      , Encode.param (getDbWord64 <$> paramProposalDrepActivity paramProposal) -- paramProposalDrepActivity
      , Encode.param $ paramProposalMinFeeRefScriptCostPerByte paramProposal -- paramProposalMinFeeRefScriptCostPerByte
      ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury_withdrawal
Description:
-}
data TreasuryWithdrawal = TreasuryWithdrawal
  { treasuryWithdrawalId :: !TreasuryWithdrawalId
  , treasuryWithdrawalGovActionProposalId :: !GovActionProposalId -- noreference
  , treasuryWithdrawalStakeAddressId :: !StakeAddressId          -- noreference
  , treasuryWithdrawalAmount :: !DbLovelace                      -- sqltype=lovelace
  } deriving (Eq, Show, Generic)

newtype TreasuryWithdrawalId = TreasuryWithdrawalId { getTreasuryWithdrawalId :: Int64 }
  deriving (Eq, Show, Ord)

treasuryWithdrawalDecoder :: Decode.Row TreasuryWithdrawal
treasuryWithdrawalDecoder =
  TreasuryWithdrawal
    { treasuryWithdrawalId = TreasuryWithdrawalId <$> Decode.column Decode.int8
    , treasuryWithdrawalGovActionProposalId = GovActionProposalId <$> Decode.column Decode.int8
    , treasuryWithdrawalStakeAddressId = StakeAddressId <$> Decode.column Decode.int8
    , treasuryWithdrawalAmount = DbLovelace <$> Decode.column Decode.int8
    }

treasuryWithdrawalEncoder :: TreasuryWithdrawal -> Encode.Params
treasuryWithdrawalEncoder treasuryWithdrawal =
  Encode.params $ mconcat
    [ Encode.param $ getTreasuryWithdrawalId $ treasuryWithdrawalId treasuryWithdrawal -- treasuryWithdrawalId
    , Encode.param $ getGovActionProposalId $ treasuryWithdrawalGovActionProposalId treasuryWithdrawal -- treasuryWithdrawalGovActionProposalId
    , Encode.param $ getStakeAddressId $ treasuryWithdrawalStakeAddressId treasuryWithdrawal -- treasuryWithdrawalStakeAddressId
    , Encode.param $ getDbLovelace $ treasuryWithdrawalAmount treasuryWithdrawal -- treasuryWithdrawalAmount
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: event_info
Description: Contains information about events, including the epoch in which they occurred and the type of event.
-}
data EventInfo = EventInfo
  { eventInfoId :: !EventInfoId
  , eventInfoTxId :: !(Maybe TxId)           -- noreference
  , eventInfoEpoch :: !Word64              -- sqltype=word31type
  , eventInfoType :: !Text
  , eventInfoExplanation :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

newtype EventInfoId = EventInfoId { getEventInfoId :: Int64 }
  deriving (Eq, Show, Ord)

eventInfoDecoder :: Decode.Row EventInfo
eventInfoDecoder =
  EventInfo
    { eventInfoId = EventInfoId <$> Decode.column Decode.int8
    , eventInfoTxId = fmap TxId <$> Decode.column (Decode.nullable Decode.int8)
    , eventInfoEpoch = fromIntegral <$> Decode.column Decode.int8
    , eventInfoType = Decode.column Decode.text
    , eventInfoExplanation = Decode.column (Decode.nullable Decode.text)
    }

eventInfoEncoder :: EventInfo -> Encode.Params
eventInfoEncoder eventInfo =
  Encode.params $ mconcat
    [ Encode.param $ getEventInfoId $ eventInfoId eventInfo
    , Encode.param (getTxId <$> eventInfoTxId eventInfo)
    , Encode.param $ fromIntegral $ eventInfoEpoch eventInfo
    , Encode.param $ eventInfoType eventInfo
    , Encode.param $ eventInfoExplanation eventInfo
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- MULTI ASSETS
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

{-|
Table Name: multi_asset
Description: Contains information about multi-assets, including the policy and name of the asset.
-}
data MultiAsset = MultiAsset
  { multiAssetId :: !MultiAssetId
  , multiAssetPolicy :: !ByteString -- sqltype=hash28type
  , multiAssetName :: !ByteString   -- sqltype=asset32type
  , multiAssetFingerprint :: !Text
  } deriving (Eq, Show, Generic)
-- UniqueMultiAsset  policy name

newtype MultiAssetId = MultiAssetId { getMultiAssetId :: Int64 }
  deriving (Eq, Show, Ord)

multiAssetDecoder :: Decode.Row MultiAsset
multiAssetDecoder =
  MultiAsset
    { multiAssetId = MultiAssetId <$> Decode.column Decode.int8
    , multiAssetPolicy = Decode.column Decode.bytea
    , multiAssetName = Decode.column Decode.bytea
    , multiAssetFingerprint = Decode.column Decode.text
    }

multiAssetEncoder :: MultiAsset -> Encode.Params
multiAssetEncoder multiAsset =
  Encode.params $ mconcat
    [ Encode.param (getMultiAssetId $ multiAssetId multiAsset)
    , Encode.param (multiAssetPolicy multiAsset)
    , Encode.param (multiAssetName multiAsset)
    , Encode.param (multiAssetFingerprint multiAsset)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: ma_tx_mint
Description: Contains information about the minting of multi-assets, including the quantity of the asset and the transaction in which it was minted.
-}
data MaTxMint = MaTxMint
  { maTxMintId :: !MaTxMintId
  , maTxMintIdent :: !MultiAssetId -- noreference
  , maTxMintQuantity :: !DbInt65   -- sqltype=int65type
  , maTxMintTxId :: !TxId          -- noreference
  } deriving (Eq, Show, Generic)

newtype MaTxMintId = MaTxMintId { getMaTxMintId :: Int64 }
  deriving (Eq, Show, Ord)

maTxMintDecoder :: Decode.Row MaTxMint
maTxMintDecoder =
  MaTxMint
    { maTxMintId = MaTxMintId <$> Decode.column Decode.int8
    , maTxMintIdent = MultiAssetId <$> Decode.column Decode.int8
    , maTxMintQuantity = DbInt65 <$> Decode.column Decode.int8
    , maTxMintTxId = TxId <$> Decode.column Decode.int8
    }

maTxMintEncoder :: MaTxMint -> Encode.Params
maTxMintEncoder maTxMint =
  Encode.params $ mconcat
    [ Encode.param (getMaTxMintId $ maTxMintId maTxMint)
    , Encode.param (getMultiAssetId $ maTxMintIdent maTxMint)
    , Encode.param (getDbInt65 $ maTxMintQuantity maTxMint)
    , Encode.param (getTxId $ maTxMintTxId maTxMint)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-- These tables manage off-chain data, including pool and vote data.
----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainPoolData = OffChainPoolData
  { offChainPoolDataId :: !OffChainPoolDataId
  , offChainPoolDataPoolId :: !PoolHashId           -- noreference
  , offChainPoolDataTickerName :: !Text
  , offChainPoolDataHash :: !ByteString             -- sqltype=hash32type
  , offChainPoolDataJson :: !Text                   -- sqltype=jsonb
  , offChainPoolDataBytes :: !ByteString            -- sqltype=bytea
  , offChainPoolDataPmrId :: !PoolMetadataRefId     -- noreference
  } deriving (Eq, Show, Generic)

newtype OffChainPoolDataId = OffChainPoolDataId { getOffChainPoolDataId :: Int64 }
  deriving (Eq, Show, Ord)

offChainPoolDataDecoder :: Decode.Row OffChainPoolData
offChainPoolDataDecoder =
  OffChainPoolData
    { offChainPoolDataId = OffChainPoolDataId <$> Decode.column Decode.int8
    , offChainPoolDataPoolId = PoolHashId <$> Decode.column Decode.int8
    , offChainPoolDataTickerName = Decode.column Decode.text
    , offChainPoolDataHash = Decode.column Decode.bytea
    , offChainPoolDataJson = Decode.column Decode.text
    , offChainPoolDataBytes = Decode.column Decode.bytea
    , offChainPoolDataPmrId = PoolMetadataRefId <$> Decode.column Decode.int8
    }

offChainPoolDataEncoder :: OffChainPoolData -> Encode.Params
offChainPoolDataEncoder offChainPoolData =
  Encode.params $ mconcat
    [ Encode.param (getOffChainPoolDataId $ offChainPoolDataId offChainPoolData)
    , Encode.param (getPoolHashId $ offChainPoolDataPoolId offChainPoolData)
    , Encode.param (offChainPoolDataTickerName offChainPoolData)
    , Encode.param (offChainPoolDataHash offChainPoolData)
    , Encode.param (offChainPoolDataJson offChainPoolData)
    , Encode.param (offChainPoolDataBytes offChainPoolData)
    , Encode.param (getPoolMetadataRefId $ offChainPoolDataPmrId offChainPoolData)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
-- The pool metadata fetch error. We duplicate the poolId for easy access.
-- TODO(KS): Debatable whether we need to persist this between migrations!
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorId :: !OffChainPoolFetchErrorId
  , offChainPoolFetchErrorPoolId :: !PoolHashId              -- noreference
  , offChainPoolFetchErrorFetchTime :: !UTCTime              -- sqltype=timestamp
  , offChainPoolFetchErrorPmrId :: !PoolMetadataRefId        -- noreference
  , offChainPoolFetchErrorFetchError :: !Text
  , offChainPoolFetchErrorRetryCount :: !Word                -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype OffChainPoolFetchErrorId = OffChainPoolFetchErrorId { getOffChainPoolFetchErrorId :: Int64 }
  deriving (Eq, Show, Ord)

offChainPoolFetchErrorDecoder :: Decode.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder =
  OffChainPoolFetchError
    { offChainPoolFetchErrorId = OffChainPoolFetchErrorId <$> Decode.column Decode.int8
    , offChainPoolFetchErrorPoolId = PoolHashId <$> Decode.column Decode.int8
    , offChainPoolFetchErrorFetchTime = Decode.column Decode.timestamptz
    , offChainPoolFetchErrorPmrId = PoolMetadataRefId <$> Decode.column Decode.int8
    , offChainPoolFetchErrorFetchError = Decode.column Decode.text
    , offChainPoolFetchErrorRetryCount = fromIntegral <$> Decode.column Decode.int4
    }

offChainPoolFetchErrorEncoder :: OffChainPoolFetchError -> Encode.Params
offChainPoolFetchErrorEncoder offChainPoolFetchError =
  Encode.params $ mconcat
    [ Encode.param (getOffChainPoolFetchErrorId $ offChainPoolFetchErrorId offChainPoolFetchError)
    , Encode.param (getPoolHashId $ offChainPoolFetchErrorPoolId offChainPoolFetchError)
    , Encode.param (offChainPoolFetchErrorFetchTime offChainPoolFetchError)
    , Encode.param (getPoolMetadataRefId $ offChainPoolFetchErrorPmrId offChainPoolFetchError)
    , Encode.param (offChainPoolFetchErrorFetchError offChainPoolFetchError)
    , Encode.param (fromIntegral $ offChainPoolFetchErrorRetryCount offChainPoolFetchError)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteData = OffChainVoteData
  { offChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteDataVotingAnchorId :: !VotingAnchorId         -- noreference
  , offChainVoteDataHash :: !ByteString
  , offChainVoteDataLanguage :: !Text
  , offChainVoteDataComment :: !(Maybe Text)
  , offChainVoteDataJson :: !Text                             -- sqltype=jsonb
  , offChainVoteDataBytes :: !ByteString                      -- sqltype=bytea
  , offChainVoteDataWarning :: !(Maybe Text)
  , offChainVoteDataIsValid :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

newtype OffChainVoteDataId = OffChainVoteDataId { getOffChainVoteDataId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteDataDecoder :: Decode.Row OffChainVoteData
offChainVoteDataDecoder =
  OffChainVoteData
    { offChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteDataVotingAnchorId = VotingAnchorId <$> Decode.column Decode.int8
    , offChainVoteDataHash = Decode.column Decode.bytea
    , offChainVoteDataLanguage = Decode.column Decode.text
    , offChainVoteDataComment = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDataJson = Decode.column Decode.text
    , offChainVoteDataBytes = Decode.column Decode.bytea
    , offChainVoteDataWarning = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDataIsValid = Decode.column (Decode.nullable Decode.bool)
    }

offChainVoteDataEncoder :: OffChainVoteData -> Encode.Params
offChainVoteDataEncoder offChainVoteData =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteDataId $ offChainVoteDataId offChainVoteData)
    , Encode.param (getVotingAnchorId $ offChainVoteDataVotingAnchorId offChainVoteData)
    , Encode.param (offChainVoteDataHash offChainVoteData)
    , Encode.param (offChainVoteDataLanguage offChainVoteData)
    , Encode.param (offChainVoteDataComment offChainVoteData)
    , Encode.param (offChainVoteDataJson offChainVoteData)
    , Encode.param (offChainVoteDataBytes offChainVoteData)
    , Encode.param (offChainVoteDataWarning offChainVoteData)
    , Encode.param (offChainVoteDataIsValid offChainVoteData)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionDataId :: !OffChainVoteGovActionDataId
  , offChainVoteGovActionDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteGovActionDataTitle :: !Text
  , offChainVoteGovActionDataAbstract :: !Text
  , offChainVoteGovActionDataMotivation :: !Text
  , offChainVoteGovActionDataRationale :: !Text
  } deriving (Eq, Show, Generic)

newtype OffChainVoteGovActionDataId = OffChainVoteGovActionDataId { getOffChainVoteGovActionDataId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteGovActionDataDecoder :: Decode.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder =
  OffChainVoteGovActionData
    { offChainVoteGovActionDataId = OffChainVoteGovActionDataId <$> Decode.column Decode.int8
    , offChainVoteGovActionDataOffChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteGovActionDataTitle = Decode.column Decode.text
    , offChainVoteGovActionDataAbstract = Decode.column Decode.text
    , offChainVoteGovActionDataMotivation = Decode.column Decode.text
    , offChainVoteGovActionDataRationale = Decode.column Decode.text
    }

offChainVoteGovActionDataEncoder :: OffChainVoteGovActionData -> Encode.Params
offChainVoteGovActionDataEncoder offChainVoteGovActionData =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteGovActionDataId $ offChainVoteGovActionDataId offChainVoteGovActionData)
    , Encode.param (getOffChainVoteDataId $ offChainVoteGovActionDataOffChainVoteDataId offChainVoteGovActionData)
    , Encode.param (offChainVoteGovActionDataTitle offChainVoteGovActionData)
    , Encode.param (offChainVoteGovActionDataAbstract offChainVoteGovActionData)
    , Encode.param (offChainVoteGovActionDataMotivation offChainVoteGovActionData)
    , Encode.param (offChainVoteGovActionDataRationale offChainVoteGovActionData)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepDataId :: !OffChainVoteDrepDataId
  , offChainVoteDrepDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteDrepDataPaymentAddress :: !(Maybe Text)
  , offChainVoteDrepDataGivenName :: !Text
  , offChainVoteDrepDataObjectives :: !(Maybe Text)
  , offChainVoteDrepDataMotivations :: !(Maybe Text)
  , offChainVoteDrepDataQualifications :: !(Maybe Text)
  , offChainVoteDrepDataImageUrl :: !(Maybe Text)
  , offChainVoteDrepDataImageHash :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

newtype OffChainVoteDrepDataId = OffChainVoteDrepDataId { getOffChainVoteDrepDataId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteDrepDataDecoder :: Decode.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder =
  OffChainVoteDrepData
    { offChainVoteDrepDataId = OffChainVoteDrepDataId <$> Decode.column Decode.int8
    , offChainVoteDrepDataOffChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteDrepDataPaymentAddress = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDrepDataGivenName = Decode.column Decode.text
    , offChainVoteDrepDataObjectives = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDrepDataMotivations = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDrepDataQualifications = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDrepDataImageUrl = Decode.column (Decode.nullable Decode.text)
    , offChainVoteDrepDataImageHash = Decode.column (Decode.nullable Decode.text)
    }

offChainVoteDrepDataEncoder :: OffChainVoteDrepData -> Encode.Params
offChainVoteDrepDataEncoder offChainVoteDrepData =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteDrepDataId $ offChainVoteDrepDataId offChainVoteDrepData)
    , Encode.param (getOffChainVoteDataId $ offChainVoteDrepDataOffChainVoteDataId offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataPaymentAddress offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataGivenName offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataObjectives offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataMotivations offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataQualifications offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataImageUrl offChainVoteDrepData)
    , Encode.param (offChainVoteDrepDataImageHash offChainVoteDrepData)
    ]
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthorId :: !OffChainVoteAuthorId
  , offChainVoteAuthorOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteAuthorName :: !(Maybe Text)
  , offChainVoteAuthorWitnessAlgorithm :: !Text
  , offChainVoteAuthorPublicKey :: !Text
  , offChainVoteAuthorSignature :: !Text
  , offChainVoteAuthorWarning :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

newtype OffChainVoteAuthorId = OffChainVoteAuthorId { getOffChainVoteAuthorId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteAuthorDecoder :: Decode.Row OffChainVoteAuthor
offChainVoteAuthorDecoder =
  OffChainVoteAuthor
    { offChainVoteAuthorId = OffChainVoteAuthorId <$> Decode.column Decode.int8
    , offChainVoteAuthorOffChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteAuthorName = Decode.column (Decode.nullable Decode.text)
    , offChainVoteAuthorWitnessAlgorithm = Decode.column Decode.text
    , offChainVoteAuthorPublicKey = Decode.column Decode.text
    , offChainVoteAuthorSignature = Decode.column Decode.text
    , offChainVoteAuthorWarning = Decode.column (Decode.nullable Decode.text)
    }

offChainVoteAuthorEncoder :: OffChainVoteAuthor -> Encode.Params
offChainVoteAuthorEncoder offChainVoteAuthor =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteAuthorId $ offChainVoteAuthorId offChainVoteAuthor)
    , Encode.param (getOffChainVoteDataId $ offChainVoteAuthorOffChainVoteDataId offChainVoteAuthor)
    , Encode.param (offChainVoteAuthorName offChainVoteAuthor)
    , Encode.param (offChainVoteAuthorWitnessAlgorithm offChainVoteAuthor)
    , Encode.param (offChainVoteAuthorPublicKey offChainVoteAuthor)
    , Encode.param (offChainVoteAuthorSignature offChainVoteAuthor)
    , Encode.param (offChainVoteAuthorWarning offChainVoteAuthor)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReferenceId :: !OffChainVoteReferenceId
  , offChainVoteReferenceOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteReferenceLabel :: !Text
  , offChainVoteReferenceUri :: !Text
  , offChainVoteReferenceHashDigest :: !(Maybe Text)
  , offChainVoteReferenceHashAlgorithm :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

newtype OffChainVoteReferenceId = OffChainVoteReferenceId { getOffChainVoteReferenceId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteReferenceDecoder :: Decode.Row OffChainVoteReference
offChainVoteReferenceDecoder =
  OffChainVoteReference
    { offChainVoteReferenceId = OffChainVoteReferenceId <$> Decode.column Decode.int8
    , offChainVoteReferenceOffChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteReferenceLabel = Decode.column Decode.text
    , offChainVoteReferenceUri = Decode.column Decode.text
    , offChainVoteReferenceHashDigest = Decode.column (Decode.nullable Decode.text)
    , offChainVoteReferenceHashAlgorithm = Decode.column (Decode.nullable Decode.text)
    }

offChainVoteReferenceEncoder :: OffChainVoteReference -> Encode.Params
offChainVoteReferenceEncoder offChainVoteReference =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteReferenceId $ offChainVoteReferenceId offChainVoteReference)
    , Encode.param (getOffChainVoteDataId $ offChainVoteReferenceOffChainVoteDataId offChainVoteReference)
    , Encode.param (offChainVoteReferenceLabel offChainVoteReference)
    , Encode.param (offChainVoteReferenceUri offChainVoteReference)
    , Encode.param (offChainVoteReferenceHashDigest offChainVoteReference)
    , Encode.param (offChainVoteReferenceHashAlgorithm offChainVoteReference)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateId :: !OffChainVoteExternalUpdateId
  , offChainVoteExternalUpdateOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteExternalUpdateTitle :: !Text
  , offChainVoteExternalUpdateUri :: !Text
  } deriving (Eq, Show, Generic)

newtype OffChainVoteExternalUpdateId = OffChainVoteExternalUpdateId { getOffChainVoteExternalUpdateId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorId :: !OffChainVoteFetchErrorId
  , offChainVoteFetchErrorVotingAnchorId :: !VotingAnchorId        -- noreference
  , offChainVoteFetchErrorFetchError :: !Text
  , offChainVoteFetchErrorFetchTime :: !UTCTime                    -- sqltype=timestamp
  , offChainVoteFetchErrorRetryCount :: !Word                      -- sqltype=word31type
  } deriving (Eq, Show, Generic)

newtype OffChainVoteFetchErrorId = OffChainVoteFetchErrorId { getOffChainVoteFetchErrorId :: Int64 }
  deriving (Eq, Show, Ord)

offChainVoteExternalUpdateDecoder :: Decode.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder =
  OffChainVoteExternalUpdate
    { offChainVoteExternalUpdateId = OffChainVoteExternalUpdateId <$> Decode.column Decode.int8
    , offChainVoteExternalUpdateOffChainVoteDataId = OffChainVoteDataId <$> Decode.column Decode.int8
    , offChainVoteExternalUpdateTitle = Decode.column Decode.text
    , offChainVoteExternalUpdateUri = Decode.column Decode.text
    }

offChainVoteExternalUpdateEncoder :: OffChainVoteExternalUpdate -> Encode.Params
offChainVoteExternalUpdateEncoder offChainVoteExternalUpdate =
  Encode.params $ mconcat
    [ Encode.param (getOffChainVoteExternalUpdateId $ offChainVoteExternalUpdateId offChainVoteExternalUpdate)
    , Encode.param (getOffChainVoteDataId $ offChainVoteExternalUpdateOffChainVoteDataId offChainVoteExternalUpdate)
    , Encode.param (offChainVoteExternalUpdateTitle offChainVoteExternalUpdate)
    , Encode.param (offChainVoteExternalUpdateUri offChainVoteExternalUpdate)
    ]


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

newtype PoolHashId = PoolHashId { getPoolHashId :: Int64 }
  deriving (Eq, Show, Ord)

poolHashDecoder :: Decode.Row PoolHash
poolHashDecoder =
  PoolHash
    { poolHashId = PoolHashId <$> Decode.column Decode.int8
    , poolHashHashRaw = Decode.column Decode.bytea
    , poolHashView = Decode.column Decode.text
    }

poolHashEncoder :: PoolHash -> Encode.Params
poolHashEncoder poolHash =
  Encode.params $ mconcat
    [ Encode.param (getPoolHashId $ poolHashId poolHash) -- poolHashId
    , Encode.param (poolHashHashRaw poolHash) -- poolHashHashRaw
    , Encode.param (poolHashView poolHash) -- poolHashView
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

newtype PoolStatId = PoolStatId { getPoolStatId :: Int64 }
  deriving (Eq, Show, Ord)

poolStatDecoder :: Decode.Row PoolStat
poolStatDecoder =
  PoolStat
    { poolStatId = PoolStatId <$> Decode.column Decode.int8
    , poolStatPoolHashId = PoolHashId <$> Decode.column Decode.int8
    , poolStatEpochNo = Decode.column Decode.int8
    , poolStatNumberOfBlocks = DbWord64 <$> Decode.column Decode.int8
    , poolStatNumberOfDelegators = DbWord64 <$> Decode.column Decode.int8
    , poolStatStake = DbWord64 <$> Decode.column Decode.int8
    , poolStatVotingPower = fmap DbWord64 <$> Decode.column (Decode.nullable Decode.int8)
    }

poolStatEncoder :: PoolStat -> Encode.Params
poolStatEncoder poolStat =
  Encode.params $ mconcat
    [ Encode.param (getPoolStatId $ poolStatId poolStat) -- poolStatId
    , Encode.param (getPoolHashId $ poolStatPoolHashId poolStat) -- poolStatPoolHashId
    , Encode.param (poolStatEpochNo poolStat) -- poolStatEpochNo
    , Encode.param (getDbWord64 $ poolStatNumberOfBlocks poolStat) -- poolStatNumberOfBlocks
    , Encode.param (getDbWord64 $ poolStatNumberOfDelegators poolStat) -- poolStatNumberOfDelegators
    , Encode.param (getDbWord64 $ poolStatStake poolStat) -- poolStatStake
    , Encode.param (getDbWord64 <$> poolStatVotingPower poolStat) -- poolStatVotingPower
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

newtype PoolUpdateId = PoolUpdateId { getPoolUpdateId :: Int64 }
  deriving (Eq, Show, Ord)

poolUpdateDecoder :: Decode.Row PoolUpdate
poolUpdateDecoder =
  PoolUpdate
    { poolUpdateId = PoolUpdateId <$> Decode.column Decode.int8
    , poolUpdateHashId = PoolHashId <$> Decode.column Decode.int8
    , poolUpdateCertIndex = Decode.column Decode.int2
    , poolUpdateVrfKeyHash = Decode.column Decode.bytea
    , poolUpdatePledge = DbLovelace <$> Decode.column Decode.int8
    , poolUpdateRewardAddrId = StakeAddressId <$> Decode.column Decode.int8
    , poolUpdateActiveEpochNo = Decode.column Decode.int8
    , poolUpdateMetaId = fmap PoolMetadataRefId <$> Decode.column (Decode.nullable Decode.int8)
    , poolUpdateMargin = Decode.column Decode.float8
    , poolUpdateFixedCost = DbLovelace <$> Decode.column Decode.int8
    , poolUpdateDeposit = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , poolUpdateRegisteredTxId = TxId <$> Decode.column Decode.int8
    }

poolUpdateEncoder :: PoolUpdate -> Encode.Params
poolUpdateEncoder poolUpdate =
  Encode.params $ mconcat
    [ Encode.param (getPoolUpdateId $ poolUpdateId poolUpdate) -- poolUpdateId
    , Encode.param (getPoolHashId $ poolUpdateHashId poolUpdate) -- poolUpdateHashId
    , Encode.param (poolUpdateCertIndex poolUpdate) -- poolUpdateCertIndex
    , Encode.param (poolUpdateVrfKeyHash poolUpdate) -- poolUpdateVrfKeyHash
    , Encode.param (getDbLovelace $ poolUpdatePledge poolUpdate) -- poolUpdatePledge
    , Encode.param (getStakeAddressId $ poolUpdateRewardAddrId poolUpdate) -- poolUpdateRewardAddrId
    , Encode.param (poolUpdateActiveEpochNo poolUpdate) -- poolUpdateActiveEpochNo
    , Encode.param (getPoolMetadataRefId <$> poolUpdateMetaId poolUpdate) -- poolUpdateMetaId
    , Encode.param (poolUpdateMargin poolUpdate) -- poolUpdateMargin
    , Encode.param (getDbLovelace $ poolUpdateFixedCost poolUpdate) -- poolUpdateFixedCost
    , Encode.param (getDbLovelace <$> poolUpdateDeposit poolUpdate) -- poolUpdateDeposit
    , Encode.param (getTxId $ poolUpdateRegisteredTxId poolUpdate) -- poolUpdateRegisteredTxId
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

newtype PoolMetadataRefId = PoolMetadataRefId { getPoolMetadataRefId :: Int64 }
  deriving (Eq, Show, Ord)

poolMetadataRefEncoder :: PoolMetadataRef -> Encode.Params
poolMetadataRefEncoder poolMetadataRef =
  Encode.params $ mconcat
    [ Encode.param (getPoolMetadataRefId $ poolMetadataRefId poolMetadataRef) -- poolMetadataRefId
    , Encode.param (getPoolHashId $ poolMetadataRefPoolId poolMetadataRef) -- poolMetadataRefPoolId
    , Encode.param (getPoolUrl $ poolMetadataRefUrl poolMetadataRef) -- poolMetadataRefUrl
    , Encode.param (poolMetadataRefHash poolMetadataRef) -- poolMetadataRefHash
    , Encode.param (getTxId $ poolMetadataRefRegisteredTxId poolMetadataRef) -- poolMetadataRefRegisteredTxId
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

newtype PoolOwnerId = PoolOwnerId { getPoolOwnerId :: Int64 }
  deriving (Eq, Show, Ord)

poolOwnerDecoder :: Decode.Row PoolOwner
poolOwnerDecoder =
  PoolOwner
    { poolOwnerId = PoolOwnerId <$> Decode.column Decode.int8
    , poolOwnerAddrId = StakeAddressId <$> Decode.column Decode.int8
    , poolOwnerPoolUpdateId = PoolUpdateId <$> Decode.column Decode.int8
    }

poolOwnerEncoder :: PoolOwner -> Encode.Params
poolOwnerEncoder poolOwner =
  Encode.params $ mconcat
    [ Encode.param (getPoolOwnerId . poolOwnerId) Encode.int8 -- poolOwnerId
    , Encode.param (getStakeAddressId . poolOwnerAddrId) Encode.int8 -- poolOwnerAddrId
    , Encode.param (getPoolUpdateId . poolOwnerPoolUpdateId) Encode.int8 -- poolOwnerPoolUpdateId
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

newtype PoolRetireId = PoolRetireId { getPoolRetireId :: Int64 }
  deriving (Eq, Show, Ord)

poolRetireDecoder :: Decode.Row PoolRetire
poolRetireDecoder =
  PoolRetire
    { poolRetireId = PoolRetireId <$> Decode.column Decode.int8
    , poolRetireHashId = PoolHashId <$> Decode.column Decode.int8
    , poolRetireCertIndex = Decode.column Decode.int2
    , poolRetireAnnouncedTxId = TxId <$> Decode.column Decode.int8
    , poolRetireRetiringEpoch = Decode.column Decode.int8
    }

poolRetireEncoder :: PoolRetire -> Encode.Params
poolRetireEncoder poolRetire =
  Encode.params $ mconcat
    [ Encode.param (getPoolRetireId $ poolRetireId poolRetire) -- poolRetireId
    , Encode.param (getPoolHashId $ poolRetireHashId poolRetire) -- poolRetireHashId
    , Encode.param (poolRetireCertIndex poolRetire) -- poolRetireCertIndex
    , Encode.param (getTxId $ poolRetireAnnouncedTxId poolRetire) -- poolRetireAnnouncedTxId
    , Encode.param (poolRetireRetiringEpoch poolRetire) -- poolRetireRetiringEpoch
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

newtype PoolRelayId = PoolRelayId { getPoolRelayId :: Int64 }
  deriving (Eq, Show, Ord)

poolRelayDecoder :: Decode.Row PoolRelay
poolRelayDecoder =
  PoolRelay
    { poolRelayId = PoolRelayId <$> Decode.column Decode.int8
    , poolRelayUpdateId = PoolUpdateId <$> Decode.column Decode.int8
    , poolRelayIpv4 = Decode.column (Decode.nullable Decode.text)
    , poolRelayIpv6 = Decode.column (Decode.nullable Decode.text)
    , poolRelayDnsName = Decode.column (Decode.nullable Decode.text)
    , poolRelayDnsSrvName = Decode.column (Decode.nullable Decode.text)
    , poolRelayPort = Decode.column (Decode.nullable Decode.int2)
    }

poolRelayEncoder :: PoolRelay -> Encode.Params
poolRelayEncoder poolRelay =
  Encode.params $ mconcat
    [ Encode.param (getPoolRelayId $ poolRelayId poolRelay) -- poolRelayId
    , Encode.param (getPoolUpdateId $ poolRelayUpdateId poolRelay) -- poolRelayUpdateId
    , Encode.param (poolRelayIpv4 poolRelay) -- poolRelayIpv4
    , Encode.param (poolRelayIpv6 poolRelay) -- poolRelayIpv6
    , Encode.param (poolRelayDnsName poolRelay) -- poolRelayDnsName
    , Encode.param (poolRelayDnsSrvName poolRelay) -- poolRelayDnsSrvName
    , Encode.param (poolRelayPort poolRelay) -- poolRelayPort
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

newtype DelistedPoolId = DelistedPoolId { getDelistedPoolId :: Int64 }
  deriving (Eq, Show, Ord)

delistedPoolDecoder :: Decode.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    { delistedPoolId = DelistedPoolId <$> Decode.column Decode.int8
    , delistedPoolHashRaw = Decode.column Decode.bytea
    }

delistedPoolEncoder :: DelistedPool -> Encode.Params
delistedPoolEncoder delistedPool =
  Encode.params $
       contramap (getDelistedPoolId . delistedPoolId) Encode.int8
    <> contramap delistedPoolHashRaw Encode.bytea
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

newtype ReservedPoolTickerId = ReservedPoolTickerId { getReservedPoolTickerId :: Int64 }
  deriving (Eq, Show, Ord)

delistedPoolDecoder :: Decode.Row DelistedPool
delistedPoolDecoder =
  DelistedPool
    { delistedPoolId = DelistedPoolId <$> Decode.column Decode.int8
    , delistedPoolHashRaw = Decode.column Decode.bytea
    }

delistedPoolEncoder :: DelistedPool -> Encode.Params
delistedPoolEncoder delistedPool =
  Encode.params $ mconcat
    [ Encode.param (getDelistedPoolId $ delistedPoolId delistedPool)
    , Encode.param (delistedPoolHashRaw delistedPool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation, and reward
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_address
Description: Contains information about stakeholder addresses.
-}
data StakeAddress = StakeAddress  -- Can be an address of a script hash
  { stakeAddressId :: !StakeAddressId -- noreference
    , stakeAddressHashRaw :: !ByteString        -- sqltype=addr29type
  , stakeAddressView :: !Text
  , stakeAddressScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  } deriving (Show, Eq, Generic)

newtype StakeAddressId = StakeAddressId { getStakeAddressId :: Int64 }
  deriving (Eq, Show, Ord)

stakeAddressDecoder :: Decode.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    { stakeAddressId = StakeAddressId <$> Decode.column Decode.int8
    , stakeAddressHashRaw = Decode.column Decode.bytea
    , stakeAddressView = Decode.column Decode.text
    , stakeAddressScriptHash = Decode.column (Decode.nullable Decode.bytea)
    }

stakeAddressEncoder :: StakeAddress -> Encode.Params
stakeAddressEncoder stakeAddress =
  Encode.params $ mconcat
    [ Encode.param (getStakeAddressId $ stakeAddressId stakeAddress)
    , Encode.param (stakeAddressHashRaw stakeAddress)
    , Encode.param (stakeAddressView stakeAddress)
    , Encode.param (stakeAddressScriptHash stakeAddress)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_registration
Description: Contains information about stakeholder registrations.
-}
data StakeRegistration = StakeRegistration
  { stakeRegistrationId :: !StakeRegistrationId
  , stakeRegistrationAddrId :: !StakeAddressId  -- noreference
  , stakeRegistrationCertIndex :: !Word16
  , stakeRegistrationEpochNo :: !Word64         -- sqltype=word31type
  , stakeRegistrationDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , stakeRegistrationTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

newtype StakeRegistrationId = StakeRegistrationId { getStakeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

stakeRegistrationDecoder :: Decode.Row StakeRegistration
stakeRegistrationDecoder =
  StakeRegistration
    { stakeRegistrationId = StakeRegistrationId <$> Decode.column Decode.int8
    , stakeRegistrationAddrId = StakeAddressId <$> Decode.column Decode.int8
    , stakeRegistrationCertIndex = fromIntegral <$> Decode.column Decode.int2
    , stakeRegistrationEpochNo = fromIntegral <$> Decode.column Decode.int8
    , stakeRegistrationDeposit = fmap DbLovelace <$> Decode.column (Decode.nullable Decode.int8)
    , stakeRegistrationTxId = TxId <$> Decode.column Decode.int8
    }

stakeRegistrationEncoder :: StakeRegistration -> Encode.Params
stakeRegistrationEncoder stakeRegistration =
  Encode.params $ mconcat
    [ Encode.param (getStakeRegistrationId $ stakeRegistrationId stakeRegistration)
    , Encode.param (getStakeAddressId $ stakeRegistrationAddrId stakeRegistration)
    , Encode.param (fromIntegral $ stakeRegistrationCertIndex stakeRegistration)
    , Encode.param (fromIntegral $ stakeRegistrationEpochNo stakeRegistration)
    , Encode.param (getDbLovelace <$> stakeRegistrationDeposit stakeRegistration)
    , Encode.param (getTxId $ stakeRegistrationTxId stakeRegistration)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_deregistration
Description: Contains information about stakeholder deregistrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data StakeDeregistration = StakeDeregistration
  { stakeDeregistrationId :: !StakeDeregistrationId
  , stakeDeregistrationAddrId :: !StakeAddressId -- noreference
  , stakeDeregistrationCertIndex :: !Word16
  , stakeDeregistrationEpochNo :: !Word64       -- sqltype=word31type
  , stakeDeregistrationTxId :: !TxId            -- noreference
  , stakeDeregistrationRedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

newtype StakeDeregistrationId = StakeDeregistrationId { getStakeDeregistrationId :: Int64 }
  deriving (Eq, Show, Ord)

stakeDeregistrationDecoder :: Decode.Row StakeDeregistration
stakeDeregistrationDecoder =
  StakeDeregistration
    { stakeDeregistrationId = StakeDeregistrationId <$> Decode.column Decode.int8
    , stakeDeregistrationAddrId = StakeAddressId <$> Decode.column Decode.int8
    , stakeDeregistrationCertIndex = fromIntegral <$> Decode.column Decode.int2
    , stakeDeregistrationEpochNo = fromIntegral <$> Decode.column Decode.int8
    , stakeDeregistrationTxId = TxId <$> Decode.column Decode.int8
    , stakeDeregistrationRedeemerId = fmap RedeemerId <$> Decode.column (Decode.nullable Decode.int8)
    }

stakeDeregistrationEncoder :: StakeDeregistration -> Encode.Params
stakeDeregistrationEncoder stakeDeregistration =
  Encode.params $ mconcat
    [ Encode.param (getStakeDeregistrationId $ stakeDeregistrationId stakeDeregistration)
    , Encode.param (getStakeAddressId $ stakeDeregistrationAddrId stakeDeregistration)
    , Encode.param (fromIntegral $ stakeDeregistrationCertIndex stakeDeregistration)
    , Encode.param (fromIntegral $ stakeDeregistrationEpochNo stakeDeregistration)
    , Encode.param (getTxId $ stakeDeregistrationTxId stakeDeregistration)
    , Encode.param (getRedeemerId <$> stakeDeregistrationRedeemerId stakeDeregistration)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delegation
Description:Contains information about stakeholder delegations, including the stakeholder's address and the pool to which they are delegating.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Delegation = Delegation
  { delegationId :: !DelegationId
  , delegationAddrId :: !StakeAddressId         -- noreference
  , delegationCertIndex :: !Word16
  , delegationPoolHashId :: !PoolHashId         -- noreference
  , delegationActiveEpochNo :: !Word64
  , delegationTxId :: !TxId                     -- noreference
  , delegationSlotNo :: !Word64                 -- sqltype=word63type
  , delegationRedeemerId :: !(Maybe RedeemerId)   -- noreference
  } deriving (Eq, Show, Generic)

newtype DelegationId = DelegationId { getDelegationId :: Int64 }
  deriving (Eq, Show, Ord)

delegationDecoder :: Decode.Row Delegation
delegationDecoder =
  Delegation
    { delegationId = DelegationId <$> Decode.column Decode.int8
    , delegationAddrId = StakeAddressId <$> Decode.column Decode.int8
    , delegationCertIndex = fromIntegral <$> Decode.column Decode.int2
    , delegationPoolHashId = PoolHashId <$> Decode.column Decode.int8
    , delegationActiveEpochNo = fromIntegral <$> Decode.column Decode.int8
    , delegationTxId = TxId <$> Decode.column Decode.int8
    , delegationSlotNo = fromIntegral <$> Decode.column Decode.int8
    , delegationRedeemerId = fmap RedeemerId <$> Decode.column (Decode.nullable Decode.int8)
    }

delegationEncoder :: Delegation -> Encode.Params
delegationEncoder delegation =
  Encode.params $
       contramap (getDelegationId . delegationId) Encode.int8 -- delegationId
    <> contramap (getStakeAddressId . delegationAddrId) Encode.int8 -- delegationAddrId
    <> contramap (fromIntegral . delegationCertIndex) Encode.int2 -- delegationCertIndex
    <> contramap (getPoolHashId . delegationPoolHashId) Encode.int8 -- delegationPoolHashId
    <> contramap (fromIntegral . delegationActiveEpochNo) Encode.int8 -- delegationActiveEpochNo
    <> contramap (getTxId . delegationTxId) Encode.int8 -- delegationTxId
    <> contramap (fromIntegral . delegationSlotNo) Encode.int8 -- delegationSlotNo
    <> contramap (fmap getRedeemerId . delegationRedeemerId) (Encode.nullable Encode.int8) -- delegationRedeemerId

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description: Reward, Stake and Treasury need to be obtained from the ledger state.
  The reward for each stake address and. This is not a balance, but a reward amount and the
  epoch in which the reward was earned.
  This table should never get rolled back.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Reward = Reward
  { rewardId :: !RewardId
  , rewardAddrId :: !StakeAddressId   -- noreference
  , rewardType :: !RewardSource       -- sqltype=rewardtype
  , rewardAmount :: !DbLovelace       -- sqltype=lovelace
  , rewardEarnedEpoch :: !Word64      -- generated="((CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED)"
  , rewardSpendableEpoch :: !Word64
  , rewardPoolId :: !PoolHashId       -- noreference
  } deriving (Show, Eq, Generic)

newtype RewardId = RewardId { getRewardId :: Int64 }
  deriving (Eq, Show, Ord)

rewardDecoder :: Decode.Row Reward
rewardDecoder =
  Reward
    { rewardId = RewardId <$> Decode.column Decode.int8
    , rewardAddrId = StakeAddressId <$> Decode.column Decode.int8
    , rewardType = Decode.column (Decode.enum rewardSourceFromText)
    , rewardAmount = DbLovelace <$> Decode.column Decode.int8
    , rewardEarnedEpoch = fromIntegral <$> Decode.column Decode.int8
    , rewardSpendableEpoch = fromIntegral <$> Decode.column Decode.int8
    , rewardPoolId = PoolHashId <$> Decode.column Decode.int8
    }

rewardEncoder :: Reward -> Encode.Params
rewardEncoder reward =
  Encode.params $ mconcat
    [ Encode.param (getRewardId $ rewardId reward)
    , Encode.param (getStakeAddressId $ rewardAddrId reward)
    , Encode.param (rewardSourceToText $ rewardType reward)
    , Encode.param (getDbLovelace $ rewardAmount reward)
    , Encode.param (fromIntegral $ rewardEarnedEpoch reward)
    , Encode.param (fromIntegral $ rewardSpendableEpoch reward)
    , Encode.param (getPoolHashId $ rewardPoolId reward)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reward_rest
Description: Contains information about the remaining reward for each stakeholder.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data RewardRest = RewardRest
  { rewardRestId :: !RewardRestId
  , rewardRestAddrId :: !StakeAddressId -- noreference
  , rewardRestType :: !RewardSource     -- sqltype=rewardtype
  , rewardRestAmount :: !DbLovelace     -- sqltype=lovelace
  , rewardRestEarnedEpoch :: !Word64    -- generated="(CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)"
  , rewardRestSpendableEpoch :: !Word64
  } deriving (Show, Eq, Generic)

newtype RewardRestId = RewardRestId { getRewardRestId :: Int64 }
  deriving (Eq, Show, Ord)

rewardRestDecoder :: Decode.Row RewardRest
rewardRestDecoder =
  RewardRest
    { rewardRestId = RewardRestId <$> Decode.column Decode.int8
    , rewardRestAddrId = StakeAddressId <$> Decode.column Decode.int8
    , rewardRestType = Decode.column (Decode.enum rewardSourceFromText)
    , rewardRestAmount = DbLovelace <$> Decode.column Decode.int8
    , rewardRestEarnedEpoch = fromIntegral <$> Decode.column Decode.int8
    , rewardRestSpendableEpoch = fromIntegral <$> Decode.column Decode.int8
    }

rewardRestEncoder :: RewardRest -> Encode.Params
rewardRestEncoder rewardRest =
  Encode.params $ mconcat
    [ Encode.param (getRewardRestId $ rewardRestId rewardRest)
    , Encode.param (getStakeAddressId $ rewardRestAddrId rewardRest)
    , Encode.param (rewardSourceToText $ rewardRestType rewardRest)
    , Encode.param (getDbLovelace $ rewardRestAmount rewardRest)
    , Encode.param (fromIntegral $ rewardRestEarnedEpoch rewardRest)
    , Encode.param (fromIntegral $ rewardRestSpendableEpoch rewardRest)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake
Description: Contains information about the stake of each stakeholder in each epoch.
  This table should never get rolled back
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStake = EpochStake
  { epochStakeId :: !EpochStakeId
  , epochStakeAddrId :: !StakeAddressId -- noreference
  , epochStakePoolId :: !PoolHashId     -- noreference
  , epochStakeAmount :: !DbLovelace     -- sqltype=lovelace
  , epochStakeEpochNo :: !Word64        -- sqltype=word31type
  } deriving (Show, Eq, Generic)
-- similar scenario as in Reward the constraint that was here is now set manually in
-- `applyAndInsertBlockMaybe` at a more optimal time.

newtype EpochStakeId = EpochStakeId { getEpochStakeId :: Int64 }
  deriving (Eq, Show, Ord)

epochStakeDecoder :: Decode.Row EpochStake
epochStakeDecoder =
  EpochStake
    { epochStakeId = EpochStakeId <$> Decode.column Decode.int8
    , epochStakeAddrId = StakeAddressId <$> Decode.column Decode.int8
    , epochStakePoolId = PoolHashId <$> Decode.column Decode.int8
    , epochStakeAmount = DbLovelace <$> Decode.column Decode.int8
    , epochStakeEpochNo = fromIntegral <$> Decode.column Decode.int8
    }

epochStakeEncoder :: EpochStake -> Encode.Params
epochStakeEncoder epochStake =
  Encode.params $ mconcat
    [ Encode.param (getEpochStakeId $ epochStakeId epochStake)
    , Encode.param (getStakeAddressId $ epochStakeAddrId epochStake)
    , Encode.param (getPoolHashId $ epochStakePoolId epochStake)
    , Encode.param (getDbLovelace $ epochStakeAmount epochStake)
    , Encode.param (fromIntegral $ epochStakeEpochNo epochStake)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake_progress
Description: Contains information about the progress of the epoch stake calculation.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStakeProgress = EpochStakeProgress
  { epochStakeProgressId :: !EpochStakeProgressId
  , epochStakeProgressEpochNo :: !Word64  -- sqltype=word31type
  , epochStakeProgressCompleted :: !Bool
  -- UniqueEpochStakeProgress epochNo
  } deriving (Show, Eq, Generic)

newtype EpochStakeProgressId = EpochStakeProgressId { getEpochStakeProgressId :: Int64 }
  deriving (Eq, Show, Ord)

epochStakeProgressDecoder :: Decode.Row EpochStakeProgress
epochStakeProgressDecoder =
  EpochStakeProgress
    { epochStakeProgressId = EpochStakeProgressId <$> Decode.column Decode.int8
    , epochStakeProgressEpochNo = fromIntegral <$> Decode.column Decode.int8
    , epochStakeProgressCompleted = Decode.column Decode.bool
    }

epochStakeProgressEncoder :: EpochStakeProgress -> Encode.Params
epochStakeProgressEncoder epochStakeProgress =
  Encode.params $ mconcat
    [ Encode.param (getEpochStakeProgressId $ epochStakeProgressId epochStakeProgress)
    , Encode.param (fromIntegral $ epochStakeProgressEpochNo epochStakeProgress)
    , Encode.param (epochStakeProgressCompleted epochStakeProgress)
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

schemaVersionDecoder :: Decode.Row SchemaVersion
schemaVersionDecoder =
  SchemaVersion
    { schemaVersionStageOne = Decode.column Decode.int4
    , schemaVersionStageTwo = Decode.column Decode.int4
    , schemaVersionStageThree = Decode.column Decode.int4
    }

schemaVersionEncoder :: SchemaVersion -> Encode.Params
schemaVersionEncoder schemaVersion =
  Encode.params $ mconcat
    [ Encode.param (schemaVersionStageOne schemaVersion)
    , Encode.param (schemaVersionStageTwo schemaVersion)
    , Encode.param (schemaVersionStageThree schemaVersion)
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

newtype MetaId = MetaId { getMetaId :: Int64 }
  deriving (Eq, Show, Ord)

metaDecoder :: Decode.Row Meta
metaDecoder =
  Meta
    { metaId = MetaId <$> Decode.column Decode.int8
    , metaStartTime = Decode.column Decode.timestamptz
    , metaNetworkName = Decode.column Decode.text
    , metaVersion = Decode.column Decode.text
    }

metaEncoder :: Meta -> Encode.Params
metaEncoder meta =
  Encode.params $ mconcat
    [ Encode.param (getMetaId $ metaId meta)
    , Encode.param (metaStartTime meta)
    , Encode.param (metaNetworkName meta)
    , Encode.param (metaVersion meta)
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

newtype ExtraMigrationsId = ExtraMigrationsId { getExtraMigrationsId :: Int64 }
  deriving (Eq, Show, Ord)

extraMigrationsDecoder :: Decode.Row ExtraMigrations
extraMigrationsDecoder =
  ExtraMigrations
    { extraMigrationsId = ExtraMigrationsId <$> Decode.column Decode.int8
    , extraMigrationsToken = Decode.column Decode.text
    , extraMigrationsDescription = Decode.column (Decode.nullable Decode.text)
    }

extraMigrationsEncoder :: ExtraMigrations -> Encode.Params
extraMigrationsEncoder extraMigrations =
  Encode.params $ mconcat
    [ Encode.param (getExtraMigrationsId $ extraMigrationsId extraMigrations)
    , Encode.param (extraMigrationsToken extraMigrations)
    , Encode.param (extraMigrationsDescription extraMigrations)
    ]

-- deriving instance Eq (Unique EpochSyncTime)

-- schemaDocs :: ![EntityDef]
-- schemaDocs =
--   document entityDefs $ do
--     SchemaVersion --^ do
--       "The version of the database schema. Schema versioning is split into three stages as detailed\
--       \ below. This table should only ever have a single row."
--       SchemaVersionStageOne # "Set up PostgreSQL data types (using SQL 'DOMAIN' statements)."
--       SchemaVersionStageTwo # "Persistent generated migrations."
--       SchemaVersionStageThree # "Set up database views, indices etc."

--     PoolHash --^ do
--       "A table for every unique pool key hash.\
--       \ The existance of an entry doesn't mean the pool is registered or in fact that is was ever registered."
--       PoolHashHashRaw # "The raw bytes of the pool hash."
--       PoolHashView # "The Bech32 encoding of the pool hash."

--     SlotLeader --^ do
--       "Every unique slot leader (ie an entity that mines a block). It could be a pool or a leader defined in genesis."
--       SlotLeaderHash # "The hash of of the block producer identifier."
--       SlotLeaderPoolHashId # "If the slot leader is a pool, an index into the `PoolHash` table."
--       SlotLeaderDescription # "An auto-generated description of the slot leader."

--     Block --^ do
--       "A table for blocks on the chain."
--       BlockHash # "The hash identifier of the block."
--       BlockEpochNo # "The epoch number."
--       BlockSlotNo # "The slot number."
--       BlockEpochSlotNo # "The slot number within an epoch (resets to zero at the start of each epoch)."
--       BlockBlockNo # "The block number."
--       BlockPreviousId # "The Block table index of the previous block."
--       BlockSlotLeaderId # "The SlotLeader table index of the creator of this block."
--       BlockSize # "The block size (in bytes). Note, this size value is not expected to be the same as the sum of the tx sizes due to the fact that txs being stored in segwit format and oddities in the CBOR encoding."
--       BlockTime # "The block time (UTCTime)."
--       BlockTxCount # "The number of transactions in this block."
--       BlockProtoMajor # "The block's major protocol number."
--       BlockProtoMinor # "The block's major protocol number."
--       -- Shelley specific
--       BlockVrfKey # "The VRF key of the creator of this block."
--       BlockOpCert # "The hash of the operational certificate of the block producer."
--       BlockOpCertCounter # "The value of the counter used to produce the operational certificate."

--     Tx --^ do
--       "A table for transactions within a block on the chain."
--       TxHash # "The hash identifier of the transaction."
--       TxBlockId # "The Block table index of the block that contains this transaction."
--       TxBlockIndex # "The index of this transaction with the block (zero based)."
--       TxOutSum # "The sum of the transaction outputs (in Lovelace)."
--       TxFee # "The fees paid for this transaction."
--       TxDeposit # "Deposit (or deposit refund) in this transaction. Deposits are positive, refunds negative."
--       TxSize # "The size of the transaction in bytes."
--       TxInvalidBefore # "Transaction in invalid before this slot number."
--       TxInvalidHereafter # "Transaction in invalid at or after this slot number."
--       TxValidContract # "False if the contract is invalid. True if the contract is valid or there is no contract."
--       TxScriptSize # "The sum of the script sizes (in bytes) of scripts in the transaction."

--     TxCbor --^ do
--       "A table holding raw CBOR encoded transactions."
--       TxCborTxId # "The Tx table index of the transaction encoded in this table."
--       TxCborBytes # "CBOR encoded transaction."

--     ReverseIndex --^ do
--       "A table for reverse indexes for the minimum input output and multi asset output related with\
--       \ this block. New in v13.1"
--       ReverseIndexBlockId # "The Block table index related with these indexes"
--       ReverseIndexMinIds # "The Reverse indexes associated with this block, as Text separated by :"

--     StakeAddress --^ do
--       "A table of unique stake addresses. Can be an actual address or a script hash. \
--       \ The existance of an entry doesn't mean the address is registered or in fact that is was ever registered."
--       StakeAddressHashRaw # "The raw bytes of the stake address hash."
--       StakeAddressView # "The Bech32 encoded version of the stake address."
--       StakeAddressScriptHash # "The script hash, in case this address is locked by a script."

--     TxIn --^ do
--       "A table for transaction inputs."
--       TxInTxInId # "The Tx table index of the transaction that contains this transaction input."
--       TxInTxOutId # "The Tx table index of the transaction that contains the referenced transaction output."
--       TxInTxOutIndex # "The index within the transaction outputs."
--       TxInRedeemerId # "The Redeemer table index which is used to validate this input."

--     CollateralTxIn --^ do
--       "A table for transaction collateral inputs."
--       CollateralTxInTxInId # "The Tx table index of the transaction that contains this transaction input"
--       CollateralTxInTxOutId # "The Tx table index of the transaction that contains the referenced transaction output."
--       CollateralTxInTxOutIndex # "The index within the transaction outputs."

--     ReferenceTxIn --^ do
--       "A table for reference transaction inputs. New in v13."
--       ReferenceTxInTxInId # "The Tx table index of the transaction that contains this transaction input"
--       ReferenceTxInTxOutId # "The Tx table index of the transaction that contains the referenced output."
--       ReferenceTxInTxOutIndex # "The index within the transaction outputs."

--     Meta --^ do
--       "A table containing metadata about the chain. There will probably only ever be one row in this table."
--       MetaStartTime # "The start time of the network."
--       MetaNetworkName # "The network name."

--     Epoch --^ do
--       "Aggregation of data within an epoch."
--       EpochOutSum # "The sum of the transaction output values (in Lovelace) in this epoch."
--       EpochFees # "The sum of the fees (in Lovelace) in this epoch."
--       EpochTxCount # "The number of transactions in this epoch."
--       EpochBlkCount # "The number of blocks in this epoch."
--       EpochNo # "The epoch number."
--       EpochStartTime # "The epoch start time."
--       EpochEndTime # "The epoch end time."

--     AdaPots --^ do
--       "A table with all the different types of total balances (Shelley only).\n\
--       \The treasury and rewards fields will be correct for the whole epoch, but all other \
--       \fields change block by block."
--       AdaPotsSlotNo # "The slot number where this AdaPots snapshot was taken."
--       AdaPotsEpochNo # "The epoch number where this AdaPots snapshot was taken."
--       AdaPotsTreasury # "The amount (in Lovelace) in the treasury pot."
--       AdaPotsReserves # "The amount (in Lovelace) in the reserves pot."
--       AdaPotsRewards # "The amount (in Lovelace) in the rewards pot."
--       AdaPotsUtxo # "The amount (in Lovelace) in the UTxO set."
--       AdaPotsDepositsStake # "The amount (in Lovelace) in the obligation pot coming from stake key and pool deposits. Renamed from deposits in 13.3."
--       AdaPotsDepositsDrep # "The amount (in Lovelace) in the obligation pot coming from drep registrations deposits. New in 13.3."
--       AdaPotsDepositsProposal # "The amount (in Lovelace) in the obligation pot coming from governance proposal deposits. New in 13.3."
--       AdaPotsFees # "The amount (in Lovelace) in the fee pot."
--       AdaPotsBlockId # "The Block table index of the block for which this snapshot was taken."

--     PoolMetadataRef --^ do
--       "An on-chain reference to off-chain pool metadata."
--       PoolMetadataRefPoolId # "The PoolHash table index of the pool for this reference."
--       PoolMetadataRefUrl # "The URL for the location of the off-chain data."
--       PoolMetadataRefHash # "The expected hash for the off-chain data."
--       PoolMetadataRefRegisteredTxId # "The Tx table index of the transaction in which provided this metadata reference."

--     PoolUpdate --^ do
--       "An on-chain pool update."
--       PoolUpdateHashId # "The PoolHash table index of the pool this update refers to."
--       PoolUpdateCertIndex # "The index of this pool update within the certificates of this transaction."
--       PoolUpdateVrfKeyHash # "The hash of the pool's VRF key."
--       PoolUpdatePledge # "The amount (in Lovelace) the pool owner pledges to the pool."
--       PoolUpdateRewardAddrId # "The StakeAddress table index of this pool's rewards address. New in v13: Replaced reward_addr."
--       PoolUpdateActiveEpochNo # "The epoch number where this update becomes active."
--       PoolUpdateMetaId # "The PoolMetadataRef table index this pool update refers to."
--       PoolUpdateMargin # "The margin (as a percentage) this pool charges."
--       PoolUpdateFixedCost # "The fixed per epoch fee (in ADA) this pool charges."
--       PoolUpdateDeposit # "The deposit payed for this pool update. Null for reregistrations."
--       PoolUpdateRegisteredTxId # "The Tx table index of the transaction in which provided this pool update."

--     PoolOwner --^ do
--       "A table containing pool owners."
--       PoolOwnerAddrId # "The StakeAddress table index for the pool owner's stake address."
--       PoolOwnerPoolUpdateId # "The PoolUpdate table index for the pool. New in v13."

--     PoolRetire --^ do
--       "A table containing information about pools retiring."
--       PoolRetireHashId # "The PoolHash table index of the pool this retirement refers to."
--       PoolRetireCertIndex # "The index of this pool retirement within the certificates of this transaction."
--       PoolRetireAnnouncedTxId # "The Tx table index of the transaction where this pool retirement was announced."
--       PoolRetireRetiringEpoch # "The epoch where this pool retires."

--     PoolRelay --^ do
--       PoolRelayUpdateId # "The PoolUpdate table index this PoolRelay entry refers to."
--       PoolRelayIpv4 # "The IPv4 address of the relay (NULLable)."
--       PoolRelayIpv6 # "The IPv6 address of the relay (NULLable)."
--       PoolRelayDnsName # "The DNS name of the relay (NULLable)."
--       PoolRelayDnsSrvName # "The DNS service name of the relay (NULLable)."
--       PoolRelayPort # "The port number of relay (NULLable)."

--     StakeRegistration --^ do
--       "A table containing stake address registrations."
--       StakeRegistrationAddrId # "The StakeAddress table index for the stake address."
--       StakeRegistrationCertIndex # "The index of this stake registration within the certificates of this transaction."
--       StakeRegistrationEpochNo # "The epoch in which the registration took place."
--       StakeRegistrationTxId # "The Tx table index of the transaction where this stake address was registered."

--     StakeDeregistration --^ do
--       "A table containing stake address deregistrations."
--       StakeDeregistrationAddrId # "The StakeAddress table index for the stake address."
--       StakeDeregistrationCertIndex # "The index of this stake deregistration within the certificates of this transaction."
--       StakeDeregistrationEpochNo # "The epoch in which the deregistration took place."
--       StakeDeregistrationTxId # "The Tx table index of the transaction where this stake address was deregistered."
--       StakeDeregistrationRedeemerId # "The Redeemer table index that is related with this certificate."

--     Delegation --^ do
--       "A table containing delegations from a stake address to a stake pool."
--       DelegationAddrId # "The StakeAddress table index for the stake address."
--       DelegationCertIndex # "The index of this delegation within the certificates of this transaction."
--       DelegationPoolHashId # "The PoolHash table index for the pool being delegated to."
--       DelegationActiveEpochNo # "The epoch number where this delegation becomes active."
--       DelegationTxId # "The Tx table index of the transaction that contained this delegation."
--       DelegationSlotNo # "The slot number of the block that contained this delegation."
--       DelegationRedeemerId # "The Redeemer table index that is related with this certificate."

--     TxMetadata --^ do
--       "A table for metadata attached to a transaction."
--       TxMetadataKey # "The metadata key (a Word64/unsigned 64 bit number)."
--       TxMetadataJson # "The JSON payload if it can be decoded as JSON."
--       TxMetadataBytes # "The raw bytes of the payload."
--       TxMetadataTxId # "The Tx table index of the transaction where this metadata was included."

--     Reward --^ do
--       "A table for earned staking rewards. After 13.2 release it includes only 3 types of rewards: member, leader and refund, \
--       \ since the other 2 types have moved to a separate table instant_reward.\
--       \ The rewards are inserted incrementally and\
--       \ this procedure is finalised when the spendable epoch comes. Before the epoch comes, some entries\
--       \ may be missing. The `reward.id` field has been removed and it only appears on docs due to a bug."
--       RewardAddrId # "The StakeAddress table index for the stake address that earned the reward."
--       RewardType # "The type of the rewards"
--       RewardAmount # "The reward amount (in Lovelace)."
--       RewardEarnedEpoch
--         # "The epoch in which the reward was earned. For `pool` and `leader` rewards spendable in epoch `N`, this will be\
--           \ `N - 2`, `refund` N."
--       RewardSpendableEpoch # "The epoch in which the reward is actually distributed and can be spent."
--       RewardPoolId
--         # "The PoolHash table index for the pool the stake address was delegated to when\
--           \ the reward is earned or for the pool that there is a deposit refund."

--     RewardRest --^ do
--       "A table for rewards which are not correlated to a pool. It includes 3 types of rewards: reserves, treasury and proposal_refund.\
--       \ Instant rewards are depredated after Conway.\
--       \ The `reward.id` field has been removed and it only appears on docs due to a bug.\
--       \ New in 13.2"
--       RewardRestAddrId # "The StakeAddress table index for the stake address that earned the reward."
--       RewardRestType # "The type of the rewards."
--       RewardRestAmount # "The reward amount (in Lovelace)."
--       RewardRestEarnedEpoch
--         # "The epoch in which the reward was earned. For rewards spendable in epoch `N`, this will be\
--           \ `N - 1`."
--       RewardRestSpendableEpoch # "The epoch in which the reward is actually distributed and can be spent."

--     Withdrawal --^ do
--       "A table for withdrawals from a reward account."
--       WithdrawalAddrId # "The StakeAddress table index for the stake address for which the withdrawal is for."
--       WithdrawalAmount # "The withdrawal amount (in Lovelace)."
--       WithdrawalTxId # "The Tx table index for the transaction that contains this withdrawal."
--       WithdrawalRedeemerId # "The Redeemer table index that is related with this withdrawal."

--     EpochStake --^ do
--       "A table containing the epoch stake distribution for each epoch. This is inserted incrementally in the first blocks of the previous epoch.\
--       \ The stake distribution is extracted from the `set` snapshot of the ledger. See Shelley specs Sec. 11.2 for more details."
--       EpochStakeAddrId # "The StakeAddress table index for the stake address for this EpochStake entry."
--       EpochStakePoolId # "The PoolHash table index for the pool this entry is delegated to."
--       EpochStakeAmount # "The amount (in Lovelace) being staked."
--       EpochStakeEpochNo # "The epoch number."

--     EpochStakeProgress --^ do
--       "A table which shows when the epoch_stake for an epoch is complete"
--       EpochStakeProgressEpochNo # "The related epoch"
--       EpochStakeProgressCompleted # "True if completed. If not completed the entry won't exist or more rarely be False."

--     Treasury --^ do
--       "A table for payments from the treasury to a StakeAddress. Note: Before protocol version 5.0\
--       \ (Alonzo) if more than one payment was made to a stake address in a single epoch, only the\
--       \ last payment was kept and earlier ones removed. For protocol version 5.0 and later, they\
--       \ are summed and produce a single reward with type `treasury`."
--       TreasuryAddrId # "The StakeAddress table index for the stake address for this Treasury entry."
--       TreasuryCertIndex # "The index of this payment certificate within the certificates of this transaction."
--       TreasuryAmount # "The payment amount (in Lovelace)."
--       TreasuryTxId # "The Tx table index for the transaction that contains this payment."

--     Reserve --^ do
--       "A table for payments from the reserves to a StakeAddress. Note: Before protocol version 5.0\
--       \ (Alonzo) if more than one payment was made to a stake address in a single epoch, only the\
--       \ last payment was kept and earlier ones removed. For protocol version 5.0 and later, they\
--       \ are summed and produce a single reward with type `reserves`"
--       ReserveAddrId # "The StakeAddress table index for the stake address for this Treasury entry."
--       ReserveCertIndex # "The index of this payment certificate within the certificates of this transaction."
--       ReserveAmount # "The payment amount (in Lovelace)."
--       ReserveTxId # "The Tx table index for the transaction that contains this payment."

--     PotTransfer --^ do
--       "A table containing transfers between the reserves pot and the treasury pot."
--       PotTransferCertIndex # "The index of this transfer certificate within the certificates of this transaction."
--       PotTransferTreasury # "The amount (in Lovelace) the treasury balance changes by."
--       PotTransferReserves # "The amount (in Lovelace) the reserves balance changes by."
--       PotTransferTxId # "The Tx table index for the transaction that contains this transfer."

--     EpochSyncTime --^ do
--       "A table containing the time required to fully sync an epoch."
--       EpochSyncTimeNo # "The epoch number for this sync time."
--       EpochSyncTimeSeconds
--         # "The time (in seconds) required to sync this epoch (may be NULL for an epoch\
--           \ that was already partially synced when `db-sync` was started)."
--       EpochSyncTimeState # "The sync state when the sync time is recorded (either 'lagging' or 'following')."

--     MultiAsset --^ do
--       "A table containing all the unique policy/name pairs along with a CIP14 asset fingerprint"
--       MultiAssetPolicy # "The MultiAsset policy hash."
--       MultiAssetName # "The MultiAsset name."
--       MultiAssetFingerprint # "The CIP14 fingerprint for the MultiAsset."

--     MaTxMint --^ do
--       "A table containing Multi-Asset mint events."
--       MaTxMintIdent # "The MultiAsset table index specifying the asset."
--       MaTxMintQuantity # "The amount of the Multi Asset to mint (can be negative to \"burn\" assets)."
--       MaTxMintTxId # "The Tx table index for the transaction that contains this minting event."

--     Redeemer --^ do
--       "A table containing redeemers. A redeemer is provided for all items that are validated by a script."
--       RedeemerTxId # "The Tx table index that contains this redeemer."
--       RedeemerUnitMem # "The budget in Memory to run a script."
--       RedeemerUnitSteps # "The budget in Cpu steps to run a script."
--       RedeemerFee
--         # "The budget in fees to run a script. The fees depend on the ExUnits and the current prices.\
--           \ Is null when --disable-ledger is enabled. New in v13: became nullable."
--       RedeemerPurpose # "What kind pf validation this redeemer is used for. It can be one of 'spend', 'mint', 'cert', 'reward', `voting`, `proposing`"
--       RedeemerIndex # "The index of the redeemer pointer in the transaction."
--       RedeemerScriptHash # "The script hash this redeemer is used for."
--       RedeemerRedeemerDataId # "The data related to this redeemer. New in v13: renamed from datum_id."

--     Script --^ do
--       "A table containing scripts available, found in witnesses, inlined in outputs (reference outputs) or auxdata of transactions."
--       ScriptTxId # "The Tx table index for the transaction where this script first became available."
--       ScriptHash # "The Hash of the Script."
--       ScriptType # "The type of the script. This is currenttly either 'timelock' or 'plutus'."
--       ScriptJson # "JSON representation of the timelock script, null for other script types"
--       ScriptBytes # "CBOR encoded plutus script data, null for other script types"
--       ScriptSerialisedSize # "The size of the CBOR serialised script, if it is a Plutus script."

--     Datum --^ do
--       "A table containing Plutus Datum, found in witnesses or inlined in outputs"
--       DatumHash # "The Hash of the Datum"
--       DatumTxId # "The Tx table index for the transaction where this script first became available."
--       DatumValue # "The actual data in JSON format (detailed schema)"
--       DatumBytes # "The actual data in CBOR format"

--     RedeemerData --^ do
--       "A table containing Plutus Redeemer Data. These are always referenced by at least one redeemer. New in v13: split from datum table."
--       RedeemerDataHash # "The Hash of the Plutus Data"
--       RedeemerDataTxId # "The Tx table index for the transaction where this script first became available."
--       RedeemerDataValue # "The actual data in JSON format (detailed schema)"
--       RedeemerDataBytes # "The actual data in CBOR format"

--     ExtraKeyWitness --^ do
--       "A table containing transaction extra key witness hashes."
--       ExtraKeyWitnessHash # "The hash of the witness."
--       ExtraKeyWitnessTxId # "The id of the tx this witness belongs to."

--     ParamProposal --^ do
--       "A table containing block chain parameter change proposals."
--       ParamProposalEpochNo
--         # "The epoch for which this parameter proposal in intended to become active.\
--           \ Changed in 13.2-Conway to nullable is always null in Conway era."
--       ParamProposalKey
--         # "The hash of the crypto key used to sign this proposal.\
--           \ Changed in 13.2-Conway to nullable is always null in Conway era."
--       ParamProposalMinFeeA # "The 'a' parameter to calculate the minimum transaction fee."
--       ParamProposalMinFeeB # "The 'b' parameter to calculate the minimum transaction fee."
--       ParamProposalMaxBlockSize # "The maximum block size (in bytes)."
--       ParamProposalMaxTxSize # "The maximum transaction size (in bytes)."
--       ParamProposalMaxBhSize # "The maximum block header size (in bytes)."
--       ParamProposalKeyDeposit # "The amount (in Lovelace) require for a deposit to register a StakeAddress."
--       ParamProposalPoolDeposit # "The amount (in Lovelace) require for a deposit to register a stake pool."
--       ParamProposalMaxEpoch # "The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for."
--       ParamProposalOptimalPoolCount # "The optimal number of stake pools."
--       ParamProposalInfluence # "The influence of the pledge on a stake pool's probability on minting a block."
--       ParamProposalMonetaryExpandRate # "The monetary expansion rate."
--       ParamProposalTreasuryGrowthRate # "The treasury growth rate."
--       ParamProposalDecentralisation # "The decentralisation parameter (1 fully centralised, 0 fully decentralised)."
--       ParamProposalEntropy # "The 32 byte string of extra random-ness to be added into the protocol's entropy pool."
--       ParamProposalProtocolMajor # "The protocol major number."
--       ParamProposalProtocolMinor # "The protocol minor number."
--       ParamProposalMinUtxoValue # "The minimum value of a UTxO entry."
--       ParamProposalMinPoolCost # "The minimum pool cost."
--       ParamProposalCoinsPerUtxoSize # "For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word."
--       ParamProposalCostModelId # "The CostModel table index for the proposal."
--       ParamProposalPriceMem # "The per word cost of script memory usage."
--       ParamProposalPriceStep # "The cost of script execution step usage."
--       ParamProposalMaxTxExMem # "The maximum number of execution memory allowed to be used in a single transaction."
--       ParamProposalMaxTxExSteps # "The maximum number of execution steps allowed to be used in a single transaction."
--       ParamProposalMaxBlockExMem # "The maximum number of execution memory allowed to be used in a single block."
--       ParamProposalMaxBlockExSteps # "The maximum number of execution steps allowed to be used in a single block."
--       ParamProposalMaxValSize # "The maximum Val size."
--       ParamProposalCollateralPercent # "The percentage of the txfee which must be provided as collateral when including non-native scripts."
--       ParamProposalMaxCollateralInputs # "The maximum number of collateral inputs allowed in a transaction."
--       ParamProposalRegisteredTxId # "The Tx table index for the transaction that contains this parameter proposal."
--       ParamProposalPvtMotionNoConfidence # "Pool Voting threshold for motion of no-confidence. New in 13.2-Conway."
--       ParamProposalPvtCommitteeNormal # "Pool Voting threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       ParamProposalPvtCommitteeNoConfidence # "Pool Voting threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       ParamProposalPvtHardForkInitiation # "Pool Voting threshold for hard-fork initiation. New in 13.2-Conway."
--       ParamProposalDvtMotionNoConfidence # "DRep Vote threshold for motion of no-confidence. New in 13.2-Conway."
--       ParamProposalDvtCommitteeNormal # "DRep Vote threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       ParamProposalDvtCommitteeNoConfidence # "DRep Vote threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       ParamProposalDvtUpdateToConstitution # "DRep Vote threshold for update to the Constitution. New in 13.2-Conway."
--       ParamProposalDvtHardForkInitiation # "DRep Vote threshold for hard-fork initiation. New in 13.2-Conway."
--       ParamProposalDvtPPNetworkGroup # "DRep Vote threshold for protocol parameter changes, network group. New in 13.2-Conway."
--       ParamProposalDvtPPEconomicGroup # "DRep Vote threshold for protocol parameter changes, economic group. New in 13.2-Conway."
--       ParamProposalDvtPPTechnicalGroup # "DRep Vote threshold for protocol parameter changes, technical group. New in 13.2-Conway."
--       ParamProposalDvtPPGovGroup # "DRep Vote threshold for protocol parameter changes, governance group. New in 13.2-Conway."
--       ParamProposalDvtTreasuryWithdrawal # "DRep Vote threshold for treasury withdrawal. New in 13.2-Conway."
--       ParamProposalCommitteeMinSize # "Minimal constitutional committee size. New in 13.2-Conway."
--       ParamProposalCommitteeMaxTermLength # "Constitutional committee term limits. New in 13.2-Conway."
--       ParamProposalGovActionLifetime # "Governance action expiration. New in 13.2-Conway."
--       ParamProposalGovActionDeposit # "Governance action deposit. New in 13.2-Conway."
--       ParamProposalDrepDeposit # "DRep deposit amount. New in 13.2-Conway."
--       ParamProposalDrepActivity # "DRep activity period. New in 13.2-Conway."

--     EpochParam --^ do
--       "The accepted protocol parameters for an epoch."
--       EpochParamEpochNo # "The first epoch for which these parameters are valid."
--       EpochParamMinFeeA # "The 'a' parameter to calculate the minimum transaction fee."
--       EpochParamMinFeeB # "The 'b' parameter to calculate the minimum transaction fee."
--       EpochParamMaxBlockSize # "The maximum block size (in bytes)."
--       EpochParamMaxTxSize # "The maximum transaction size (in bytes)."
--       EpochParamMaxBhSize # "The maximum block header size (in bytes)."
--       EpochParamKeyDeposit # "The amount (in Lovelace) require for a deposit to register a StakeAddress."
--       EpochParamPoolDeposit # "The amount (in Lovelace) require for a deposit to register a stake pool."
--       EpochParamMaxEpoch # "The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for."
--       EpochParamOptimalPoolCount # "The optimal number of stake pools."
--       EpochParamInfluence # "The influence of the pledge on a stake pool's probability on minting a block."
--       EpochParamMonetaryExpandRate # "The monetary expansion rate."
--       EpochParamTreasuryGrowthRate # "The treasury growth rate."
--       EpochParamDecentralisation # "The decentralisation parameter (1 fully centralised, 0 fully decentralised)."
--       EpochParamExtraEntropy # "The 32 byte string of extra random-ness to be added into the protocol's entropy pool. New in v13: renamed from entopy."
--       EpochParamProtocolMajor # "The protocol major number."
--       EpochParamProtocolMinor # "The protocol minor number."
--       EpochParamMinUtxoValue # "The minimum value of a UTxO entry."
--       EpochParamMinPoolCost # "The minimum pool cost."
--       EpochParamNonce # "The nonce value for this epoch."
--       EpochParamCoinsPerUtxoSize # "For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word."
--       EpochParamCostModelId # "The CostModel table index for the params."
--       EpochParamPriceMem # "The per word cost of script memory usage."
--       EpochParamPriceStep # "The cost of script execution step usage."
--       EpochParamMaxTxExMem # "The maximum number of execution memory allowed to be used in a single transaction."
--       EpochParamMaxTxExSteps # "The maximum number of execution steps allowed to be used in a single transaction."
--       EpochParamMaxBlockExMem # "The maximum number of execution memory allowed to be used in a single block."
--       EpochParamMaxBlockExSteps # "The maximum number of execution steps allowed to be used in a single block."
--       EpochParamMaxValSize # "The maximum Val size."
--       EpochParamCollateralPercent # "The percentage of the txfee which must be provided as collateral when including non-native scripts."
--       EpochParamMaxCollateralInputs # "The maximum number of collateral inputs allowed in a transaction."
--       EpochParamBlockId # "The Block table index for the first block where these parameters are valid."
--       EpochParamPvtMotionNoConfidence # "Pool Voting threshold for motion of no-confidence. New in 13.2-Conway."
--       EpochParamPvtCommitteeNormal # "Pool Voting threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       EpochParamPvtCommitteeNoConfidence # "Pool Voting threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       EpochParamPvtHardForkInitiation # "Pool Voting threshold for hard-fork initiation. New in 13.2-Conway."
--       EpochParamDvtMotionNoConfidence # "DRep Vote threshold for motion of no-confidence. New in 13.2-Conway."
--       EpochParamDvtCommitteeNormal # "DRep Vote threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       EpochParamDvtCommitteeNoConfidence # "DRep Vote threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       EpochParamDvtUpdateToConstitution # "DRep Vote threshold for update to the Constitution. New in 13.2-Conway."
--       EpochParamDvtHardForkInitiation # "DRep Vote threshold for hard-fork initiation. New in 13.2-Conway."
--       EpochParamDvtPPNetworkGroup # "DRep Vote threshold for protocol parameter changes, network group. New in 13.2-Conway."
--       EpochParamDvtPPEconomicGroup # "DRep Vote threshold for protocol parameter changes, economic group. New in 13.2-Conway."
--       EpochParamDvtPPTechnicalGroup # "DRep Vote threshold for protocol parameter changes, technical group. New in 13.2-Conway."
--       EpochParamDvtPPGovGroup # "DRep Vote threshold for protocol parameter changes, governance group. New in 13.2-Conway."
--       EpochParamDvtTreasuryWithdrawal # "DRep Vote threshold for treasury withdrawal. New in 13.2-Conway."
--       EpochParamCommitteeMinSize # "Minimal constitutional committee size. New in 13.2-Conway."
--       EpochParamCommitteeMaxTermLength # "Constitutional committee term limits. New in 13.2-Conway."
--       EpochParamGovActionLifetime # "Governance action expiration. New in 13.2-Conway."
--       EpochParamGovActionDeposit # "Governance action deposit. New in 13.2-Conway."
--       EpochParamDrepDeposit # "DRep deposit amount. New in 13.2-Conway."
--       EpochParamDrepActivity # "DRep activity period. New in 13.2-Conway."

--     CostModel --^ do
--       "CostModel for EpochParam and ParamProposal."
--       CostModelHash # "The hash of cost model. It ensures uniqueness of entries. New in v13."
--       CostModelCosts # "The actual costs formatted as json."

--     PoolStat --^ do
--       "Stats per pool and per epoch."
--       PoolStatPoolHashId # "The pool_hash_id reference."
--       PoolStatEpochNo # "The epoch number."
--       PoolStatNumberOfBlocks # "Number of blocks created on the previous epoch."
--       PoolStatNumberOfDelegators # "Number of delegators in the mark snapshot."
--       PoolStatStake # "Total stake in the mark snapshot."
--       PoolStatVotingPower # "Voting power of the SPO."

--     EpochState --^ do
--       "Table with governance (and in the future other) stats per epoch."
--       EpochStateCommitteeId # "The reference to the current committee."
--       EpochStateNoConfidenceId # "The reference to the current gov_action_proposal of no confidence. TODO: This remains NULL."
--       EpochStateConstitutionId # "The reference to the current constitution. Should never be null."
--       EpochStateEpochNo # "The epoch in question."

--     ExtraMigrations --^ do
--       "Extra optional migrations. New in 13.2."
--       ExtraMigrationsDescription # "A description of the migration"

--     DrepHash --^ do
--       "A table for every unique drep key hash.\
--       \ The existance of an entry doesn't mean the DRep is registered.\
--       \ New in 13.2-Conway."
--       DrepHashRaw # "The raw bytes of the DRep."
--       DrepHashView # "The human readable encoding of the Drep."
--       DrepHashHasScript # "Flag which shows if this DRep credentials are a script hash"

--     CommitteeHash --^ do
--       "A table for all committee credentials hot or cold"
--       CommitteeHashRaw # "The key or script hash"
--       CommitteeHashHasScript # "Flag which shows if this credential is a script hash"

--     DelegationVote --^ do
--       "A table containing delegations from a stake address to a stake pool. New in 13.2-Conway."
--       DelegationVoteAddrId # "The StakeAddress table index for the stake address."
--       DelegationVoteCertIndex # "The index of this delegation within the certificates of this transaction."
--       DelegationVoteDrepHashId # "The DrepHash table index for the pool being delegated to."
--       DelegationVoteTxId # "The Tx table index of the transaction that contained this delegation."
--       DelegationVoteRedeemerId # "The Redeemer table index that is related with this certificate. TODO: can vote redeemers index these delegations?"

--     CommitteeRegistration --^ do
--       "A table for every committee hot key registration. New in 13.2-Conway."
--       CommitteeRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       CommitteeRegistrationCertIndex # "The index of this registration within the certificates of this transaction."
--       CommitteeRegistrationColdKeyId # "The reference to the registered cold key hash id"
--       CommitteeRegistrationHotKeyId # "The reference to the registered hot key hash id"

--     CommitteeDeRegistration --^ do
--       "A table for every committee key de-registration. New in 13.2-Conway."
--       CommitteeDeRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       CommitteeDeRegistrationCertIndex # "The index of this deregistration within the certificates of this transaction."
--       CommitteeDeRegistrationColdKeyId # "The reference to the the deregistered cold key hash id"
--       CommitteeDeRegistrationVotingAnchorId # "The Voting anchor reference id"

--     DrepRegistration --^ do
--       "A table for DRep registrations, deregistrations or updates. Registration have positive deposit values, deregistrations have negative and\
--       \ updates have null. Based on this distinction, for a specific DRep, getting the latest entry gives its registration state. New in 13.2-Conway."
--       DrepRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       DrepRegistrationCertIndex # "The index of this registration within the certificates of this transaction."
--       DrepRegistrationDeposit # "The deposits payed if this is an initial registration."
--       DrepRegistrationDrepHashId # "The Drep hash index of this registration."

--     VotingAnchor --^ do
--       "A table for every Anchor that appears on Governance Actions. These are pointers to offchain metadata. \
--       \ The tuple of url and hash is unique. New in 13.2-Conway."
--       VotingAnchorBlockId # "The Block table index of the tx that includes this anchor. This only exists to facilitate rollbacks"
--       VotingAnchorDataHash # "A hash of the contents of the metadata URL"
--       VotingAnchorUrl # "A URL to a JSON payload of metadata"
--       VotingAnchorType # "The type of the anchor. It can be gov_action, drep, other, vote, committee_dereg, constitution"

--     GovActionProposal --^ do
--       "A table for proposed GovActionProposal, aka ProposalProcedure, GovAction or GovProposal.\
--       \ This table may be referenced\
--       \ by TreasuryWithdrawal or NewCommittee. New in 13.2-Conway."
--       GovActionProposalTxId # "The Tx table index of the tx that includes this certificate."
--       GovActionProposalIndex # "The index of this proposal procedure within its transaction."
--       GovActionProposalPrevGovActionProposal # "The previous related GovActionProposal. This is null for "
--       GovActionProposalDeposit # "The deposit amount payed for this proposal."
--       GovActionProposalReturnAddress # "The StakeAddress index of the reward address to receive the deposit when it is repaid."
--       GovActionProposalVotingAnchorId # "The Anchor table index related to this proposal."
--       GovActionProposalType # "Can be one of ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction"
--       GovActionProposalDescription # "A Text describing the content of this GovActionProposal in a readable way."
--       GovActionProposalParamProposal # "If this is a param proposal action, this has the index of the param_proposal table."
--       GovActionProposalRatifiedEpoch # "If not null, then this proposal has been ratified at the specfied epoch."
--       GovActionProposalEnactedEpoch # "If not null, then this proposal has been enacted at the specfied epoch."
--       GovActionProposalExpiredEpoch # "If not null, then this proposal has been expired at the specfied epoch."
--       GovActionProposalDroppedEpoch
--         # "If not null, then this proposal has been dropped at the specfied epoch. A proposal is dropped when it's \
--           \expired or enacted or when one of its dependencies is expired."
--       GovActionProposalExpiration # "Shows the epoch at which this governance action will expire."

--     TreasuryWithdrawal --^ do
--       "A table for all treasury withdrawals proposed on a GovActionProposal. New in 13.2-Conway."
--       TreasuryWithdrawalGovActionProposalId
--         # "The GovActionProposal table index for this withdrawal.\
--           \Multiple TreasuryWithdrawal may reference the same GovActionProposal."
--       TreasuryWithdrawalStakeAddressId # "The address that benefits from this withdrawal."
--       TreasuryWithdrawalAmount # "The amount for this withdrawl."

--     Committee --^ do
--       "A table for new committee proposed on a GovActionProposal. New in 13.2-Conway."
--       CommitteeGovActionProposalId # "The GovActionProposal table index for this new committee. This can be null for genesis committees."
--       CommitteeQuorumNumerator # "The proposed quorum nominator."
--       CommitteeQuorumDenominator # "The proposed quorum denominator."

--     CommitteeMember --^ do
--       "A table for members of the committee. A committee can have multiple members. New in 13.3-Conway."
--       CommitteeMemberCommitteeId # "The reference to the committee"
--       CommitteeMemberCommitteeHashId # "The reference to the committee hash"
--       CommitteeMemberExpirationEpoch # "The epoch this member expires"

--     Constitution --^ do
--       "A table for constitution attached to a GovActionProposal. New in 13.2-Conway."
--       ConstitutionGovActionProposalId # "The GovActionProposal table index for this constitution."
--       ConstitutionVotingAnchorId # "The ConstitutionVotingAnchor table index for this constitution."
--       ConstitutionScriptHash # "The Script Hash. It's associated script may not be already inserted in the script table."

--     VotingProcedure --^ do
--       "A table for voting procedures, aka GovVote. A Vote can be Yes No or Abstain. New in 13.2-Conway."
--       VotingProcedureTxId # "The Tx table index of the tx that includes this VotingProcedure."
--       VotingProcedureIndex # "The index of this VotingProcedure within this transaction."
--       VotingProcedureGovActionProposalId # "The index of the GovActionProposal that this vote targets."
--       VotingProcedureVoterRole # "The role of the voter. Can be one of ConstitutionalCommittee, DRep, SPO."
--       VotingProcedureCommitteeVoter # "A reference to the hot key committee hash entry that voted"
--       VotingProcedureDrepVoter # "A reference to the drep hash entry that voted"
--       VotingProcedurePoolVoter # "A reference to the pool hash entry that voted"
--       VotingProcedureVote # "The Vote. Can be one of Yes, No, Abstain."
--       VotingProcedureVotingAnchorId # "The VotingAnchor table index associated with this VotingProcedure."
--       VotingProcedureInvalid # "TODO: This is currently not implemented and always stays null. Not null if the vote is invalid."

--     OffChainVoteData --^ do
--       "The table with the offchain metadata related to Vote Anchors. It accepts metadata in a more lenient way than what's\
--       \ decribed in CIP-100. New in 13.2-Conway."
--       OffChainVoteDataVotingAnchorId # "The VotingAnchor table index this offchain data refers."
--       OffChainVoteDataHash # "The hash of the offchain data."
--       OffChainVoteDataLanguage # "The langauge described in the context of the metadata. Described in CIP-100. New in 13.3-Conway."
--       OffChainVoteDataJson # "The payload as JSON."
--       OffChainVoteDataBytes # "The raw bytes of the payload."
--       OffChainVoteDataWarning # "A warning that occured while validating the metadata."
--       OffChainVoteDataIsValid
--         # "False if the data is found invalid. db-sync leaves this field null \
--           \since it normally populates off_chain_vote_fetch_error for invalid data. \
--           \It can be used manually to mark some metadata invalid by clients."

--     OffChainVoteGovActionData --^ do
--       "The table with offchain metadata for Governance Actions. Implementes CIP-108. New in 13.3-Conway."
--       OffChainVoteGovActionDataOffChainVoteDataId # "The vote metadata table index this offchain data belongs to."
--       OffChainVoteGovActionDataTitle # "The title"
--       OffChainVoteGovActionDataAbstract # "The abstract"
--       OffChainVoteGovActionDataMotivation # "The motivation"
--       OffChainVoteGovActionDataRationale # "The rationale"

--     OffChainVoteDrepData --^ do
--       "The table with offchain metadata for Drep Registrations. Implementes CIP-119. New in 13.3-Conway."
--       OffChainVoteDrepDataOffChainVoteDataId # "The vote metadata table index this offchain data belongs to."
--       OffChainVoteDrepDataPaymentAddress # "The payment address"
--       OffChainVoteDrepDataGivenName # "The name. This is the only mandatory field"
--       OffChainVoteDrepDataObjectives # "The objectives"
--       OffChainVoteDrepDataMotivations # "The motivations"
--       OffChainVoteDrepDataQualifications # "The qualifications"

--     OffChainVoteAuthor --^ do
--       "The table with offchain metadata authors, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteAuthorOffChainVoteDataId # "The OffChainVoteData table index this offchain data refers."
--       OffChainVoteAuthorName # "The name of the author."
--       OffChainVoteAuthorWitnessAlgorithm # "The witness algorithm used by the author."
--       OffChainVoteAuthorPublicKey # "The public key used by the author."
--       OffChainVoteAuthorSignature # "The signature of the author."
--       OffChainVoteAuthorWarning # "A warning related to verifying this metadata."

--     OffChainVoteReference --^ do
--       "The table with offchain metadata references, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteReferenceOffChainVoteDataId # "The OffChainVoteData table index this entry refers."
--       OffChainVoteReferenceLabel # "The label of this vote reference."
--       OffChainVoteReferenceUri # "The uri of this vote reference."
--       OffChainVoteReferenceHashDigest
--         # "The hash digest of this vote reference, as described in CIP-108. \
--           \This only appears for governance action metadata."
--       OffChainVoteReferenceHashAlgorithm
--         # "The hash algorithm of this vote reference, as described in CIP-108. \
--           \This only appears for governance action metadata."

--     OffChainVoteExternalUpdate --^ do
--       "The table with offchain metadata external updates, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteExternalUpdateOffChainVoteDataId # "The OffChainVoteData table index this entry refers."
--       OffChainVoteExternalUpdateTitle # "The title of this external update."
--       OffChainVoteExternalUpdateUri # "The uri of this external update."

--     OffChainVoteFetchError --^ do
--       "Errors while fetching or validating offchain Voting Anchor metadata. New in 13.2-Conway."
--       OffChainVoteFetchErrorVotingAnchorId # "The VotingAnchor table index this offchain fetch error refers."
--       OffChainVoteFetchErrorFetchError # "The text of the error."
--       OffChainVoteFetchErrorRetryCount # "The number of retries."

--     DrepDistr --^ do
--       "The table for the distribution of voting power per DRep per. Currently this has a single entry per DRep\
--       \ and doesn't show every delegator. This may change. New in 13.2-Conway."
--       DrepDistrHashId # "The DrepHash table index that this distribution entry has information about."
--       DrepDistrAmount # "The total amount of voting power this DRep is delegated."
--       DrepDistrEpochNo # "The epoch no this distribution is about."
--       DrepDistrActiveUntil # "The epoch until which this drep is active. TODO: This currently remains null always. "

--     OffChainPoolData --^ do
--       "The pool offchain (ie not on chain) for a stake pool."
--       OffChainPoolDataPoolId # "The PoolHash table index for the pool this offchain data refers."
--       OffChainPoolDataTickerName # "The pool's ticker name (as many as 5 characters)."
--       OffChainPoolDataHash # "The hash of the offchain data."
--       OffChainPoolDataJson # "The payload as JSON."
--       OffChainPoolDataBytes # "The raw bytes of the payload."
--       OffChainPoolDataPmrId # "The PoolMetadataRef table index for this offchain data."

--     OffChainPoolFetchError --^ do
--       "A table containing pool offchain data fetch errors."
--       OffChainPoolFetchErrorPoolId # "The PoolHash table index for the pool this offchain fetch error refers."
--       OffChainPoolFetchErrorFetchTime # "The UTC time stamp of the error."
--       OffChainPoolFetchErrorPmrId # "The PoolMetadataRef table index for this offchain data."
--       OffChainPoolFetchErrorFetchError # "The text of the error."
--       OffChainPoolFetchErrorRetryCount # "The number of retries."

--     ReservedPoolTicker --^ do
--       "A table containing a managed list of reserved ticker names."
--       ReservedPoolTickerName # "The ticker name."
--       ReservedPoolTickerPoolHash # "The hash of the pool that owns this ticker."

--     DelistedPool --^ do
--       "A table containing pools that have been delisted."
--       DelistedPoolHashRaw # "The pool hash"
