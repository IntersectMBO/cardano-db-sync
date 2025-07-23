# Creating Hasql Encoders, Decoders, and DbInfo Instances

## Data Type Definition

```haskell
-- Example data type
data MaTxOutAddress = MaTxOutAddress
  { maTxOutAddressIdent :: !Id.MultiAssetId
  , maTxOutAddressQuantity :: !DbWord64
  , maTxOutAddressTxOutId :: !Id.TxOutAddressId
  }
  deriving (Eq, Show, Generic)

-- Required: Key type instance
type instance Key MaTxOutAddress = Id.MaTxOutAddressId
```

## DbInfo Instance

```haskell
instance DbInfo MaTxOutAddress where
  -- Explicit table name (overrides default snake_case conversion)
  tableName _ = "ma_tx_out"
  
  -- Column names in database order (excludes auto-generated 'id' column)
  columnNames _ = NE.fromList ["quantity", "tx_out_id", "ident"]
  
  -- For bulk operations: (column_name, postgres_array_type)
  unnestParamTypes _ = 
    [ ("ident", "bigint[]")
    , ("quantity", "bigint[]") 
    , ("tx_out_id", "bigint[]")
    ]
  
  -- Optional: Unique constraint columns
  uniqueFields _ = ["unique_col1", "unique_col2"]
  
  -- Optional: JSONB columns
  jsonbFields _ = ["json_column"]
```

### DbInfo Configuration Options

```haskell
instance DbInfo SomeTable where
  -- Table name (default: snake_case of type name)
  tableName _ = "custom_table_name"
  
  -- Column names (default: derived from field names)
  columnNames _ = NE.fromList ["col1", "col2", "col3"]
  
  -- Unique constraints
  uniqueFields _ = ["col1", "col2"]  -- Multi-column unique constraint
  
  -- Bulk unique fields (for bulk operations only)
  bulkUniqueFields _ = ["bulk_unique_col"]
  
  -- JSONB columns (require ::jsonb casting)
  jsonbFields _ = ["metadata", "config"]
  
  -- Enum columns with their types
  enumFields _ = [("status", "status_type"), ("priority", "priority_type")]
  
  -- Generated columns (excluded from inserts)
  generatedFields _ = ["created_at", "updated_at"]
  
  -- Bulk operation parameters
  unnestParamTypes _ = 
    [ ("col1", "bigint[]")
    , ("col2", "text[]")
    , ("col3", "boolean[]")
    ]
```

## Entity Decoder

```haskell
entityMaTxOutAddressDecoder :: D.Row (Entity MaTxOutAddress)
entityMaTxOutAddressDecoder =
  Entity
    <$> Id.idDecoder Id.MaTxOutAddressId  -- Entity ID
    <*> maTxOutAddressDecoder             -- Entity data
```

## Record Decoder

```haskell
maTxOutAddressDecoder :: D.Row MaTxOutAddress
maTxOutAddressDecoder =
  MaTxOutAddress
    <$> Id.idDecoder Id.MultiAssetId  -- Foreign key ID
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8)  -- DbWord64
    <*> Id.idDecoder Id.TxOutAddressId  -- Another foreign key ID
```

### Decoder Patterns

```haskell
-- Basic types
<*> D.column (D.nonNullable D.text)           -- Text
<*> D.column (D.nonNullable D.bool)           -- Bool
<*> D.column (D.nonNullable D.bytea)          -- ByteString
<*> D.column (D.nonNullable $ fromIntegral <$> D.int8)  -- Word64/Int

-- Nullable types
<*> D.column (D.nullable D.text)              -- Maybe Text
<*> D.column (D.nullable D.bytea)             -- Maybe ByteString

-- ID types
<*> Id.idDecoder Id.SomeId                    -- !Id.SomeId
<*> Id.maybeIdDecoder Id.SomeId               -- !(Maybe Id.SomeId)

-- Custom types with decoders
<*> dbLovelaceDecoder                         -- DbLovelace
<*> D.column (D.nonNullable utcTimeAsTimestampDecoder)  -- UTCTime
<*> rewardSourceDecoder                       -- Custom enum

-- Wrapped types
<*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8)  -- DbWord64
```

## Entity Encoder

```haskell
entityMaTxOutAddressEncoder :: E.Params (Entity MaTxOutAddress)
entityMaTxOutAddressEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getMaTxOutAddressId  -- Entity ID
    , entityVal >$< maTxOutAddressEncoder                -- Entity data
    ]
```

## Record Encoder

```haskell
maTxOutAddressEncoder :: E.Params MaTxOutAddress
maTxOutAddressEncoder =
  mconcat
    [ maTxOutAddressIdent >$< Id.idEncoder Id.getMultiAssetId
    , maTxOutAddressQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutAddressTxOutId >$< Id.idEncoder Id.getTxOutAddressId
    ]
```

### Encoder Patterns

```haskell
-- Basic types
field >$< E.param (E.nonNullable E.text)           -- Text
field >$< E.param (E.nonNullable E.bool)           -- Bool  
field >$< E.param (E.nonNullable E.bytea)          -- ByteString
field >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)  -- Word64/Int

-- Nullable types  
field >$< E.param (E.nullable E.text)              -- Maybe Text
field >$< E.param (E.nullable E.bytea)             -- Maybe ByteString

-- ID types
field >$< Id.idEncoder Id.getSomeId                 -- Id.SomeId
field >$< Id.maybeIdEncoder Id.getSomeId            -- Maybe Id.SomeId

-- Custom types with encoders
field >$< dbLovelaceEncoder                         -- DbLovelace
field >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)  -- UTCTime
field >$< rewardSourceEncoder                       -- Custom enum

-- Wrapped types
field >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)  -- DbWord64
```

## Bulk Encoder

```haskell
maTxOutAddressBulkEncoder :: E.Params ([Id.MultiAssetId], [DbWord64], [Id.TxOutAddressId])
maTxOutAddressBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ Id.getMultiAssetId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)  
    (bulkEncoder $ E.nonNullable $ Id.getTxOutAddressId >$< E.int8)
```

### Bulk Encoder Utilities

```haskell
-- For 2 fields
contrazip2 encoder1 encoder2

-- For 3 fields  
contrazip3 encoder1 encoder2 encoder3

-- For 4 fields
contrazip4 encoder1 encoder2 encoder3 encoder4

-- For 5 fields
contrazip5 encoder1 encoder2 encoder3 encoder4 encoder5

-- Pattern for each field
(bulkEncoder $ E.nonNullable $ transformation >$< E.baseType)
(bulkEncoder $ E.nullable $ transformation >$< E.baseType)  -- For nullable
```

## Complete Example

```haskell
-- Data type
data EventInfo = EventInfo
  { eventInfoTxId :: !(Maybe Id.TxId)
  , eventInfoEpoch :: !Word64
  , eventInfoType :: !Text
  , eventInfoExplanation :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key EventInfo = Id.EventInfoId

-- DbInfo instance
instance DbInfo EventInfo where
  tableName _ = "event_info"
  columnNames _ = NE.fromList ["tx_id", "epoch", "type", "explanation"]
  unnestParamTypes _ = 
    [ ("tx_id", "bigint[]")
    , ("epoch", "bigint[]")
    , ("type", "text[]")
    , ("explanation", "text[]")
    ]

-- Entity decoder
entityEventInfoDecoder :: D.Row (Entity EventInfo)
entityEventInfoDecoder =
  Entity
    <$> Id.idDecoder Id.EventInfoId
    <*> eventInfoDecoder

-- Record decoder
eventInfoDecoder :: D.Row EventInfo
eventInfoDecoder =
  EventInfo
    <$> Id.maybeIdDecoder Id.TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nullable D.text)

-- Entity encoder
entityEventInfoEncoder :: E.Params (Entity EventInfo)
entityEventInfoEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getEventInfoId
    , entityVal >$< eventInfoEncoder
    ]

-- Record encoder
eventInfoEncoder :: E.Params EventInfo
eventInfoEncoder =
  mconcat
    [ eventInfoTxId >$< Id.maybeIdEncoder Id.getTxId
    , eventInfoEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , eventInfoType >$< E.param (E.nonNullable E.text)
    , eventInfoExplanation >$< E.param (E.nullable E.text)
    ]

-- Bulk encoder
eventInfoBulkEncoder :: E.Params ([Maybe Id.TxId], [Word64], [Text], [Maybe Text])
eventInfoBulkEncoder =
  contrazip4
    (bulkEncoder $ E.nullable $ Id.getTxId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nullable E.text)
```

## Field Naming Convention

- Fields must start with the lowercased type name
- Follow with uppercase letter for the actual field name
- Example: `MaTxOutAddress` → `maTxOutAddressFieldName`

## Type Mapping Reference

| Haskell Type | Decoder | Encoder |
|-------------|---------|---------|
| `Text` | `D.text` | `E.text` |
| `Bool` | `D.bool` | `E.bool` |
| `ByteString` | `D.bytea` | `E.bytea` |
| `Word64` | `fromIntegral <$> D.int8` | `fromIntegral >$< E.int8` |
| `UTCTime` | `utcTimeAsTimestampDecoder` | `utcTimeAsTimestampEncoder` |
| `DbLovelace` | `dbLovelaceDecoder` | `dbLovelaceEncoder` |
| `DbWord64` | `DbWord64 . fromIntegral <$> D.int8` | `fromIntegral . unDbWord64 >$< E.int8` |
| `Id.SomeId` | `Id.idDecoder Id.SomeId` | `Id.idEncoder Id.getSomeId` |
| `Maybe Id.SomeId` | `Id.maybeIdDecoder Id.SomeId` | `Id.maybeIdEncoder Id.getSomeId` |

## Common Patterns

### JSON Fields
```haskell
instance DbInfo MyTable where
  jsonbFields _ = ["metadata"]

-- In decoder/encoder, treat as Text with special handling
```

### Unique Constraints
```haskell
instance DbInfo MyTable where  
  uniqueFields _ = ["field1", "field2"]  -- Composite unique constraint
```

### Generated Fields
```haskell
instance DbInfo MyTable where
  generatedFields _ = ["created_at"]  -- Excluded from inserts
```
