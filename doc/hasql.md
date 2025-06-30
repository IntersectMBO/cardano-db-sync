# Developer Guide: Working with Hasql Implementation

## Core Concepts

### DbAction Monad
All database operations now use `DbAction m` instead of `ReaderT SqlBackend m`:

```haskell
-- Old (Persistent)
insertBlock :: MonadIO m => Block -> ReaderT SqlBackend m BlockId

-- New (Hasql)
insertBlock :: MonadIO m => Block -> DbAction m BlockId
```

### Statement Construction
Database operations built using Hasql's encoder/decoder pattern:

```haskell
insertBlockStmt :: HsqlStmt.Statement Block BlockId
insertBlockStmt = 
  insert 
    blockEncoder 
    (WithResult $ HsqlD.singleRow $ idDecoder BlockId)
```

## Module Structure

- `Cardano.Db.Schema.*` - Type-safe schema definitions
- `Cardano.Db.Statement.*` - Database operations organized by domain
- `Cardano.Db.Statement.Function.*` - Core statement building utilities

## Key Operations

### Inserts
```haskell
-- Simple insert
insert :: HsqlE.Params a -> ResultType r r -> HsqlS.Statement a r

-- Bulk insert
insertBulk :: [a] -> DbAction m [r]

-- Conditional insert
insertIfUnique :: HsqlE.Params a -> ResultType r r -> HsqlS.Statement a r
```

### Queries
```haskell
-- Count operations
countAll :: HsqlStmt.Statement () Word64
countWhere :: Text -> Text -> HsqlStmt.Statement () Word64

-- Existence checks
existsById :: Key a -> DbAction m Bool
existsWhereByColumn :: Text -> p -> DbAction m Bool
```

### Execution Pattern
```haskell
runOperation :: MonadIO m => SomeRecord -> DbAction m SomeId
runOperation record = 
  runDbSession (mkDbCallStack "runOperation") $
    HsqlSes.statement record someStmt
```

## Type Safety

### Column Validation
All column references validated at compile time:
```haskell
validateColumn @Block "epoch_no"  -- Compile-time check
```

### Schema Correspondence
Each table has corresponding encoder/decoder pairs ensuring type safety.

## Migration Notes

### Database Functions
- Replace `rawSql` calls with typed statements
- Use `HsqlStmt.Statement` construction pattern
- Wrap operations in `runDbSession` with call stack

### Error Handling
```haskell
-- Handle Maybe results
case result of
  Just value -> pure value
  Nothing -> throwError $ DbError callStack errorMsg Nothing
```

### Testing
- Test database roundtrips with property-based testing
- Use `runDbLovelaceRoundtrip` style functions for validation
- Test encoders/decoders separately from business logic
