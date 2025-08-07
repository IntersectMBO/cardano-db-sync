# Database Monad Layer Documentation

## Overview

The database layer provides a clean, composable interface for database operations in cardano-db-sync. It abstracts away connection management, transaction handling, and error management while providing different execution contexts for various use cases.

## Core Architecture

### DbM Monad

The foundation of the database layer is the `DbM` monad:

```haskell
newtype DbM a = DbM {runDbM :: ReaderT DbEnv IO a}
  deriving (Functor, Applicative, Monad, MonadReader DbEnv, MonadIO)
```

`DbM` is essentially a `ReaderT` that provides access to database environment (`DbEnv`) within IO operations. This design allows:

- **Environment Access**: All database operations have access to connection information and configuration
- **Composability**: Database operations can be easily combined and sequenced  
- **IO Integration**: Direct integration with IO operations when needed
- **Clean Abstractions**: Hide connection management complexity from business logic

### DbEnv Environment

The database environment contains all necessary context for database operations:

```haskell
data DbEnv = DbEnv
  { dbConnection :: !HsqlCon.Connection           -- Primary connection for sequential/transactional ops
  , dbPoolConnection :: !(Maybe (Pool HsqlCon.Connection))  -- Pool for parallel/async ops  
  , dbTracer :: !(Maybe (Trace IO Text))          -- Optional tracing/logging
  }
```

**Connection Strategy**:
- **Primary Connection**: Used for main application logic, transactions, sequential operations
- **Pool Connection**: Used for background operations, parallel queries, non-blocking operations
- **Tracer**: Provides structured logging when available

## Database Operations

### Writing Database Operations

Database operations are written using the `runSession` function:

```haskell
runSession :: HsqlS.Session a -> DbM a
```

**Example**:
```haskell
insertUser :: User -> DbM UserId
insertUser user = 
  runSession $ HsqlS.statement user insertUserStmt
  where
    insertUserStmt :: HsqlStmt.Statement User UserId
    insertUserStmt = -- Hasql statement definition

queryUserById :: UserId -> DbM (Maybe User)
queryUserById userId =
  runSession $ HsqlS.statement userId queryUserByIdStmt
  where
    queryUserByIdStmt :: HsqlStmt.Statement UserId (Maybe User)
    queryUserByIdStmt = -- Hasql statement definition
```

**Key Points**:
- `runSession` executes Hasql sessions within the `DbM` context
- Each `runSession` call creates its own database operation
- Session errors are automatically converted to exceptions
- Operations have access to `DbEnv` through the `MonadReader` interface

### Composing Operations

Database operations compose naturally in the `DbM` monad:

```haskell
createUserWithProfile :: UserData -> ProfileData -> DbM UserProfile  
createUserWithProfile userData profileData = do
  -- These operations can access the same DbEnv
  userId <- insertUser userData
  profileId <- insertProfile userId profileData  
  user <- queryUser userId
  profile <- queryProfile profileId
  pure $ UserProfile user profile
```

### Error Handling and Rollback

Transaction runners automatically handle rollback:

```haskell
result <- runDbTransactionIohkLogging tracer dbEnv $ do
  userId <- insertUser user
  when (invalidUser user) $ 
    throwIO $ ValidationError "Invalid user data"  -- Triggers rollback
  insertProfile userId profile
  -- If validation fails, both insertUser and partial work are rolled back
```

For operations that might fail at the application level (not just session level):

```haskell
-- Query that might not find a result
queryUserByEmail :: Email -> DbM (Either Text User)
queryUserByEmail email = do
  result <- runSession $ HsqlS.statement email queryUserByEmailStmt
  case result of
    Nothing -> pure $ Left "User not found"
    Just user -> pure $ Right user

-- Using it in application code
findUser :: Email -> ExceptT SomeAppError DbM User
findUser email = do
  result <- lift $ queryUserByEmail email
  case result of
    Left err -> throwError $ UserNotFound err
    Right user -> pure user
```

## Database Runners

The database layer provides different runners for different execution contexts:

### Transaction Runner

For operations requiring ACID guarantees:

#### `runDbTransactionIohkLogging`
```haskell
runDbTransactionIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbM a -> m a
```

**Use Case**: Primary runner for blockchain synchronization and any operations requiring ACID guarantees
- Wraps all operations in a single database transaction
- Provides full ACID guarantees with automatic BEGIN/COMMIT/ROLLBACK
- Comprehensive logging for debugging and monitoring
- **Example**: Processing a complete block with all transactions atomically

**Complete End-to-End Transaction Example**:
```haskell

-- Usage: All operations run in a single transaction
processBlockTransactionally :: DbEnv -> Block -> IO (Either SomeError ProcessResult)
processBlockTransactionally dbEnv block = do
  result <- try $ runDbTransactionIohkLogging tracer dbEnv $ do
    processCompleteBlock block
  
  case result of
    Left dbErr -> pure $ Left $ DatabaseError dbErr
    Right processResult -> pure $ Right processResult
    
  where
    tracer = fromMaybe mempty (dbTracer dbEnv)

-- Composite operation combining multiple DB operations
processCompleteBlock :: Block -> DbM ProcessResult
processCompleteBlock block = do
  -- Query: Check if block already exists
  existingBlockId <- queryExistingBlock (blockHash block)
  
  case existingBlockId of
    Just blockId -> do
      -- Block exists, just return existing ID
      pure $ BlockAlreadyExists blockId
      
    Nothing -> do
      -- Insert: Create new block
      blockId <- insertBlock block
      
      -- Insert: Process all transactions in the block  
      txIds <- traverse (insertTx blockId) (blockTxs block)
      
      -- Insert: Process all outputs for each transaction
      traverse_ processTransactionOutputs (zip txIds (blockTxs block))
      
      -- Update: Calculate and store block statistics
      let stats = calculateBlockStats block txIds
      updateBlockStats blockId stats
      
      -- Query: Verify the block was inserted correctly
      verificationResult <- queryExistingBlock (blockHash block)
      
      case verificationResult of
        Just verifiedBlockId -> pure $ BlockProcessed verifiedBlockId (length txIds)
        Nothing -> throwIO $ DatabaseInconsistencyError "Block not found after insertion"
  
  where
    processTransactionOutputs :: (TxId, Tx) -> DbM ()
    processTransactionOutputs (txId, tx) = do
      traverse_ (insertTxOut txId) (txOutputs tx)

-- In The Database Library

-- Individual database operations
insertBlock :: Block -> DbM BlockId
insertBlock block = 
  runSession $ HsqlS.statement block insertBlockStmt

insertTx :: BlockId -> Tx -> DbM TxId
insertTx blockId tx = 
  runSession $ HsqlS.statement (blockId, tx) insertTxStmt

insertTxOut :: TxId -> TxOut -> DbM TxOutId  
insertTxOut txId txOut =
  runSession $ HsqlS.statement (txId, txOut) insertTxOutStmt

queryExistingBlock :: BlockHash -> DbM (Maybe BlockId)
queryExistingBlock blockHash =
  runSession $ HsqlS.statement blockHash queryBlockByHashStmt

updateBlockStats :: BlockId -> BlockStats -> DbM ()
updateBlockStats blockId stats =
  runSession $ HsqlS.statement (blockId, stats) updateStatsStmt

```

**In this example, the single transaction includes**:
1. **Query Operation**: Check for existing block
2. **Multiple Insert Operations**: Block, transactions, and outputs  
3. **Update Operation**: Block statistics
4. **Verification Query**: Confirm successful insertion

**Transaction Guarantees**:
- If ANY operation fails, ALL operations are rolled back
- The database remains in a consistent state
- Either all changes are committed together, or none are

### Pool-Based Runners

For concurrent operations that don't require transactions:

#### `runDbPoolIohkLogging`
```haskell
runDbPoolIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbM a -> m a
```

**Use Case**: Background operations parallel to main sync
- Uses connection from the DbEnv's pool
- Each operation auto-commits (no explicit transaction boundaries)
- **Example**: Background cache updates, statistics calculations


## Connection Management

### Primary vs Pool Connections

**Primary Connection (`dbConnection`)**:
- Used for sequential operations and transactions
- Single-threaded access pattern
- Maintains transaction state
- Used by transaction runners

**Pool Connections (`dbPoolConnection`)**:
- Used for parallel/async operations  
- Multi-threaded access with resource pooling
- Each connection operates independently
- Used by pool runners

### Connection Lifecycle

**Manual Management** (when you have DbEnv):
```haskell
-- Connection is managed by the caller
processWithExistingEnv :: DbEnv -> SomeData -> IO Result
processWithExistingEnv dbEnv someData = 
  runDbTransactionIohkLogging tracer dbEnv $ do
    -- operations here
```

**Automatic Management** (standalone runners):
```haskell
-- Connection created and cleaned up automatically
processStandalone :: SomeData -> IO Result  
processStandalone someData =
  runDbMNoLoggingDefaultEnv $ do
    -- operations here
    -- connection automatically closed when done
```

## Transaction Behavior

### Transaction Boundaries

**With Transaction Runner**:
```haskell
result <- runDbTransactionIohkLogging tracer dbEnv $ do
  userId <- insertUser user          -- Part of transaction
  profileId <- insertProfile profile -- Part of transaction  
  updateStats userId                 -- Part of transaction
  -- All operations committed together or all rolled back on error
```

**Without Transaction Runner**:
```haskell
result <- runDbPoolIohkLogging tracer dbEnv $ do
  userId <- insertUser user          -- Auto-commits immediately
  profileId <- insertProfile profile -- Auto-commits immediately
  updateStats userId                 -- Auto-commits immediately  
  -- Each operation commits independently
```

## Best Practices

### Operation Design

**Prefer small, focused operations**:
```haskell
-- Good: focused, single responsibility
insertBlock :: Block -> DbM BlockId
insertTx :: BlockId -> Tx -> DbM TxId
insertTxOut :: TxId -> TxOut -> DbM TxOutId

-- Compose them in business logic
insertBlockWithTxs :: Block -> DbM BlockId
insertBlockWithTxs block = do
  blockId <- insertBlock block
  traverse_ (insertTx blockId) (blockTxs block)
  pure blockId
```

**Handle application-level failures explicitly**:
```haskell
-- Return Either for expected failures
queryOptionalData :: SomeId -> DbM (Either Text SomeData)

-- Let exceptions propagate for unexpected failures  
queryRequiredData :: SomeId -> DbM SomeData -- Throws on session errors
```

### Performance Considerations

1. **Use transactions for related operations**: Group related inserts/updates
2. **Use pools for independent queries**: Parallel operations that don't need coordination
3. **Minimize transaction scope**: Don't include unnecessary operations in transactions
4. **Consider connection reuse**: Prefer existing DbEnv over creating new connections

## Migration from Previous Architecture

The new `DbM` architecture replaces the previous `DbAction` monad that had built-in `ExceptT`. Key changes:

### Before (DbAction with ExceptT)
```haskell
insertUser :: User -> DbAction m (Either DbError UserId)
insertUser user = do
  -- Complex nested error handling
  result <- lift $ runDbSession stmt
  case result of
    Left dbErr -> pure $ Left dbErr
    Right userId -> pure $ Right userId
```

### After (DbM)
```haskell
insertUser :: User -> DbM UserId  
insertUser user = 
  runSession $ HsqlS.statement user insertUserStmt
  -- Errors handled automatically by runSession
```

The new approach:
- **Eliminates nested Either types** that were confusing
- **Centralizes error handling** in the runner functions
- **Provides cleaner composition** without explicit error propagation
- **Maintains the same safety guarantees** through different mechanisms
