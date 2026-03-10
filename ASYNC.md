# Cardano DB-Sync Asynchronous Optimization Strategy

## Executive Summary

**Current State:** cardano-db-sync processes blocks in a strictly sequential manner during initial sync, with a single database connection handling all insertions within large transactions. A 4-connection pool exists but sits idle during the critical path.

**Key Insight:** During initial sync (SyncLagging mode), db-sync is processing blocks that are millions of slots behind the chain tip - these blocks are **already finalized** and can never be rolled back. This changes the safety constraints and opens up parallelization opportunities that aren't safe when following the tip.

**Foundation:** Two recent PRs provide critical infrastructure:
- **PR #2076** (merged): Mode-aware isolation levels via `determineIsolationLevel`
- **PR #2079** (in review): Pipelining for epoch stake/reward inserts, batch cache lookups

These PRs validate our approach and provide patterns we'll extend throughout the codebase.

**Recommendation Priority:**
1. **CRITICAL (40-60% improvement):** Implement mode-adaptive batching: per-block transactions during SyncLagging, batch transactions when SyncFollowing
2. **HIGH (20-40% improvement):** Parallel bulk inserts using connection pool (enabled by Phase 1)
3. **HIGH (20-40% improvement):** Two-stage pipeline separating ledger application from DB insertion  
4. **MEDIUM (10-20% improvement):** Parallelize independent operations within blocks

---

## 1. Current Architecture & Bottlenecks

### 1.1 Thread Topology (Already Optimal)

The system runs 5 concurrent threads:
- **Thread 1:** Database writer (single connection) - **THE BOTTLENECK**
- **Thread 2:** ChainSync client (receives blocks from node)
- **Thread 3-5:** Off-chain metadata & ledger snapshot writers (fully independent)

Threads 1 & 2 form a strict producer-consumer pipeline with a 47-element bounded queue (TBQueue) between them.

**Why 47?** From `DbEvent.hs:231-238`:
```haskell
-- Use an odd number here so that the db_tip_height metric increments by this odd number
-- when syncing, instead of incrementing by say 100.
-- The pipeline queue in the LocalChainSync machinery is 50 elements long
-- so we should not exceed that.
ThreadChannels <$> TBQ.newTBQueueIO 47
```

The ChainSync protocol pipeline supports up to 50 in-flight blocks (`pipelineDecisionLowHighMark 1 50` in Sync.hs:300). The queue capacity of 47 stays below this limit while using an odd number for better metrics visibility.

### 1.2 Critical Sequential Bottlenecks

**Block-Level Sequential Processing:**
```haskell
-- Default.hs:60 - THE PRIMARY BOTTLENECK
traverse_ (applyAndInsertBlockMaybe syncEnv tracer) blocks
```
Each block in the 47-block batch waits for the previous block to completely finish before starting.

**Transaction-Level Sequential Processing:**
```haskell
-- Block.hs:100 - SECONDARY BOTTLENECK  
blockGroupedData <- foldM (\gp (idx, tx) -> txInserter idx tx gp) mempty zippedTx
```
Each transaction within a block is processed sequentially using `foldM`.

**Within-Transaction Sequential Operations:**
- Certificate insertions: `mapM_` (Tx.hs:175-176)
- Withdrawals: `mapM_` (Tx.hs:179)
- Input resolution: `mapM` (Tx.hs:93, 97, 106)
- Output insertion: `mapM` (Tx.hs:145, 154)
- Scripts, redeemers, governance proposals: all sequential

### 1.3 Transaction Boundary Analysis - The Core Problem

**Current Behavior: One-Size-Fits-All Batching**

The current implementation uses a **single batching strategy** for both sync modes:

```haskell
-- Default.hs:56-60 - SAME FOR BOTH MODES
insertListBlocks syncEnv blocks = do
  isolationLevel <- determineIsolationLevel syncEnv  -- From PR #2076
  runDbSyncTransaction (getTrace syncEnv) (envDbEnv syncEnv) isolationLevel $ do
    traverse_ (applyAndInsertBlockMaybe syncEnv tracer) blocks  -- All blocks in ONE transaction
```

**What Actually Happens:**

| Sync Mode | Queue State | Typical Batch Size | Transaction Behavior |
|-----------|-------------|-------------------|---------------------|
| **SyncLagging** | Queue fills rapidly | 20-47 blocks | ONE transaction for entire batch |
| **SyncFollowing** | Queue nearly empty | 1-3 blocks | ONE transaction for 1-3 blocks |

**The Problem: Different Modes Have Conflicting Needs**

**During SyncLagging (Historical Sync):**
- Blocks are **finalized** (millions of slots old, cannot be rolled back)
- Processing 20-47 blocks at maximum speed
- **Large batch transaction HURTS:**
  - Single DB connection monopoly for 5-10 seconds
  - Connection pool (4 connections) sits completely idle
  - Cannot parallelize ANY DB operations
  - Large transaction → excessive WAL pressure, long lock holds
  - Less granular crash recovery

**During SyncFollowing (Following Tip):**
- Blocks are **NOT finalized** (within k=2160 rollback window)
- Processing 1-3 blocks slowly (~20 seconds between blocks)
- **Small batch transaction is GOOD:**
  - Atomicity provides rollback safety
  - Transaction is naturally small (1-3 blocks)
  - No parallelization needed (low throughput)
  - Consistency guarantees maintained

**Key Insight:**

Your code **already recognizes** these modes are different (PR #2076):

```haskell
-- Api.hs:254-260 - Mode-aware isolation level (from PR #2076)
determineIsolationLevel :: SyncEnv -> IO (Maybe DB.IsolationLevel)
determineIsolationLevel syncEnv = do
  syncState <- readTVarIO (envDbIsolationState syncEnv)
  pure $ case syncState of
    DB.SyncLagging   -> Just DB.ReadCommitted   -- Fast, less strict
    DB.SyncFollowing -> Nothing                  -- Safe, RepeatableRead
```

**But it doesn't go far enough!** The code changes isolation level but keeps the same batching strategy. We need to make the **batching strategy also adaptive**.

---

## 2. Existing Parallelization (Limited)

The codebase DOES have some parallelization:

### 2.1 CPU-Bound Preparation (Grouped.hs:102-115)
```haskell
a1 <- async $ pure $ prepareTxInProcessing syncEnv grouped
a2 <- async $ pure $ prepareMetadataProcessing syncEnv grouped  
a3 <- async $ pure $ prepareMintProcessing syncEnv grouped
a4 <- async $ do -- TxOut chunking
```
**BUT:** These are pure CPU operations for chunking data, NOT database operations.

### 2.2 Hasql Pipelining (Limited Use)
PostgreSQL pipelining exists for some bulk operations:
- `insertBulkEpochStakePiped` (StakeDelegation.hs:98-101)
- `insertBulkTxOutPiped` (TxOut.hs:139-179)

**Pipelining allows** sending multiple SQL statements without waiting for responses, but still runs on single connection.

---

## 3. Prerequisites and Foundation (PRs #2076 & #2079)

### 3.1 Overview

Two recent pull requests establish critical infrastructure and patterns that validate our entire optimization approach:

- **PR #2076** (merged to master) - Isolation level management and mode-aware behavior  
- **PR #2079** (in review) - Pipelining infrastructure and batch cache operations

Together, these PRs demonstrate that the team **already embraces** the concepts we're proposing to extend.

### 3.2 PR #2076: Mode-Aware Isolation Levels

**Pull Request:** [#2076 - fix isolation commits](https://github.com/IntersectMBO/cardano-db-sync/pull/2076)  
**Status:** Merged to master  
**Impact:** Establishes mode-aware infrastructure we'll build upon

**Key Changes:**

1. **`determineIsolationLevel` made public** (`Api.hs:254-260`)
   ```haskell
   determineIsolationLevel :: SyncEnv -> IO (Maybe DB.IsolationLevel)
   determineIsolationLevel syncEnv = do
     syncState <- readTVarIO (envDbIsolationState syncEnv)
     pure $ case syncState of
       DB.SyncLagging   -> Just DB.ReadCommitted   -- Fast
       DB.SyncFollowing -> Nothing                  -- Safe (RepeatableRead)
   ```

2. **`runDbTransSilent` signature changed** (requires explicit `IsolationLevel`)

3. **Off-chain threads use dynamic isolation level** (pattern we'll replicate)

### 3.3 PR #2079: Pipelining and Batch Operations

**Pull Request:** [#2079 - Pipeline stake reward](https://github.com/IntersectMBO/cardano-db-sync/pull/2079)  
**Status:** Draft (in review)  
**Impact:** Demonstrates pipelining benefits and batch cache patterns

**Key Changes:**

1. **Renamed `*Piped` to `*Chunked`** - More accurate (chunking + pipelining)

2. **Batch cache lookups** (`Cache.hs` - **Critical for Phase 2!**)
   - `queryOrInsertStakeAddressBatch` - Pipeline stake address lookups
   - `queryPoolKeyOrInsertBatch` - Pipeline pool hash lookups
   - Pattern: Check cache → pipeline all misses → insert any not found

3. **Pipelined epoch stake/reward inserts** (`Epoch.hs`)
   ```haskell
   -- NEW: Batch pipeline all lookups
   saIds <- queryOrInsertStakeAddressBatch syncEnv UpdateCacheStrong nw (map fst chunk)
   poolIds <- queryPoolKeyOrInsertBatch syncEnv UpdateCache (map (snd . snd) chunk)
   ```

### 3.4 Why These PRs Matter

**PR #2076 provides:**
- ✅ Mode detection infrastructure (`determineIsolationLevel`)  
- ✅ Validation that mode-aware behavior is accepted  
- ✅ Foundation for adaptive strategies

**PR #2079 provides:**
- ✅ Pipelining infrastructure and patterns  
- ✅ Batch cache lookup examples  
- ✅ Proven performance benefits from parallelization

**We're not inventing new patterns - we're applying existing patterns more broadly.**

---

## 4. The Finality Insight (Key to Unlock Parallelism)

### 3.1 Why SyncLagging is Different

During initial sync, db-sync is processing blocks from months/years ago. Cardano's Ouroboros consensus guarantees blocks older than `k = 2160` slots (~12 hours) are **immutably finalized**.

**Current constraints that can be relaxed during SyncLagging:**

| Constraint | Following Tip (Required) | Lagging Sync (Can Change) |
|------------|-------------------------|--------------------------|
| Transaction isolation | `RepeatableRead` | ✅ Already uses `ReadCommitted` |
| Single batch transaction | Yes (rollback safety) | ❌ Can use per-block transactions |
| Single connection | Yes (cache coherence) | ❌ Can use pool for independent ops |
| Ledger snapshots | Every epoch | ✅ Already skipped during lagging |
| DB commits per batch | Multiple (at snapshots) | ✅ Already none (or can do per-block) |

### 3.2 Safety Analysis for Per-Block Commits

**Crash Recovery Path:**
1. On restart, db-sync queries latest block in DB
2. Loads latest ledger snapshot from disk
3. Sends latest point to cardano-node
4. Node responds with rollback OR next block
5. `isConsistent` check handles reconciliation

**Per-block commits during SyncLagging are safe because:**
- Blocks being inserted are already millions of slots old = finalized
- Crash causes replay from last committed DB state + last ledger snapshot
- Existing `isConsistent` / `rollbackFromBlockNo` handles post-crash state
- No rollback will ever reach blocks that far behind tip

---

## 4. Proposed Architectural Improvements

### PHASE 1: Mode-Adaptive Batching Strategy (HIGHEST ROI - 40-60% improvement)

**Priority:** CRITICAL  
**Complexity:** LOW  
**Risk Level:** LOW  
**Estimated Time:** 1-2 weeks  
**Foundation:** PR #2076 already provides `determineIsolationLevel` infrastructure

**Files to modify:**
- `cardano-db-sync/src/Cardano/DbSync/Default.hs`

#### Current Implementation (Post PR #2076)
```haskell
-- Default.hs:56-60 - One-size-fits-all batching
insertListBlocks syncEnv blocks = do
  isolationLevel <- determineIsolationLevel syncEnv  -- From PR #2076
  runDbSyncTransaction (getTrace syncEnv) (envDbEnv syncEnv) isolationLevel $ do
    traverse_ (applyAndInsertBlockMaybe syncEnv tracer) blocks  -- ALL blocks in ONE transaction
```

This processes 47 blocks during SyncLagging and 1-3 blocks during SyncFollowing, but always in a single transaction.

#### Proposed Implementation: Mode-Adaptive Batching

```haskell
-- Default.hs:56-xx - Mode-adaptive batching strategy
insertListBlocks :: SyncEnv -> [CardanoBlock] -> IO (Either SyncNodeError ())
insertListBlocks syncEnv blocks = do
  isolationLevel <- determineIsolationLevel syncEnv  -- Already exists from PR #2076
  syncState <- readTVarIO (envDbIsolationState syncEnv)
  
  case syncState of
    DB.SyncLagging -> do
      -- Historical sync: Per-block transactions for parallelization
      -- These blocks are FINALIZED (millions of slots old, no rollback risk)
      forM_ blocks $ \blk ->
        runDbSyncTransaction (getTrace syncEnv) (envDbEnv syncEnv) isolationLevel $
          applyAndInsertBlockMaybe syncEnv (getTrace syncEnv) blk
        
    DB.SyncFollowing -> do
      -- Following tip: Batch transaction for rollback safety
      -- These blocks are NOT finalized (within k=2160, rollback possible)
      runDbSyncTransaction (getTrace syncEnv) (envDbEnv syncEnv) isolationLevel $
        traverse_ (applyAndInsertBlockMaybe syncEnv (getTrace syncEnv)) blocks
```

**Why This Works:**

The key insight is that `isolationLevel` from `determineIsolationLevel` already differentiates:
- `SyncLagging` → `Just ReadCommitted` (fast)
- `SyncFollowing` → `Nothing` → defaults to `RepeatableRead` (safe)

We're simply extending this mode-awareness to the **batching strategy** itself.

#### Required Changes to `commitOrIndexes`

**Location:** `Default.hs:231-247`

**Current Logic:**
```haskell
-- Default.hs:231-247 (current)
commitOrIndexes :: Bool -> Bool -> ExceptT SyncNodeError DB.DbM ()
commitOrIndexes withinTwoMin withinHalfHour = do
  commited <-
    if withinTwoMin || tookSnapshot
      then do
        lift $ DB.transactionSaveWithIsolation DB.RepeatableRead
        pure True
      else pure False
  when withinHalfHour $ do
    bootStrapMaybe syncEnv
    ranIndexes <- liftIO $ getRanIndexes syncEnv
    addConstraintsIfNotExist syncEnv tracer
    unless ranIndexes $ do
      unless commited $ lift $ DB.transactionSaveWithIsolation DB.RepeatableRead
      liftIO $ runNearTipMigrations syncEnv
```

**Problem:** During SyncLagging with per-block transactions, `commitOrIndexes` is called within each block's transaction. The mid-transaction commit (`transactionSaveWithIsolation`) becomes **redundant** because the outer per-block transaction will commit anyway.

**Updated Logic:**
```haskell
commitOrIndexes :: Bool -> Bool -> ExceptT SyncNodeError DB.DbM ()
commitOrIndexes withinTwoMin withinHalfHour = do
  syncState <- liftIO $ readTVarIO (envDbIsolationState syncEnv)
  
  case syncState of
    DB.SyncLagging -> 
      -- Per-block outer transaction already commits after each block
      -- Only handle indexes/constraints here (no mid-transaction commit)
      when withinHalfHour $ do
        bootStrapMaybe syncEnv
        ranIndexes <- liftIO $ getRanIndexes syncEnv
        addConstraintsIfNotExist syncEnv tracer
        unless ranIndexes $ liftIO $ runNearTipMigrations syncEnv
        
    DB.SyncFollowing -> do
      -- Original behavior: mid-transaction commits for large batches
      commited <-
        if withinTwoMin || tookSnapshot
          then do
            lift $ DB.transactionSaveWithIsolation DB.RepeatableRead
            pure True
          else pure False
      when withinHalfHour $ do
        bootStrapMaybe syncEnv
        ranIndexes <- liftIO $ getRanIndexes syncEnv
        addConstraintsIfNotExist syncEnv tracer
        unless ranIndexes $ do
          unless commited $ lift $ DB.transactionSaveWithIsolation DB.RepeatableRead
          liftIO $ runNearTipMigrations syncEnv
```

#### Benefits

**For SyncLagging (Historical Sync):**
- ✅ Shorter transactions → faster commit, earlier lock releases
- ✅ Enables Phase 2 (pool parallelism) - **critical unlock**
- ✅ Reduced WAL pressure (smaller transactions)
- ✅ Better progress visibility (commit every block)
- ✅ More granular crash recovery (lose at most 1 block, not 47)
- ✅ Expected **40-60% performance improvement**

**For SyncFollowing (Following Tip):**
- ✅ **No change** to existing behavior
- ✅ Rollback safety preserved (batch transaction)
- ✅ Consistency guarantees maintained
- ✅ Zero regression risk

#### Risks & Mitigations

**Risk 1: More commits = more overhead**
- **Impact:** LOW - Only during SyncLagging (finalized blocks)
- **Mitigation:** Smaller transactions actually reduce overhead (less lock contention, smaller WAL entries)
- **Measurement:** Benchmark blocks/second before and after

**Risk 2: Crash recovery with partial blocks**
- **Impact:** LOW - `isConsistent` already handles this
- **Mitigation:** Existing crash recovery path unchanged. On restart:
  1. Query latest block in DB
  2. Load latest ledger snapshot
  3. Send point to cardano-node
  4. Node sends rollback or next block
- **Testing:** Crash scenarios at various block boundaries

**Risk 3: Change in transaction semantics**
- **Impact:** NONE for SyncFollowing
- **Mitigation:** Only changes SyncLagging path; SyncFollowing unchanged
- **Testing:** Full sync comparison (sequential vs adaptive batching)

**Risk 4: Mid-transaction commit redundancy**
- **Impact:** LOW - Could cause unnecessary COMMITs during SyncLagging
- **Mitigation:** Updated `commitOrIndexes` to skip commits when in SyncLagging mode
- **Testing:** Verify no double commits via PostgreSQL logs

#### Testing Strategy

**Unit Tests:**
```haskell
-- Test mode detection
testDetermineIsolationLevel :: Spec
testDetermineIsolationLevel = do
  describe "determineIsolationLevel" $ do
    it "returns ReadCommitted for SyncLagging" $ do
      -- Test implementation
    it "returns Nothing (RepeatableRead) for SyncFollowing" $ do
      -- Test implementation

-- Test batching strategy selection
testInsertListBlocks :: Spec
testInsertListBlocks = do
  describe "insertListBlocks batching strategy" $ do
    it "uses per-block transactions during SyncLagging" $ do
      -- Verify forM_ path taken
    it "uses batch transaction during SyncFollowing" $ do
      -- Verify traverse_ path taken
```

**Integration Tests:**

1. **Full Sync Comparison:**
   - Run testnet sync from genesis with adaptive batching
   - Compare against baseline sync (sequential batching)
   - Verify identical final database state (every row, every table)
   - Measure performance improvement

2. **Crash Recovery Scenarios:**
   - Kill process during SyncLagging at various block numbers
   - Verify restart succeeds from last committed block
   - Kill process during SyncFollowing
   - Verify no data corruption

3. **Mode Transition:**
   - Start sync from genesis (SyncLagging)
   - Let it catch up to within 2 minutes (transition to SyncFollowing)
   - Verify smooth transition
   - Verify no errors or data inconsistencies

4. **Performance Benchmarks:**
   ```bash
   # Measure blocks/second during different phases
   # Before optimization:
   $ cardano-db-sync ... | grep "blocks/sec"
   
   # After Phase 1:
   $ cardano-db-sync --enable-adaptive-batching | grep "blocks/sec"
   ```

5. **PostgreSQL Monitoring:**
   ```sql
   -- Monitor transaction sizes
   SELECT COUNT(*), AVG(xact_commit - xact_rollback) 
   FROM pg_stat_database 
   WHERE datname = 'cexplorer';
   
   -- Monitor WAL generation
   SELECT pg_current_wal_lsn() - '0/0'::pg_lsn as wal_bytes;
   ```

6. **Database Consistency Validation:**
   ```sql
   -- Verify foreign key integrity
   SELECT * FROM block WHERE previous_id IS NOT NULL 
     AND previous_id NOT IN (SELECT id FROM block);
   
   -- Verify tx input/output consistency
   SELECT * FROM tx_in WHERE tx_out_id NOT IN (SELECT id FROM tx_out);
   ```

---

### PHASE 2: Parallel Bulk Inserts via Pool (HIGH ROI - adds 20-30% on top of Phase 1)

**Priority:** HIGH  
**Complexity:** MEDIUM  
**Risk Level:** MEDIUM  
**Estimated Time:** 2-3 weeks  
**Prerequisite:** Phase 1 must be implemented first  
**Foundation:** Connection pool infrastructure already exists (4-connection pool)

**Files to modify:**
- `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Grouped.hs`
- `cardano-db-sync/src/Cardano/DbSync/DbEvent.hs`
- `cardano-db-sync/src/Cardano/DbSync/Api.hs` (add helper functions)

#### Current Implementation

**Location:** `Grouped.hs:117-123`

After `insertBulkTxOut` generates TxOutIds, these operations run **sequentially** on one connection:
```haskell
txOutIds <- concat <$> mapM (lift . DB.insertBulkTxOut disInOut) txOutChunks

-- All sequential on main connection
executePreparedTxInPiped prepTxIn       -- Independent
executePreparedMetadataPiped prepMeta   -- Independent  
executePreparedMintPiped prepMint       -- Independent
processMaTxOuts syncEnv txOutIds grouped -- Depends on TxOutIds
processUtxoConsumption txOutIds minIdsC  -- Depends on TxOutIds
```

#### Proposed Implementation

**Step 1: Add helper function in Api.hs**
```haskell
-- Api.hs - New helper for mode-aware parallel execution
shouldUseParallelInserts :: SyncEnv -> IO Bool
shouldUseParallelInserts syncEnv = do
  syncState <- readTVarIO (envDbIsolationState syncEnv)
  pure $ case syncState of
    DB.SyncLagging -> True   -- Safe: finalized blocks
    DB.SyncFollowing -> False -- Unsafe: within rollback window
```

**Step 2: Update insertBlockGroupedData in Grouped.hs**
```haskell
-- Grouped.hs:94-128 - Mode-adaptive parallel bulk inserts
insertBlockGroupedData :: 
  SyncEnv -> 
  BlockGroupedData -> 
  ExceptT SyncNodeError DB.DbM MinIds
insertBlockGroupedData syncEnv grouped = do
  let tracer = getTrace syncEnv
  let dbEnv = envDbEnv syncEnv
  let disInOut = getDisableInOut $ getInsertOptions syncEnv
  
  -- Parallel preparation (already exists - CPU-bound)
  (prepTxIn, prepMeta, prepMint, txOutChunks) <- liftIO $ do
    a1 <- async $ pure $ prepareTxInProcessing syncEnv grouped
    a2 <- async $ pure $ prepareMetadataProcessing syncEnv grouped
    a3 <- async $ pure $ prepareMintProcessing syncEnv grouped
    a4 <- async $ chunkForBulkQueryWith (Proxy @DB.TxOut) Nothing (groupedTxOut grouped)
    (,,,) <$> wait a1 <*> wait a2 <*> wait a3 <*> wait a4
  
  -- Step 1: Insert TxOut (generates IDs needed by later operations)
  txOutIds <- concat <$> mapM (lift . DB.insertBulkTxOut disInOut) txOutChunks
  
  -- Step 2: Check if we should parallelize (mode-aware)
  useParallel <- liftIO $ shouldUseParallelInserts syncEnv
  
  case useParallel of
    True -> do
      -- SyncLagging: Parallel execution using pool connections
      -- These operations are independent and can run concurrently
      liftIO $ concurrently3
        (runDbPoolTransLogged tracer dbEnv (Just DB.ReadCommitted) $ 
           executePreparedTxInPiped prepTxIn)
        (runDbPoolTransLogged tracer dbEnv (Just DB.ReadCommitted) $ 
           executePreparedMetadataPiped prepMeta)
        (runDbPoolTransLogged tracer dbEnv (Just DB.ReadCommitted) $ 
           executePreparedMintPiped prepMint)
      
      -- Main connection handles TxOut-dependent work
      processMaTxOuts syncEnv txOutIds grouped
      minIdsC <- processUtxoConsumption txOutIds
      pure minIdsC
      
    False -> do
      -- SyncFollowing: Sequential execution for safety
      executePreparedTxInPiped prepTxIn
      executePreparedMetadataPiped prepMeta
      executePreparedMintPiped prepMint
      processMaTxOuts syncEnv txOutIds grouped
      minIdsC <- processUtxoConsumption txOutIds
      pure minIdsC
```

**Key Changes:**
1. Added `shouldUseParallelInserts` helper (mode detection)
2. Made `runDbPoolTransLogged` use explicit `ReadCommitted` (per PR #2076 signature)
3. Clear separation between parallel (SyncLagging) and sequential (SyncFollowing) paths

#### Why This Works

1. **Phase 1 ensures prior block's TxOuts are committed** → visible to pool workers
2. **TxIn references prior blocks' TxOuts** (now visible from committed transactions)
3. **Metadata & Mint are fully independent** - no foreign key dependencies on current block
4. **No cache writes in these operations** - only bulk SQL inserts
5. **Each pool worker uses separate transaction** - isolated from main transaction

#### Cache Coherence Analysis

**Safe operations for pool parallelization:**
- `executePreparedTxInPiped`: Bulk INSERT into `tx_in` table only
- `executePreparedMetadataPiped`: Bulk INSERT into `tx_metadata` table only
- `executePreparedMintPiped`: Bulk INSERT into `ma_tx_mint` table only

**None of these touch `CacheStatus`** - they only perform bulk SQL inserts of pre-prepared data.

**Operations that MUST stay on main connection:**
- `processMaTxOuts`: References TxOutIds, updates multi-asset tables
- `processUtxoConsumption`: Updates `tx_out.consumed_by_tx_in_id`
- Any operation that writes to cache

#### Benefits
- 3-4x parallelism for bulk operations
- Main connection freed for dependent work
- Connection pool finally utilized (3 of 4 connections active)
- No change to data model or semantics

#### Risks & Mitigations
- **Risk:** Pool connection exhaustion
  - **Mitigation:** Bounded concurrency (exactly 3 concurrent operations)
- **Risk:** Pool worker transaction failures
  - **Mitigation:** Proper error handling with `concurrently3`, rollback on failure
- **Risk:** Cache coherence issues
  - **Mitigation:** Only parallelize non-cache-writing operations
- **Risk:** Transaction isolation issues
  - **Mitigation:** Each pool worker has its own transaction; Phase 1 ensures visibility

#### Testing Strategy
1. Verify TxIn foreign key constraints satisfied (references exist)
2. Database consistency checks on completion
3. Stress test with high-transaction blocks
4. Monitor pool connection utilization
5. Compare database state with sequential implementation
6. Test failure scenarios (pool exhaustion, worker failures)

---

### PHASE 3: Two-Stage Ledger/DB Pipeline (MEDIUM-HIGH ROI - 20-40% improvement)

**Priority:** HIGH (but optional)  
**Complexity:** HIGH  
**Risk Level:** MEDIUM  
**Estimated Time:** 3-4 weeks  
**Prerequisite:** Phase 1

**Files to modify:**
- `cardano-db-sync/src/Cardano/DbSync/Default.hs` (significant refactor)

#### Current Implementation

For each block, ledger application and DB insertion run sequentially:
```
Block N:   [applyBlock (30ms CPU)] → [insertBlock (100ms DB I/O)]
Block N+1: [applyBlock (30ms CPU)] → [insertBlock (100ms DB I/O)]
```

**CPU sits idle during DB I/O. DB connection idle during ledger apply.**

#### Proposed Implementation

Two-stage pipeline with bounded queue:

```haskell
-- New pipeline types
data PipelineItem = PipelineItem
  { piBlock :: !CardanoBlock
  , piApplyResult :: !ApplyResult
  , piTookSnapshot :: !Bool
  , piDetails :: !SlotDetails
  }

-- Stage 1: Ledger application thread
ledgerApplyStage :: 
  SyncEnv -> 
  [CardanoBlock] -> 
  TBQueue PipelineItem -> 
  IO (Either SyncNodeError ())
ledgerApplyStage syncEnv blocks queue = runExceptT $ do
  forM_ blocks $ \blk -> do
    details <- liftIO $ getSlotDetails syncEnv blk
    (!applyRes, !tookSnapshot) <- liftIO $ mkApplyResult syncEnv blk
    liftIO $ atomically $ writeTBQueue queue $ 
      PipelineItem blk applyRes tookSnapshot details

-- Stage 2: DB insertion thread
dbInsertStage :: 
  SyncEnv -> 
  TBQueue PipelineItem -> 
  IO (Either SyncNodeError ())
dbInsertStage syncEnv queue = runExceptT $ forever $ do
  item <- liftIO $ atomically $ readTBQueue queue
  runDbSyncTransaction tracer dbEnv (Just DB.ReadCommitted) $ 
    insertBlock syncEnv (piBlock item) (piApplyResult item) 
      (piDetails item) (piTookSnapshot item)

-- Main coordinator (SyncLagging only)
insertListBlocksPipelined :: 
  SyncEnv -> 
  [CardanoBlock] -> 
  IO (Either SyncNodeError ())
insertListBlocksPipelined syncEnv blocks = do
  -- Safety check: ensure DB is consistent before starting pipeline
  isConsist <- isConsistent syncEnv
  unless isConsist $ 
    throwIO $ SNErrDefault mkSyncNodeCallStack "Database inconsistent, cannot use pipeline"
  
  -- Create bounded queue (depth 4 = tunable parameter)
  queue <- newTBQueueIO 4
  
  -- Run both stages concurrently
  result <- race
    (ledgerApplyStage syncEnv blocks queue)
    (dbInsertStage syncEnv queue)
  
  case result of
    Left (Left err) -> pure $ Left err   -- Ledger stage error
    Left (Right _) -> do
      -- Ledger stage finished, drain queue for DB stage
      atomically $ writeTBQueue queue PipelineTerminate
      waitForDbStage
    Right (Left err) -> pure $ Left err  -- DB stage error
    Right (Right _) -> error "DB stage should not finish first"

-- Modified insertListBlocks to use pipeline conditionally
insertListBlocks ::
  SyncEnv ->
  [CardanoBlock] ->
  IO (Either SyncNodeError ())
insertListBlocks syncEnv blocks = do
  syncState <- readTVarIO (envDbIsolationState syncEnv)
  usePipeline <- readTVarIO (envUsePipeline syncEnv)  -- Config flag
  
  case (syncState, usePipeline) of
    (DB.SyncLagging, True) -> insertListBlocksPipelined syncEnv blocks
    _ -> insertListBlocksSequential syncEnv blocks  -- Original implementation
```

#### Why This Works

- **`applyBlock` only touches `leStateVar :: TVar LedgerDB`** (STM-safe)
- **`ApplyResult` is immutable once produced** - safe to pass between threads
- **DB insert only reads `ApplyResult`** - no feedback from DB into ledger state
- **No shared mutable state between stages** except the bounded queue
- **Consistency guard**: `isConsistent` check before starting pipeline

#### Pipeline Depth Tuning

```haskell
-- Configurable queue depth
data PipelineConfig = PipelineConfig
  { pcQueueDepth :: !Int        -- Default: 4
  , pcEnabled :: !Bool          -- Default: False (opt-in)
  , pcMinBlocksForPipeline :: !Int  -- Default: 10 (don't pipeline small batches)
  }
```

**Queue depth considerations:**
- Too small (1-2): Not enough buffering, stages wait on each other
- Too large (>8): Excessive memory usage, harder crash recovery
- Sweet spot: 4-6 blocks

#### Benefits
- Overlaps CPU-bound ledger work (~30ms) with DB I/O (~100ms)
- Recovers 20-40% wall-clock time during historical sync
- Natural backpressure via bounded queue
- Ledger state application doesn't wait for DB

#### Risks & Mitigations
- **Risk:** More complex control flow
  - **Mitigation:** Clear separation of stages, comprehensive testing
- **Risk:** Error propagation across threads
  - **Mitigation:** Use `race` or `concurrently` with proper exception handling
- **Risk:** Shutdown handling complexity
  - **Mitigation:** Graceful termination with sentinel values, timeout handling
- **Risk:** Cache access patterns
  - **Mitigation:** Verify cache is only written in DB stage, read-only in ledger stage
- **Risk:** Memory usage increase
  - **Mitigation:** Bounded queue depth, monitor memory consumption
- **Risk:** Crash recovery more complex
  - **Mitigation:** Drain queue on shutdown, `isConsistent` check on restart

#### Configuration

Add to `SyncNodeConfig`:
```haskell
data SyncInsertOptions = SyncInsertOptions
  { ...
  , sioUsePipeline :: !Bool           -- Enable two-stage pipeline
  , sioPipelineQueueDepth :: !Int     -- Pipeline queue depth (default 4)
  , sioMinBlocksForPipeline :: !Int   -- Min blocks to activate pipeline (default 10)
  }
```

#### Testing Strategy
1. Full sync from genesis with pipeline enabled/disabled comparison
2. Crash scenarios at various pipeline stages
3. Memory profiling (queue growth, GC pressure)
4. CPU utilization monitoring (verify CPU/IO overlap)
5. Database consistency validation
6. Small batch handling (< 10 blocks)
7. Error injection testing (simulated failures in each stage)

---

### PHASE 4: Bulk Insert Certificates & Withdrawals (MEDIUM ROI - 10-15% improvement)

**Priority:** MEDIUM  
**Complexity:** MEDIUM-HIGH  
**Risk Level:** MEDIUM-HIGH  
**Estimated Time:** 3-4 weeks  
**Prerequisite:** None (independent of other phases)

**Files to modify:**
- `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Grouped.hs`
- `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Tx.hs`
- `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs`
- `cardano-db/src/Cardano/Db/Statement/Certificate.hs` (new file)

#### Current Implementation

**Location:** `Tx.hs:175-176`

Per-transaction, per-certificate insertion:
```haskell
mapM_ (insertCertificate syncEnv blkId txId epochNo redeemers) $
  Generic.txCertificates tx

mapM_ (insertWithdrawals syncEnv txId redeemers) $
  Generic.txWithdrawals tx
```

Each certificate/withdrawal causes:
1. Cache lookup for stake address
2. Individual INSERT statement
3. Round-trip to database

#### Proposed Implementation

**Step 1:** Extend `BlockGroupedData`:
```haskell
data BlockGroupedData = BlockGroupedData
  { groupedTxOut :: ![(ExtendedTxOut, [ExtendedTxIn])]
  , groupedTxIn :: ![ExtendedTxIn]
  , groupedTxMetadata :: ![TxMetadata]
  , groupedMaTxMint :: ![MaTxMint]
  , groupedTxFees :: !Word64
  , groupedTxOutSum :: !Word64
  
  -- NEW: Accumulated certificates and withdrawals
  , groupedCerts :: ![PreparedCertificate]
  , groupedWithdrawals :: ![PreparedWithdrawal]
  , groupedScripts :: ![Script]
  , groupedRedeemers :: ![Redeemer]
  , groupedGovActions :: ![PreparedGovAction]
  }

data PreparedCertificate = PreparedCertificate
  { pcTxId :: !DB.TxId
  , pcCertIndex :: !Word16
  , pcStakeAddressId :: !DB.StakeAddressId  -- Pre-resolved
  , pcPoolHashId :: !(Maybe DB.PoolHashId)  -- Pre-resolved
  , pcCertType :: !CertificateType
  , pcDetails :: !CertificateDetails
  }
```

**Step 2:** Separate Lookup Phase from Insert Phase

In `insertTx`:
```haskell
-- OLD: immediate insert
mapM_ (insertCertificate syncEnv blkId txId epochNo redeemers) certs

-- NEW: prepare for bulk insert
preparedCerts <- mapM (prepareCertificate syncEnv blkId txId epochNo redeemers) certs
return $ grouped <> mempty { groupedCerts = preparedCerts }
```

**Step 3:** Add Bulk Insert Functions

New file: `cardano-db/src/Cardano/Db/Statement/Certificate.hs`
```haskell
insertBulkCertificates :: 
  DisableInOut -> 
  [PreparedCertificate] -> 
  DbM [DB.CertificateId]
insertBulkCertificates disInOut certs = do
  -- Chunk by batch size
  let chunks = DB.chunkForBulkQuery (Proxy @DB.Certificate) Nothing certs
  concat <$> mapM insertChunk chunks
  where
    insertChunk chunk = 
      DB.insertBulkWith 
        conflictStrategy 
        certificateEncoder 
        chunk
```

#### Complexity Note

**Certificate inserts interleave cache lookups:**
- Pool registration: lookup pool hash
- Delegation: lookup stake address AND pool hash
- Stake address registration: lookup/insert stake address

**Solution Pattern (already used for TxOut):**
1. **Prepare phase**: Resolve all lookups, build `PreparedCertificate`
2. **Insert phase**: Bulk insert using UNNEST with pre-resolved IDs

This is the same pattern used in `insertTxOut` → `groupedTxOut` → `insertBulkTxOut`.

#### Benefits
- Fewer DB round-trips (one bulk INSERT vs N individual INSERTs)
- Better PostgreSQL bulk operation efficiency
- Enables better pipelining
- Certificates are common in blocks (especially delegation)

#### Risks & Mitigations
- **Risk:** Significant refactoring required
  - **Mitigation:** Incremental implementation (certs first, then withdrawals, etc.)
- **Risk:** Cache lookup/insert separation complex
  - **Mitigation:** Follow existing TxOut pattern
- **Risk:** Need to maintain ordering constraints
  - **Mitigation:** Include cert_index field in PreparedCertificate
- **Risk:** Different certificate types have different fields
  - **Mitigation:** Use sum type with per-type details

#### Testing Strategy
1. Compare certificate insertion with original (exact same IDs)
2. Test all certificate types (registration, deregistration, delegation, pool registration, etc.)
3. Cache consistency verification
4. High-certificate blocks (epoch boundaries)
5. Database foreign key constraint validation

---

### PHASE 5: Parallelize Epoch Boundary Events (LOW-MEDIUM ROI - 5-10% improvement)

**Priority:** LOW-MEDIUM  
**Complexity:** MEDIUM  
**Risk Level:** LOW-MEDIUM  
**Estimated Time:** 1-2 weeks  
**Prerequisite:** Phase 1

**Files to modify:**
- `cardano-db-sync/src/Cardano/DbSync/Default.hs`
- `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Block.hs`

#### Current Implementation

**Location:** `Default.hs:140-156`

At epoch boundaries:
```haskell
-- insertBlock function
insertNewEpochLedgerEvents syncEnv blkId applyResult details
  -- Inserts thousands of rewards, stakes, etc.

-- Then...
insertBlockUniversal syncEnv shouldLog withinTwoMins withinHalfHour blk details isMember applyResult
  -- Inserts block body
```

**Sequential execution:**
```
[insertNewEpochLedgerEvents (5-30 seconds)] → [insertBlockUniversal]
```

#### Proposed Implementation

```haskell
-- Both need ApplyResult but are independent of each other
case syncState of
  DB.SyncLagging | hasNewEpochEvents -> do
    -- Parallel execution on separate connections
    liftIO $ concurrently_
      (runDbPoolTransLogged tracer dbEnv Nothing $ 
         insertNewEpochLedgerEvents syncEnv blkId applyResult details)
      (runDbSyncTransaction tracer dbEnv (Just DB.ReadCommitted) $
         insertBlockUniversal syncEnv shouldLog withinTwoMins withinHalfHour 
           blk details isMember applyResult)
  
  _ -> do
    -- Original sequential path
    insertNewEpochLedgerEvents syncEnv blkId applyResult details
    insertBlockUniversal syncEnv shouldLog withinTwoMins withinHalfHour 
      blk details isMember applyResult
```

#### Benefits
- Epoch boundaries process faster (overlap 5-30 seconds of work)
- Occurs every ~432,000 slots (5 days) but significant when it happens
- Mainnet has many epoch boundaries during initial sync

#### Risks & Mitigations
- **Risk:** Shared state between epoch events and block body
  - **Mitigation:** Verify no dependencies (both only read ApplyResult)
- **Risk:** Error handling complexity
  - **Mitigation:** Use `concurrently_` with proper exception propagation
- **Risk:** Pool connection availability
  - **Mitigation:** Only during SyncLagging with available pool connections

#### Testing Strategy
1. Test at actual epoch boundaries (mainnet/testnet)
2. Verify both paths complete successfully
3. Database consistency at epoch boundaries
4. Performance measurement (time saved)

---

## 5. PostgreSQL Optimization Opportunities

### 5.1 Constraint Deferment During SyncLagging

**Current:** All foreign key constraints checked per-row during INSERT.

**Proposed:**
```haskell
-- In runDbSyncTransaction when SyncLagging detected
runDbSyncTransaction tracer dbEnv (Just DB.ReadCommitted) action = do
  -- Begin transaction
  HsqlS.statement () (beginTransactionStmt DB.ReadCommitted)
  
  -- Defer constraints during SyncLagging
  HsqlS.statement () deferConstraintsStmt
  
  -- Execute action
  result <- action
  
  -- Commit (constraints checked here)
  HsqlS.statement () commitTransactionStmt
  pure result
  
deferConstraintsStmt :: HsqlS.Statement () ()
deferConstraintsStmt = 
  HsqlS.Statement 
    "SET CONSTRAINTS ALL DEFERRED" 
    HsqlE.noParams 
    HsqlD.noResult 
    True
```

**Benefits:**
- Significantly faster bulk inserts (30-50% improvement)
- Constraints still enforced, just at commit time
- Safe during finalized block insertion
- No schema changes required

**Risks:**
- Constraint violations only detected at commit (later feedback)
- May increase commit time slightly

### 5.2 Disable Indexes During Initial Sync (Manual Operation)

**NOT for automatic implementation** - document as manual optimization for operators.

For truly fresh initial sync, consider:
```sql
-- Drop non-essential indexes
DROP INDEX idx_tx_out_tx_id;
DROP INDEX idx_tx_in_tx_in_id;
-- ... other indexes ...

-- Do full initial sync with db-sync

-- Rebuild indexes (can parallelize with CONCURRENTLY)
CREATE INDEX CONCURRENTLY idx_tx_out_tx_id ON tx_out(tx_id);
CREATE INDEX CONCURRENTLY idx_tx_in_tx_in_id ON tx_in(tx_in_id);
```

**Benefits:** 50-70% faster inserts during initial sync  
**Risks:** Requires manual intervention, longer rebuild time at end, queries slow during sync

### 5.3 Connection Pool Configuration

**Current:** `createHasqlConnectionPool settings 4`

**Proposed Tuning:**
```haskell
-- Dynamic pool sizing based on mode
createConnectionPool :: SyncState -> IO (Pool HsqlCon.Connection)
createConnectionPool syncState = 
  let poolSize = case syncState of
        DB.SyncLagging -> 8   -- More connections for parallel operations
        DB.SyncFollowing -> 4 -- Conservative for tip-following
  in createHasqlConnectionPool settings poolSize
```

**Or consider separate pools:**
```haskell
data DbEnv = DbEnv
  { dbConnection :: !HsqlCon.Connection       -- Main connection
  , dbMainPool :: !(Pool HsqlCon.Connection)  -- General purpose (4 connections)
  , dbBulkPool :: !(Pool HsqlCon.Connection)  -- Bulk operations (6 connections)
  , ...
  }
```

### 5.4 PostgreSQL Configuration for Bulk Loading

Recommended `postgresql.conf` settings for initial sync:

```ini
# Memory settings
shared_buffers = 8GB                    # 25% of RAM (for 32GB system)
effective_cache_size = 24GB             # 75% of RAM
maintenance_work_mem = 2GB              # For index creation
work_mem = 256MB                        # Per-operation

# Checkpoint settings (reduce frequency during bulk load)
checkpoint_completion_target = 0.9      # Spread out checkpoints
max_wal_size = 8GB                      # Reduce checkpoint frequency
min_wal_size = 2GB
wal_buffers = 16MB

# Query planner
default_statistics_target = 100
random_page_cost = 1.1                  # For SSD (lower = better)
effective_io_concurrency = 200          # For SSD

# Parallelism (if PostgreSQL 14+)
max_worker_processes = 8
max_parallel_workers_per_gather = 4
max_parallel_workers = 8
max_parallel_maintenance_workers = 4

# Connection settings (for connection pool)
max_connections = 100                   # Default 100 is usually fine
```

**After initial sync completes**, consider tuning back for query workload:
```ini
shared_buffers = 4GB                    # Less for query workload
work_mem = 64MB                         # Conservative
max_wal_size = 2GB                      # More frequent checkpoints
```

---

## 6. Implementation Strategy

### 6.1 Recommended Phasing

**Phase 1 (Week 1-2): Per-Block Transactions**
- Lowest risk, highest immediate benefit
- Foundation for all other improvements
- Can measure improvement independently
- **Deliverable:** 40-60% faster sync during SyncLagging

**Phase 2 (Week 3-4): Parallel Bulk Inserts**
- Builds on Phase 1
- Clear benefit, moderate risk
- Good test of pool connection handling
- **Deliverable:** Additional 20-30% improvement (cumulative ~2-3x)

**Phase 3 (Week 5-7): Two-Stage Pipeline**
- More complex but high value
- Consider making this optional/configurable
- Needs extensive testing
- **Deliverable:** Additional 20-40% improvement (cumulative ~3-4x)

**Phases 4-5 (Week 8+): Additional Optimizations**
- Diminishing returns
- Consider based on results of Phases 1-3
- Can be implemented independently

### 6.2 Testing Strategy

#### Unit Tests
- Connection pool behavior
- Transaction boundary correctness
- Cache coherence verification
- Prepared data structures (PreparedCertificate, etc.)

#### Integration Tests
- Full sync from genesis on testnet
- Crash/recovery scenarios at various points
- Rollback handling
- Mode transitions (SyncLagging → SyncFollowing)

#### Performance Benchmarks
- Blocks/second throughput
- Time to sync N blocks (10K, 100K, 1M)
- CPU/memory utilization
- PostgreSQL metrics:
  - WAL growth rate
  - Checkpoint frequency
  - Lock contention
  - Query latency during sync

#### Safety Tests
- Database consistency checks (foreign keys, constraints)
- Ledger state validation against reference
- Compare final database with current implementation (exact match)
- Epoch boundary correctness
- Rollback correctness

### 6.3 Rollback Plan

Each phase should be:
- **Feature-flagged:** Allow disabling via config
- **Backward compatible:** Must work with existing databases
- **Independently testable:** Can enable/disable phases individually
- **Monitored:** Metrics to detect issues early

#### Example Configuration

Add to `config/mainnet-config.yaml`:
```yaml
# Asynchronous optimization flags
AsyncOptimizations:
  # Phase 1: Per-block transactions during SyncLagging
  EnablePerBlockTransactions: true
  
  # Phase 2: Parallel bulk inserts using connection pool
  EnableParallelBulkInserts: true
  ParallelBulkInsertMaxWorkers: 3  # Max concurrent pool operations
  
  # Phase 3: Two-stage ledger/DB pipeline
  EnableLedgerDbPipeline: false     # Opt-in (more experimental)
  LedgerDbPipelineQueueDepth: 4
  LedgerDbPipelineMinBlocks: 10
  
  # Phase 4: Bulk certificate/withdrawal inserts
  EnableBulkCertificates: false
  
  # Phase 5: Parallel epoch boundary events
  EnableParallelEpochEvents: true
  
  # PostgreSQL optimizations
  EnableConstraintDeferment: true   # SET CONSTRAINTS ALL DEFERRED
  
  # Connection pool configuration
  MainPoolSize: 4                   # Main connection pool
  BulkPoolSize: 6                   # Bulk operation pool (if separate)
```

#### Runtime Flags

Support environment variables for easy testing:
```bash
# Disable all optimizations
DBSYNC_DISABLE_ASYNC_OPTS=1 cardano-db-sync ...

# Enable specific phases
DBSYNC_ENABLE_PIPELINE=1 cardano-db-sync ...
DBSYNC_PIPELINE_DEPTH=6 cardano-db-sync ...
```

---

## 7. Estimated Performance Gains

Based on architectural analysis and assuming proper implementation:

| Optimization | Conservative | Likely | Optimistic | Risk Level |
|-------------|--------------|---------|-----------|-----------|
| Phase 1: Per-block txs | +25% | +40% | +60% | LOW |
| Phase 2: Pool parallelism | +15% | +25% | +35% | MEDIUM |
| Phase 3: Ledger/DB pipeline | +15% | +30% | +50% | MEDIUM |
| Phase 4: Bulk certs/withdrawals | +5% | +10% | +15% | MEDIUM-HIGH |
| Phase 5: Parallel epoch events | +2% | +5% | +10% | LOW-MEDIUM |
| PostgreSQL tuning | +10% | +20% | +30% | LOW |

**Cumulative estimate (Phases 1-3 + PG tuning):**
- **Conservative:** 2-3x faster initial sync (66-100 hours → 22-50 hours for mainnet)
- **Likely:** 3-4x faster initial sync (66-100 hours → 16-33 hours for mainnet)
- **Optimistic:** 4-6x faster initial sync (66-100 hours → 11-25 hours for mainnet)

**Note:** These are multiplicative when combined, not additive.  
Example: 1.4x (Phase 1) × 1.25x (Phase 2) × 1.3x (Phase 3) × 1.2x (PG) = 2.73x improvement

### 7.1 Measurement Methodology

Track these metrics before/after each phase:

```haskell
-- Sync performance metrics
data SyncPerformanceMetrics = SyncPerformanceMetrics
  { spmBlocksProcessed :: !Int64
  , spmTotalTimeSeconds :: !Double
  , spmBlocksPerSecond :: !Double
  , spmAvgBlockProcessingMs :: !Double
  , spmAvgLedgerApplyMs :: !Double
  , spmAvgDbInsertMs :: !Double
  , spmDbTransactionCount :: !Int64
  , spmDbCommitCount :: !Int64
  , spmPoolConnectionsUsed :: !Int
  , spmCacheHitRate :: !Double
  }
```

---

## 8. Risks & Mitigations

### 8.1 Data Consistency Risks

**Risk:** Parallel inserts violate database consistency  
**Severity:** HIGH  
**Likelihood:** MEDIUM

**Mitigations:**
- Only parallelize operations proven to be independent
- Extensive testing with database integrity checks
- Start with SyncLagging only (finalized blocks)
- Keep SyncFollowing on safe sequential path
- Foreign key constraint validation
- Compare final database state with sequential implementation

### 8.2 Cache Coherence Risks

**Risk:** Concurrent cache access causes inconsistencies  
**Severity:** HIGH  
**Likelihood:** MEDIUM

**Mitigations:**
- Detailed audit of cache access patterns
- Only parallelize non-cache-writing operations
- Consider lock-free cache structures (CAS operations)
- Add cache consistency validation checks
- Monitor cache hit rates before/after

### 8.3 Complexity Risks

**Risk:** Code becomes harder to maintain  
**Severity:** MEDIUM  
**Likelihood:** HIGH

**Mitigations:**
- Comprehensive documentation (this document!)
- Clear separation between SyncLagging/SyncFollowing paths
- Feature flags for easy debugging
- Maintain sequential fallback path
- Code review requirements for concurrent code

### 8.4 Connection Pool Exhaustion

**Risk:** Pool connections depleted under high load  
**Severity:** MEDIUM  
**Likelihood:** LOW

**Mitigations:**
- Bounded concurrency (don't spawn unbounded workers)
- Monitor pool utilization metrics
- Graceful degradation if pool exhausted
- Configurable pool sizes
- Use semaphores or bounded queues

### 8.5 Crash Recovery Complexity

**Risk:** Crash during parallel operations leaves inconsistent state  
**Severity:** MEDIUM  
**Likelihood:** LOW

**Mitigations:**
- Rely on existing `isConsistent` check
- Phase 1 per-block transactions isolate failures
- Pool workers use independent transactions
- Drain queues on shutdown (graceful termination)
- Test crash scenarios extensively

### 8.6 Performance Regression

**Risk:** Optimizations actually slow things down  
**Severity:** LOW  
**Likelihood:** LOW

**Mitigations:**
- Benchmark before/after each phase
- Feature flags allow easy disabling
- Monitor performance metrics continuously
- A/B testing on testnet before mainnet
- Keep sequential fallback path

---

## 9. Monitoring & Metrics

### 9.1 New Metrics to Add

```haskell
-- Add to Cardano.DbSync.Metrics
data AsyncOptMetrics = AsyncOptMetrics
  { -- Performance metrics
    aomBlocksPerSecond :: !Gauge
  , aomAvgBlockProcessingMs :: !Gauge
  , aomAvgLedgerApplyMs :: !Gauge
  , aomAvgDbInsertMs :: !Gauge
  
    -- Connection pool metrics
  , aomPoolConnectionsUsed :: !Gauge
  , aomPoolConnectionsAvailable :: !Gauge
  , aomPoolWaitTimeMs :: !Histogram
  
    -- Transaction metrics
  , aomTransactionsPerBlock :: !Histogram
  , aomCommitsPerBatch :: !Counter
  , aomRollbacksPerBatch :: !Counter
  
    -- Bulk operation metrics
  , aomBulkInsertSizes :: !Histogram
  , aomBulkInsertDurationMs :: !Histogram
  , aomParallelOpsActive :: !Gauge
  
    -- Cache metrics
  , aomCacheHitRate :: !Gauge
  , aomCacheSize :: !Gauge
  , aomCacheMisses :: !Counter
  
    -- Pipeline metrics (Phase 3)
  , aomPipelineQueueDepth :: !Gauge
  , aomPipelineStallsLedger :: !Counter
  , aomPipelineStallsDb :: !Counter
  }
```

### 9.2 Logging Enhancements

Add structured logging for async operations:

```haskell
-- Log format
[INFO] [AsyncOpt] Phase 1 enabled: per-block transactions
[INFO] [AsyncOpt] Phase 2 enabled: parallel bulk inserts (3 workers)
[DEBUG] [AsyncOpt] Block batch started: 47 blocks, isolation=ReadCommitted
[DEBUG] [AsyncOpt] Block N committed: 2.3ms ledger, 45.7ms DB, 123 txs
[DEBUG] [AsyncOpt] Pool operation: TxIn bulk insert (12,543 rows, 234ms)
[WARN] [AsyncOpt] Pool connection wait: 150ms (pool exhausted?)
[ERROR] [AsyncOpt] Parallel bulk insert failed: constraint violation
```

### 9.3 Prometheus Metrics Export

```
# Performance
cardano_dbsync_blocks_per_second{phase="1"} 4.5
cardano_dbsync_block_processing_ms{quantile="0.5"} 48.2
cardano_dbsync_block_processing_ms{quantile="0.99"} 234.5

# Pool utilization
cardano_dbsync_pool_connections_active 3
cardano_dbsync_pool_connections_available 1
cardano_dbsync_pool_wait_time_ms{quantile="0.95"} 12.3

# Bulk operations
cardano_dbsync_bulk_insert_rows{table="tx_in",quantile="0.5"} 12543
cardano_dbsync_bulk_insert_duration_ms{table="tx_in",quantile="0.95"} 234

# Pipeline (Phase 3)
cardano_dbsync_pipeline_queue_depth 2
cardano_dbsync_pipeline_stalls_total{stage="ledger"} 15
cardano_dbsync_pipeline_stalls_total{stage="db"} 8
```

---

## 10. Documentation Requirements

### 10.1 User Documentation

Update the following files:

- **`doc/configuration.md`**: Document new config options
- **`doc/building-running.md`**: Add performance tuning section
- **`doc/troubleshooting.md`**: Add async optimization troubleshooting
- **`Readme.md`**: Mention async optimizations in features

### 10.2 Operator Guide

Create new file: **`doc/async-optimizations.md`**

Contents:
- Overview of async optimization features
- How to enable/disable each phase
- Expected performance improvements
- PostgreSQL tuning recommendations
- Monitoring and metrics guide
- Troubleshooting common issues
- FAQ

### 10.3 Developer Documentation

Update the following:

- **`PIPELINE.md`**: Incorporate changes from this document
- **Code comments**: Add detailed comments to modified functions
- **Architecture diagrams**: Update to show parallel paths
- **Testing guide**: Document testing requirements for concurrent code

---

## 11. Success Criteria

### 11.1 Phase 1 Success Criteria

- [ ] Per-block transactions implemented for SyncLagging mode
- [ ] SyncFollowing mode unchanged (sequential)
- [ ] Full sync from genesis on testnet completes successfully
- [ ] Database state matches reference (sequential) implementation exactly
- [ ] Performance improvement: 30-50% faster sync
- [ ] No increase in crash/recovery incidents
- [ ] Metrics dashboard shows per-block commit granularity

### 11.2 Phase 2 Success Criteria

- [ ] Parallel bulk inserts working correctly
- [ ] Connection pool showing 3-4 active connections during bulk operations
- [ ] No foreign key constraint violations
- [ ] Cache consistency maintained
- [ ] Performance improvement: Additional 20-30% (cumulative 2-2.5x)
- [ ] Pool connection monitoring shows no exhaustion

### 11.3 Phase 3 Success Criteria

- [ ] Two-stage pipeline operational (opt-in)
- [ ] Ledger and DB stages overlapping (visible in CPU/IO metrics)
- [ ] Queue depth stays within bounds (no unbounded growth)
- [ ] Graceful shutdown working correctly
- [ ] Performance improvement: Additional 20-30% (cumulative 3-4x)
- [ ] No memory leaks or increased GC pressure

### 11.4 Overall Success Criteria

- [ ] Full mainnet sync from genesis 3-4x faster than baseline
- [ ] Database consistency maintained (100% match with sequential)
- [ ] No new critical bugs introduced
- [ ] Rollback functionality unaffected
- [ ] All phases can be disabled via config
- [ ] Comprehensive monitoring and metrics in place
- [ ] Documentation complete and accurate

---

## 12. Open Questions & Discussion Points

### 12.1 Per-Block Transactions (Phase 1)

**Q1:** Should we commit every single block during SyncLagging, or batch N blocks per transaction?

**Options:**
- A) Commit every block (maximally granular, maximum parallelization potential)
- B) Commit every 5-10 blocks (balance between granularity and overhead)
- C) Make it configurable

**Recommendation:** Start with (A), make (C) configurable if overhead becomes an issue.

---

**Q2:** How do we handle the transition from SyncLagging to SyncFollowing?

**Considerations:**
- Mode can change mid-batch
- Need to drain any in-flight parallel operations
- Switch to sequential mode cleanly

**Recommendation:** Check `envDbIsolationState` at batch boundaries only, not mid-batch.

---

### 12.2 Parallel Bulk Inserts (Phase 2)

**Q3:** Which operations are truly safe to parallelize?

**Verified safe (no cache writes):**
- `executePreparedTxInPiped`
- `executePreparedMetadataPiped`
- `executePreparedMintPiped`

**Needs verification:**
- Can we parallelize `processMaTxOuts`? (Probably not - has dependencies)
- What about `insertRedeemer` operations?

**Recommendation:** Start with the three verified operations, expand after measuring.

---

**Q4:** How do we handle pool worker failures?

**Options:**
- A) Fail entire block if any worker fails (conservative)
- B) Retry failed worker operations (more complex)
- C) Fall back to sequential for that block (defensive)

**Recommendation:** Start with (A), consider (C) as enhancement.

---

### 12.3 Two-Stage Pipeline (Phase 3)

**Q5:** What's the optimal pipeline queue depth?

**Considerations:**
- Too shallow: Pipeline stalls
- Too deep: Memory usage, crash recovery complexity
- Workload dependent (block size varies)

**Recommendation:** Make configurable (default 4), add metrics to tune.

---

**Q6:** Should the pipeline be enabled by default or opt-in?

**Arguments for opt-in:**
- More complex, higher risk
- Needs more testing
- Easier rollback

**Arguments for default:**
- Significant performance benefit
- Users want fastest sync

**Recommendation:** Opt-in for first release, default-on after proven stable.

---

### 12.4 PostgreSQL Optimizations

**Q7:** Should constraint deferment be automatic or require explicit config?

**Considerations:**
- Automatic = better out-of-box performance
- Explicit = more control, less surprising behavior

**Recommendation:** Automatic during SyncLagging, document clearly.

---

**Q8:** Should we provide scripts for index management during initial sync?

**Options:**
- A) Fully automated (drop before sync, rebuild after)
- B) Manual scripts + documentation
- C) Don't recommend (too risky)

**Recommendation:** (B) - provide scripts but require manual execution.

---

### 12.5 General Questions

**Q9:** How do we ensure backwards compatibility with existing databases?

**Considerations:**
- New schema versions needed?
- Migration path for existing installations
- Can optimizations work with old schemas?

**Recommendation:** No schema changes required, optimizations work with existing DBs.

---

**Q10:** What's the testing burden for each phase?

**Estimation:**
- Phase 1: 1-2 weeks testing
- Phase 2: 2-3 weeks testing
- Phase 3: 3-4 weeks testing

**Recommendation:** Plan for equal testing and development time.

---

**Q11:** Should we implement phases on separate feature branches or one branch?

**Arguments for separate:**
- Independent testing
- Can merge phases at different times
- Easier rollback

**Arguments for single:**
- Easier to see overall picture
- Less merge conflict resolution
- Phases depend on each other

**Recommendation:** Separate feature branches that merge to a single `async-opt` integration branch.

---

## 13. Next Steps

### 13.1 Immediate Actions (Pre-Implementation)

1. **Review this document with team**
   - Get consensus on approach
   - Answer open questions
   - Assign owners for each phase

2. **Set up testing infrastructure**
   - Testnet sync environment
   - Performance benchmarking scripts
   - Metrics dashboard

3. **Create feature branch structure**
   - `feature/async-opt-phase1-per-block-txs`
   - `feature/async-opt-phase2-parallel-bulk`
   - `feature/async-opt-phase3-pipeline`
   - `feature/async-opt-integration`

4. **Baseline measurements**
   - Current sync performance on testnet/mainnet
   - CPU/memory/disk usage profiles
   - Database metrics

### 13.2 Phase 1 Kickoff (Week 1)

1. Create detailed implementation plan for Phase 1
2. Set up code review process for concurrent code
3. Begin implementation:
   - Modify `insertListBlocks`
   - Update `commitOrIndexes`
   - Add configuration support
   - Add metrics

### 13.3 Communication Plan

- **Weekly status updates** to stakeholders
- **Monthly performance reports** with metrics
- **Post-phase retrospectives** to learn and adjust
- **Public documentation updates** as features stabilize

---

## 14. Conclusion

This document outlines a comprehensive strategy to significantly improve cardano-db-sync's initial synchronization performance through carefully designed parallelization and optimization.

**Key Takeaways:**

1. **The opportunity is real**: Current architecture is heavily sequential despite available parallelism
2. **The approach is safe**: SyncLagging mode allows optimizations not safe when following the tip
3. **The plan is incremental**: Each phase delivers value independently
4. **The risks are manageable**: Feature flags, extensive testing, and fallback paths
5. **The benefits are substantial**: 3-4x faster initial sync is achievable

**Expected Timeline:**
- Phase 1: 2-3 weeks (implementation + testing)
- Phase 2: 3-4 weeks (implementation + testing)
- Phase 3: 4-5 weeks (implementation + testing)
- **Total: 10-12 weeks** for all three major phases

**Expected Results:**
- **Mainnet initial sync**: 66-100 hours → **16-33 hours** (3-4x improvement)
- **Testnet initial sync**: Proportionally faster
- **Re-sync from snapshot**: Even more dramatic improvement

This represents a significant enhancement to cardano-db-sync that will benefit all users, particularly those performing initial syncs or recovering from issues.

---

## Appendix A: File Modification Checklist

### Phase 1 Files
- [ ] `cardano-db-sync/src/Cardano/DbSync/Default.hs`
  - [ ] `insertListBlocks` function
  - [ ] `commitOrIndexes` function
  - [ ] Add `SyncState` checks
- [ ] `cardano-db-sync/src/Cardano/DbSync/Config/Types.hs`
  - [ ] Add `sioPerBlockTransactions :: Bool` to `SyncInsertOptions`

### Phase 2 Files
- [ ] `cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Grouped.hs`
  - [ ] `insertBlockGroupedData` function
  - [ ] Add parallel execution logic
- [ ] `cardano-db-sync/src/Cardano/DbSync/DbEvent.hs`
  - [ ] Verify `runDbPoolTransLogged` works correctly
  - [ ] Add error handling for pool operations
- [ ] `cardano-db-sync/src/Cardano/DbSync/Config/Types.hs`
  - [ ] Add `sioParallelBulkInserts :: Bool`
  - [ ] Add `sioParallelBulkInsertMaxWorkers :: Int`

### Phase 3 Files
- [ ] `cardano-db-sync/src/Cardano/DbSync/Default.hs`
  - [ ] Add `insertListBlocksPipelined` function
  - [ ] Add `ledgerApplyStage` function
  - [ ] Add `dbInsertStage` function
  - [ ] Modify `insertListBlocks` to dispatch based on config
- [ ] `cardano-db-sync/src/Cardano/DbSync/Config/Types.hs`
  - [ ] Add `sioUsePipeline :: Bool`
  - [ ] Add `sioPipelineQueueDepth :: Int`
  - [ ] Add `sioPipelineMinBlocks :: Int`

### Common Files (All Phases)
- [ ] `cardano-db-sync/src/Cardano/DbSync/Metrics.hs`
  - [ ] Add `AsyncOptMetrics` type
  - [ ] Add metric registration
  - [ ] Add metric update calls
- [ ] `cardano-db-sync/cardano-db-sync.cabal`
  - [ ] Add any new dependencies
- [ ] Config files
  - [ ] `config/mainnet-config.yaml`
  - [ ] `config/testnet-config.yaml`

---

## Appendix B: Testing Checklist

### Phase 1 Testing
- [ ] Unit tests for per-block transaction logic
- [ ] Integration test: Full testnet sync from genesis
- [ ] Integration test: Crash recovery at various points
- [ ] Integration test: Mode transition (Lagging → Following)
- [ ] Performance test: Compare with baseline
- [ ] Consistency test: Database comparison with sequential
- [ ] Stress test: High-transaction blocks

### Phase 2 Testing
- [ ] Unit tests for parallel bulk insert logic
- [ ] Unit tests for pool connection management
- [ ] Integration test: Full testnet sync with parallel bulk inserts
- [ ] Integration test: Pool worker failure scenarios
- [ ] Integration test: Foreign key constraint validation
- [ ] Performance test: Pool utilization metrics
- [ ] Performance test: Compare with Phase 1 only
- [ ] Consistency test: Cache coherence validation

### Phase 3 Testing
- [ ] Unit tests for pipeline queue management
- [ ] Unit tests for stage coordination
- [ ] Integration test: Full testnet sync with pipeline
- [ ] Integration test: Pipeline shutdown (graceful and abrupt)
- [ ] Integration test: Pipeline with small batches
- [ ] Integration test: Pipeline with large batches
- [ ] Performance test: CPU/IO overlap measurement
- [ ] Performance test: Compare with Phase 1+2
- [ ] Memory test: Queue growth monitoring
- [ ] Memory test: GC pressure comparison

### Cross-Phase Testing
- [ ] Configuration validation (all combinations)
- [ ] Feature flag testing (each phase independently)
- [ ] Rollback testing (disable phases during sync)
- [ ] Mainnet sync test (once stable on testnet)
- [ ] Long-running stability test (days/weeks)
- [ ] Resource leak detection (memory, connections, file handles)

---

## Appendix C: Metrics Dashboard Queries

### PostgreSQL Queries for Monitoring

```sql
-- Current sync progress
SELECT 
  MAX(block_no) as latest_block,
  MAX(slot_no) as latest_slot,
  NOW() - MAX(time) as blocks_behind
FROM block;

-- Average block processing time (last 1000 blocks)
SELECT 
  AVG(EXTRACT(EPOCH FROM (time - LAG(time) OVER (ORDER BY id)))) as avg_block_time_sec
FROM block 
ORDER BY id DESC 
LIMIT 1000;

-- Transaction volume (last 1000 blocks)
SELECT 
  COUNT(*) as tx_count,
  AVG(size) as avg_tx_size,
  SUM(size) as total_size
FROM tx 
WHERE block_id > (SELECT MAX(id) - 1000 FROM block);

-- Connection pool usage
SELECT 
  count(*) as active_connections,
  state,
  wait_event_type
FROM pg_stat_activity 
WHERE datname = 'cexplorer' 
GROUP BY state, wait_event_type;

-- WAL generation rate
SELECT 
  pg_current_wal_lsn() - '0/0'::pg_lsn as wal_bytes,
  pg_size_pretty(pg_current_wal_lsn() - '0/0'::pg_lsn) as wal_size;
```

---

*Document Version: 1.0*  
*Last Updated: 2026-03-10*  
*Author: Deep Architecture Analysis*  
*Status: DRAFT - For Review and Discussion*
