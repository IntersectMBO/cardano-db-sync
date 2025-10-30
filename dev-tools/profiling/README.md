# Cardano DB Sync - Profiling Tool

A local development profiling suite for cardano-db-sync using ghc-debug for interactive heap analysis and memory leak detection.

## Overview

This tool enables deep memory profiling of cardano-db-sync during development:

- **Interactive heap exploration** with ghc-debug-brick TUI
- **Memory leak detection** via retainer chain analysis
- **Automated memory monitoring** with trend detection
- **Snapshot management** for comparing different runs

Based on the [ouroboros-consensus team's approach](https://github.com/IntersectMBO/ouroboros-consensus/pull/1731#issuecomment-3442427702).

## Quick Start

### Prerequisites

1. **ghc-debug-brick** (interactive TUI client):
   ```bash
   git clone https://gitlab.haskell.org/ghc/ghc-debug.git
   cd ghc-debug/brick
   cabal install ghc-debug-brick
   ```

2. **Instrumented cardano-db-sync binary**:

   Add to `cardano-db-sync/cardano-db-sync.cabal`:
   ```cabal
   executable cardano-db-sync
     build-depends:
       , ghc-debug-stub
   ```

   Add to `cabal.project`:
   ```cabal
   package cardano-db-sync
     ghc-options: -finfo-table-map -fdistinct-constructor-tables
   ```

   Update `cardano-db-sync/app/cardano-db-sync.hs`:
   ```haskell
   import GHC.Debug.Stub (withGhcDebug)

   main :: IO ()
   main = withGhcDebug actualMain

   actualMain :: IO ()
   actualMain = do
     cmd <- Opt.execParser opts
     -- ... rest of existing code
   ```

   Rebuild:
   ```bash
   cabal build cardano-db-sync
   ```

### Start Profiling

```bash
# From the repository root
cd dev-tools/profiling

# Start cardano-db-sync with profiling enabled
./scripts/start-profiling.sh
```

This launches a tmux session with:
- **Pane 0**: cardano-db-sync with ghc-debug enabled
- **Pane 1**: Memory monitoring (logs every 5 minutes)

### Run and Collect Data

Let cardano-db-sync run for several hours (recommended: 4-8 hours) to:
- Build up memory usage patterns
- Allow potential leaks to accumulate
- Capture different phases (sync, steady state)

### Connect and Analyze

While cardano-db-sync is running, connect with the interactive debugger:

```bash
# In a new terminal
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug
```

**Interactive TUI Commands**:
- `Enter` - Expand selected closure
- `s` - Search for type (e.g., "TxOut", "Connection", "LRUCache")
- `r` - View retainer chain (what's keeping this alive)
- `/` - Filter results
- `q` - Quit/back

### Analyze Memory Trends

```bash
# Analyze the most recent memory log
./scripts/analyze-memory.sh

# Or specify a log file
./scripts/analyze-memory.sh logs/memory-20250129-143022.csv
```

## What to Look For

### In ghc-debug-brick TUI

1. **Search for key types** (press `s`):
   - `TxOut` - Transaction outputs (should be in database, not memory)
   - `TxIn` - Transaction inputs
   - `Connection` - Hasql database connections
   - `LRUCache` - LRU caches
   - `FIFOCache` - FIFO caches
   - `BlockGroupedData` - In-memory grouped data
   - `ByteString` - Raw bytes (check for large accumulations)
   - `Text` - Text data

2. **Check retainer chains** (press `r` on a closure):
   - Look for unexpected retention paths
   - Find lazy fields holding large data structures
   - Identify closures keeping old data alive

3. **Look for common leak patterns**:
   - Lazy configuration records holding large data
   - Accumulating lists that should be processed
   - Cached data that should have been evicted
   - Thunks (unevaluated expressions) in hot paths

### In Memory Analysis Report

- **Memory growth > 50%**: Warning - potential leak
- **Memory growth 20-50%**: Monitor closely
- **Flat or declining memory**: Healthy

## Common Workflows

### 1. Finding a Memory Leak

**Goal**: Identify what's accumulating in memory

```bash
# 1. Start profiling
./scripts/start-profiling.sh

# 2. Wait 4-8 hours for memory to grow

# 3. Connect with debugger
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug

# 4. In TUI:
#    - Press 's' to search
#    - Try "TxOut" - are there many in memory?
#    - Press 'r' on one to see retainer chain
#    - Note what's holding it (cache? lazy field? list?)

# 5. Check memory trend
./scripts/analyze-memory.sh

# 6. Document finding in reports/
echo "Finding: TxOut retained by lazy field in Config" > reports/leak-001.txt
```

### 2. Comparing Before/After Changes

**Goal**: Verify a fix reduces memory usage

```bash
# 1. Take baseline snapshot
./scripts/start-profiling.sh
# ... let run 4 hours ...
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug
# In TUI: take snapshot (Ctrl+S), save to snapshots/baseline.bin
./scripts/analyze-memory.sh > reports/baseline-memory.txt

# 2. Apply fix (e.g., add strictness annotation)
vim cardano-db-sync/src/Cardano/DbSync/Config.hs
# Add: data Config = Config { !configField :: Field }
cabal build cardano-db-sync

# 3. Take new snapshot
./scripts/start-profiling.sh
# ... let run 4 hours ...
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug
# Take snapshot, save to snapshots/fixed.bin
./scripts/analyze-memory.sh > reports/fixed-memory.txt

# 4. Compare
diff reports/baseline-memory.txt reports/fixed-memory.txt
```

### 3. Investigating Cache Behavior

**Goal**: Understand if caches are sized appropriately

```bash
# 1. Search for cache types
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug
# Press 's', search "LRUCache"

# 2. Check cache sizes
# Count how many cache entries exist
# Compare to configured cache size

# 3. Look for cache thrashing
# If many entries are being evicted but heap keeps growing,
# cached objects might not be getting GC'd

# 4. Correlate with Prometheus metrics (if available)
# Check cache_hits_total and cache_misses_total
# in the monitoring tool
```

### 4. Profiling Different Workloads

**Goal**: Compare memory behavior in different scenarios

```bash
# Scenario 1: Initial sync
TESTNET_DIR=/path/to/fresh/testnet ./scripts/start-profiling.sh
# ... run until sync completes ...
./scripts/analyze-memory.sh > reports/initial-sync.txt

# Scenario 2: Steady state
# Use existing synced testnet
./scripts/start-profiling.sh
# ... run for 8 hours at tip ...
./scripts/analyze-memory.sh > reports/steady-state.txt

# Scenario 3: With tx_cbor enabled
# Edit config to enable tx_cbor
./scripts/start-profiling.sh
# ... run for 4 hours ...
./scripts/analyze-memory.sh > reports/with-cbor.txt

# Compare
diff reports/initial-sync.txt reports/steady-state.txt
```

## Integration with Monitoring

Combine profiling with monitoring for complete visibility:

```bash
# Terminal 1: Start monitoring
cd dev-tools/monitoring
./scripts/start-monitoring.sh

# Terminal 2: Start profiling
cd dev-tools/profiling
./scripts/start-profiling.sh

# Terminal 3: Monitor Prometheus/Grafana
# Open http://localhost:9090 and http://localhost:3000

# When you see a metric spike in Grafana:
# 1. Note the timestamp
# 2. Connect ghc-debug-brick
# 3. Take a snapshot
# 4. Save snapshot with timestamp: snapshots/spike-20250129-1430.bin
# 5. Investigate in TUI
```

## Configuration

### Environment Variables

```bash
# Socket path for ghc-debug
export GHC_DEBUG_SOCKET=/tmp/cardano-db-sync.ghc-debug

# Testnet directory
export TESTNET_DIR=$HOME/Code/IOG/testnet

# Memory monitoring interval (seconds)
export MEMORY_LOG_INTERVAL=300  # Default: 5 minutes

./scripts/start-profiling.sh
```

### Memory Log Format

CSV format: `timestamp,rss_kb,vsz_kb,cpu_percent`

```csv
2025-01-29 14:30:00,2048576,4096000,45.2
2025-01-29 14:35:00,2150432,4100000,47.8
```

### Snapshot Naming Convention

Use descriptive names for snapshots:

```bash
snapshots/baseline-4h.bin              # Baseline after 4 hours
snapshots/after-strictness-fix-4h.bin  # After applying fix
snapshots/high-memory-20250129.bin     # During high memory event
snapshots/epoch-boundary-spike.bin     # During epoch boundary
```

## Troubleshooting

### ghc-debug-brick won't connect

**Problem**: `cannot connect to /tmp/cardano-db-sync.ghc-debug`

**Solution**:
1. Check socket exists: `ls -l /tmp/cardano-db-sync.ghc-debug`
2. Verify cardano-db-sync is running: `ps aux | grep cardano-db-sync`
3. Check if instrumented: `ldd $(which cardano-db-sync) | grep ghc-debug-stub`
4. Check socket permissions: `chmod 777 /tmp/cardano-db-sync.ghc-debug`

### Build fails with ghc-debug-stub

**Problem**: `Could not find module 'GHC.Debug.Stub'`

**Solution**:
1. Check `cabal.project` has ghc-debug source repo
2. Run: `cabal update && cabal build --only-dependencies cardano-db-sync`
3. Verify: `cabal list-bin cardano-db-sync`

### Memory log is empty

**Problem**: No entries in memory log CSV

**Solution**:
1. Check cardano-db-sync is running: `tmux attach -t cardano-profiling`
2. Check memory monitor pane (pane 1)
3. Check log file permissions: `ls -l logs/`
4. Try running monitor script manually:
   ```bash
   while true; do
     ps aux | grep cardano-db-sync | grep -v grep | awk '{print $6,$5,$3}'
     sleep 300
   done
   ```

### TUI shows "No source info"

**Problem**: Can't see source locations in ghc-debug-brick

**Solution**:
This is expected if `-finfo-table-map` wasn't enabled during build.

1. Verify ghc-options in cabal.project:
   ```cabal
   package cardano-db-sync
     ghc-options: -finfo-table-map -fdistinct-constructor-tables
   ```
2. Clean and rebuild:
   ```bash
   cabal clean
   cabal build cardano-db-sync
   ```

### Snapshots are huge (>10GB)

**Problem**: Snapshot files are very large

**Solution**:
This is normal for large heap sizes. To manage:

1. Compress snapshots:
   ```bash
   gzip snapshots/*.bin
   ```

2. Keep only important snapshots, delete others

3. Use `.gitignore` (already configured) to prevent committing

## Files and Directories

```
profiling/
├── README.md                  # This file
├── scripts/
│   ├── start-profiling.sh     # Launch profiling session
│   └── analyze-memory.sh      # Analyze memory logs
├── snapshots/                 # Heap snapshots (gitignored)
│   └── .gitignore
├── logs/                      # Memory monitoring logs (gitignored)
│   └── .gitignore
└── reports/                   # Analysis reports (commit these)
    └── .gitkeep
```

## Code Changes Required

### Minimal Setup (3 changes)

**1. Add dependency** in `cardano-db-sync/cardano-db-sync.cabal`:
```cabal
executable cardano-db-sync
  build-depends:
    , ghc-debug-stub
```

**2. Add GHC flags** in `cabal.project`:
```cabal
package cardano-db-sync
  ghc-options: -finfo-table-map -fdistinct-constructor-tables
```

**3. Instrument main** in `cardano-db-sync/app/cardano-db-sync.hs`:
```haskell
import GHC.Debug.Stub (withGhcDebug)

main :: IO ()
main = withGhcDebug actualMain

actualMain :: IO ()  -- rename existing main
actualMain = do
  -- existing code
```

## Known Findings (Example)

Document findings in `reports/` directory:

**reports/finding-lazy-config.md**:
```markdown
# Finding: Lazy Config Field Retaining Genesis UTxO

## Date
2025-01-29

## Symptom
Memory grows from 2GB to 8GB over 6 hours

## Investigation
1. Connected ghc-debug-brick after 6 hours
2. Searched for "TxOut" - found 500K instances in memory
3. Checked retainer chain - all retained by `config` field in `Env`
4. Identified lazy field: `configGenesis :: Genesis`

## Root Cause
The `Genesis` type has a lazy field holding all genesis UTxOs.
Config is created once and held in `Env`, keeping entire genesis in memory.

## Fix
Add strictness annotation:
```haskell
data Config = Config
  { configGenesis :: !Genesis  -- added bang pattern
  }
```

## Result
Memory now stable at 2-3GB after fix
```

## Next Steps

1. **Run initial profiling session** (4-8 hours)
2. **Investigate top memory consumers** with ghc-debug-brick
3. **Document findings** in reports/
4. **Apply fixes** (strictness, cache tuning, etc.)
5. **Verify improvements** with before/after comparison
6. **Set up Prometheus integration** to export profiling metrics

## See Also

- [Monitoring Tool](../monitoring/README.md) - Prometheus/Grafana monitoring
- [Quick Start Guide](../../PROFILING_QUICK_START.md) - Original quick start
- [ghc-debug Documentation](https://gitlab.haskell.org/ghc/ghc-debug)
- [ouroboros-consensus PR #1731](https://github.com/IntersectMBO/ouroboros-consensus/pull/1731) - Inspiration for this approach
