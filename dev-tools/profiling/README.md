# Cardano DB Sync - ghc-debug Profiling

Interactive heap profiling for cardano-db-sync using ghc-debug.

## Quick Setup

### 1. Install ghc-debug-brick

```bash
cabal install ghc-debug-brick
```

This automatically installs all ghc-debug dependencies.

### 2. Configure ghc-debug (first time only)

The ghc-debug dependencies are configured in `cabal.project.local` (not committed to git).

If you don't have them yet, add to your `cabal.project.local`:

```cabal
source-repository-package
  type: git
  location: https://gitlab.haskell.org/ghc/ghc-debug.git
  tag: 1b0f36fab86e9baa9734c88dcc1dbe17d10d8c93
  subdir: stub

source-repository-package
  type: git
  location: https://gitlab.haskell.org/ghc/ghc-debug.git
  tag: 1b0f36fab86e9baa9734c88dcc1dbe17d10d8c93
  subdir: common
```

**Note**: The ghc-debug commit above is from master branch (0.7.0.0). If you have GHC compatibility issues, you may need to adjust this commit.

### 3. Build cardano-db-sync with profiling

```bash
# From repository root
cabal build --flag enable-ghc-debug exe:cardano-db-sync

# Or use the build script
./dev-tools/profiling/scripts/build-profiling.sh
```

### 4. Run cardano-db-sync

Start cardano-db-sync as you normally would. The `enable-ghc-debug` flag automatically enables ghc-debug support:

```bash
# Using the start script (launches in tmux)
./dev-tools/profiling/scripts/start-profiling.sh

# Or run manually with your usual command
PGPASSFILE=config/pgpass-mainnet cabal run cardano-db-sync -- \
    --config config/mainnet-config.yaml \
    --socket-path /path/to/node.socket \
    --state-dir ledger-state/mainnet \
    --schema-dir schema/
```

**Note**: When running with `enable-ghc-debug`, cardano-db-sync will print:
```
Starting ghc-debug on socket: ~/.local/share/ghc-debug/debuggee/sockets/<PID>-cardano-db-sync
```

### 5. Connect with ghc-debug-brick

In a new terminal, while cardano-db-sync is running:

```bash
ghc-debug-brick
```

## Files

- `scripts/build-profiling.sh` - Build with profiling enabled
- `scripts/start-profiling.sh` - Launch cardano-db-sync in tmux with monitoring

## Troubleshooting

**ghc-debug-brick shows "0 found"**:
1. Run `ghc-debug-brick` without arguments
2. Press `Tab` to switch to process view
3. Check socket exists: `ls ~/.local/share/ghc-debug/debuggee/sockets/`
4. Verify cardano-db-sync is running: `ps aux | grep cardano-db-sync`

**MacOS Do NOT set `GHC_DEBUG_SOCKET` environment variable** - it can break auto-discovery.

## See Also

- [ghc-debug Documentation](https://gitlab.haskell.org/ghc/ghc-debug)
- [ouroboros-consensus PR #1731](https://github.com/IntersectMBO/ouroboros-consensus/pull/1731) - Similar approach
