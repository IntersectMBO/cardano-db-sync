#!/bin/bash
# start-profiling.sh
# Run cardano-db-sync with ghc-debug profiling enabled
# Based on ouroboros-consensus team's approach

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILING_DIR="$(dirname "$SCRIPT_DIR")"
DEV_TOOLS_DIR="$(dirname "$PROFILING_DIR")"
CARDANO_DB_SYNC_DIR="$(dirname "$DEV_TOOLS_DIR")"

HOMEIOG=${HOMEIOG:-$HOME/Code/IOG}
CARDANO_NODE_DIR="${CARDANO_NODE_DIR:-$HOMEIOG/cardano-node}"
TESTNET_DIR="${TESTNET_DIR:-$HOMEIOG/testnet}"

# Detect OS for pgpass file
if [[ "$OSTYPE" == "darwin"* ]]; then
    PGPASS_FILE="${PGPASS_FILE:-$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet-macos}"
else
    PGPASS_FILE="${PGPASS_FILE:-$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet}"
fi

# Profiling configuration
SNAPSHOT_DIR="$PROFILING_DIR/snapshots"
LOG_DIR="$PROFILING_DIR/logs"
MEMORY_LOG="$LOG_DIR/memory-$(date +%Y%m%d-%H%M%S).csv"
# Note: Don't set GHC_DEBUG_SOCKET - let ghc-debug-stub use default XDG location
# Socket will be at: ~/.local/share/ghc-debug/debuggee/sockets/<PID>-cardano-db-sync

# Create profiling directories
mkdir -p "$SNAPSHOT_DIR"
mkdir -p "$LOG_DIR"

# Build cardano-db-sync with profiling support
echo "Building profiling-enabled binary..."
"$SCRIPT_DIR/build-profiling.sh"
echo ""

# Find the profiling-enabled binary (similar to run-everything-tmux.sh)
dbsync="$(find "$CARDANO_DB_SYNC_DIR"/ -name cardano-db-sync -type f | head -1)"

if [ -z "$dbsync" ]; then
    echo "ERROR: Could not find cardano-db-sync binary"
    echo "Build may have failed. Check output above."
    exit 1
fi

echo "======================================"
echo "cardano-db-sync Profiling Setup"
echo "======================================"
echo "DB Sync binary: $dbsync"
echo "Memory log: $MEMORY_LOG"
echo "Snapshot dir: $SNAPSHOT_DIR"
echo "Socket location: ~/.local/share/ghc-debug/debuggee/sockets/<PID>-cardano-db-sync"
echo "======================================"
echo ""

# Check if ghc-debug-brick is installed
if ! command -v ghc-debug-brick &> /dev/null; then
    echo "WARNING: ghc-debug-brick not found in PATH"
    echo "To install:"
    echo "  git clone https://gitlab.haskell.org/ghc/ghc-debug.git"
    echo "  cd ghc-debug/brick"
    echo "  cabal install ghc-debug-brick"
    echo ""
fi

# Note: Sockets are auto-managed by ghc-debug in XDG directory

# Set up tmux session
session="DB-SYNC-PROFILE"

# Kill existing session if it exists
if tmux has-session -t $session 2>/dev/null; then
    echo "Killing existing tmux session: $session"
    tmux kill-session -t $session
    killall cardano-node 2>/dev/null || true
    pkill -f cardano-db-sync 2>/dev/null || true
    sleep 2
fi

echo "Creating tmux session: $session"
tmux new-session -d -s $session

# Rename the window
tmux rename-window -t $session "profiling"

# Split into 3 panes:
# 0: cardano-node
# 1: cardano-db-sync
# 2: monitoring
tmux split-window -h -t $session
tmux split-window -v -t $session:0.1

# Pane 0: Cardano Node
echo "Setting up cardano-node (pane 0)..."
tmux send-keys -t $session:0.0 "cd $CARDANO_NODE_DIR/" 'C-m'
tmux send-keys -t $session:0.0 "echo 'Starting cardano-node...'" 'C-m'
tmux send-keys -t $session:0.0 "cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json" 'C-m'

# Wait for node to start
sleep 5

# Pane 1: Cardano DB-Sync with profiling
echo "Setting up cardano-db-sync with profiling (pane 1)..."
tmux send-keys -t $session:0.1 "cd $CARDANO_DB_SYNC_DIR/" 'C-m'
tmux send-keys -t $session:0.1 "export PGPASSFILE=$PGPASS_FILE" 'C-m'
sleep 2
tmux send-keys -t $session:0.1 "PGPASSFILE=$PGPASS_FILE $dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/" 'C-m'

# Pane 2: Monitoring and instructions
echo "Setting up monitoring pane (pane 2)..."
tmux send-keys -t $session:0.2 "cd $CARDANO_DB_SYNC_DIR/" 'C-m'

# Start memory monitoring
tmux send-keys -t $session:0.2 "$SCRIPT_DIR/monitor-dbsync-memory.sh $MEMORY_LOG 300 2>&1 | tee -a $LOG_DIR/monitor.log" 'C-m'

# Set pane sizes
tmux resize-pane -t $session:0.0 -x 50%
tmux resize-pane -t $session:0.1 -x 50%

tmux -CC attach-session -t $session
