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
TESTNET_DIR="${TESTNET_DIR:-$HOMEIOG/testnet}"

# Profiling configuration
SNAPSHOT_DIR="$PROFILING_DIR/snapshots"
LOG_DIR="$PROFILING_DIR/logs"
SOCKET_PATH="${GHC_DEBUG_SOCKET:-/tmp/cardano-db-sync.ghc-debug}"
MEMORY_LOG="$LOG_DIR/memory-$(date +%Y%m%d-%H%M%S).csv"

# Create profiling directories
mkdir -p "$SNAPSHOT_DIR"
mkdir -p "$LOG_DIR"

# Find the db-sync binary
dbsync="$(find "$CARDANO_DB_SYNC_DIR/" -name cardano-db-sync -type f | head -1)"

if [ -z "$dbsync" ]; then
    echo "ERROR: Could not find cardano-db-sync binary"
    echo "Please run: cabal build cardano-db-sync"
    exit 1
fi

echo "======================================"
echo "cardano-db-sync Profiling Setup"
echo "======================================"
echo "DB Sync binary: $dbsync"
echo "Socket path: $SOCKET_PATH"
echo "Memory log: $MEMORY_LOG"
echo "Snapshot dir: $SNAPSHOT_DIR"
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

# Clean up old socket if exists
if [ -S "$SOCKET_PATH" ]; then
    echo "Removing old socket: $SOCKET_PATH"
    rm -f "$SOCKET_PATH"
fi

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
tmux send-keys -t $session:0.1 "export PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet" 'C-m'
tmux send-keys -t $session:0.1 "export GHC_DEBUG_SOCKET=$SOCKET_PATH" 'C-m'
tmux send-keys -t $session:0.1 "echo ''" 'C-m'
tmux send-keys -t $session:0.1 "echo '===================================='" 'C-m'
tmux send-keys -t $session:0.1 "echo 'Starting cardano-db-sync with profiling'" 'C-m'
tmux send-keys -t $session:0.1 "echo 'Socket: $SOCKET_PATH'" 'C-m'
tmux send-keys -t $session:0.1 "echo '===================================='" 'C-m'
tmux send-keys -t $session:0.1 "echo ''" 'C-m'
sleep 2
tmux send-keys -t $session:0.1 "PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet $dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/" 'C-m'

# Pane 2: Monitoring and instructions
echo "Setting up monitoring pane (pane 2)..."
tmux send-keys -t $session:0.2 "cd $CARDANO_DB_SYNC_DIR/" 'C-m'
tmux send-keys -t $session:0.2 "clear" 'C-m'
tmux send-keys -t $session:0.2 "echo '===================================='" 'C-m'
tmux send-keys -t $session:0.2 "echo 'Profiling Monitor'" 'C-m'
tmux send-keys -t $session:0.2 "echo '===================================='" 'C-m'
tmux send-keys -t $session:0.2 "echo ''" 'C-m'
tmux send-keys -t $session:0.2 "echo 'To profile with ghc-debug-brick:'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  ghc-debug-brick $SOCKET_PATH'" 'C-m'
tmux send-keys -t $session:0.2 "echo ''" 'C-m'
tmux send-keys -t $session:0.2 "echo 'Key commands in ghc-debug-brick:'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  p        - Pause process'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  Ctrl+e   - Search for closures'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  Enter    - Navigate into closure'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  Esc      - Go back'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  s        - Take snapshot'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  r        - Resume process'" 'C-m'
tmux send-keys -t $session:0.2 "echo '  q        - Quit'" 'C-m'
tmux send-keys -t $session:0.2 "echo ''" 'C-m'
tmux send-keys -t $session:0.2 "echo 'Memory monitoring log: $MEMORY_LOG'" 'C-m'
tmux send-keys -t $session:0.2 "echo 'Snapshots directory: $SNAPSHOT_DIR'" 'C-m'
tmux send-keys -t $session:0.2 "echo '===================================='" 'C-m'
tmux send-keys -t $session:0.2 "echo ''" 'C-m'

# Start memory monitoring
cat > /tmp/monitor-dbsync-memory.sh <<'EOF'
#!/bin/bash
LOG_FILE="$1"
INTERVAL="${2:-300}"  # Default 5 minutes

echo "timestamp,rss_kb,vsz_kb,cpu_percent" > "$LOG_FILE"

while true; do
    TIMESTAMP=$(date +%Y-%m-%d_%H:%M:%S)

    # Get the PID of cardano-db-sync
    PID=$(pgrep -f "cardano-db-sync" | head -1)

    if [ -z "$PID" ]; then
        echo "[$TIMESTAMP] WARNING: cardano-db-sync not running"
        sleep 10
        continue
    fi

    # Get memory stats
    RSS=$(ps -o rss= -p "$PID" 2>/dev/null | tr -d ' ')
    VSZ=$(ps -o vsz= -p "$PID" 2>/dev/null | tr -d ' ')
    CPU=$(ps -o %cpu= -p "$PID" 2>/dev/null | tr -d ' ')

    if [ -n "$RSS" ]; then
        echo "$TIMESTAMP,$RSS,$VSZ,$CPU" >> "$LOG_FILE"
        echo "[$TIMESTAMP] RSS: ${RSS}KB, VSZ: ${VSZ}KB, CPU: ${CPU}%"
    fi

    sleep "$INTERVAL"
done
EOF

chmod +x /tmp/monitor-dbsync-memory.sh

tmux send-keys -t $session:0.2 "echo 'Starting memory monitor...'" 'C-m'
tmux send-keys -t $session:0.2 "/tmp/monitor-dbsync-memory.sh $MEMORY_LOG 300 2>&1 | tee -a $LOG_DIR/monitor.log" 'C-m'

# Set pane sizes
tmux resize-pane -t $session:0.0 -x 50%
tmux resize-pane -t $session:0.1 -x 50%

echo ""
echo "======================================"
echo "Setup Complete!"
echo "======================================"
echo ""
echo "Tmux session '$session' is now running with:"
echo "  - Pane 0: cardano-node"
echo "  - Pane 1: cardano-db-sync (with ghc-debug)"
echo "  - Pane 2: Memory monitoring"
echo ""
echo "To attach to the session:"
echo "  tmux attach -t $session"
echo ""
echo "To run ghc-debug-brick (from another terminal):"
echo "  ghc-debug-brick $SOCKET_PATH"
echo ""
echo "Wait a few seconds for the socket to be created..."
sleep 3

# Check if socket was created
if [ -S "$SOCKET_PATH" ]; then
    echo "✓ ghc-debug socket created: $SOCKET_PATH"
    echo ""
    echo "You can now run:"
    echo "  ghc-debug-brick $SOCKET_PATH"
else
    echo "⚠ Socket not yet created. Check db-sync logs for errors."
    echo "  Expected socket: $SOCKET_PATH"
fi

echo ""
echo "To view memory log:"
echo "  tail -f $MEMORY_LOG"
echo ""

# Attach to session
echo "Attaching to tmux session..."
sleep 1
tmux attach-session -t $session
