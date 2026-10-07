#!/bin/bash

set -e  # Exit on error

# Determine project root directory (parent of scripts/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CARDANO_DB_SYNC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Set default paths (can be overridden by environment variables)
HOMEIOG="${HOMEIOG:-$HOME/Code/IOG}"
CARDANO_NODE_DIR="${CARDANO_NODE_DIR:-$HOMEIOG/cardano-node}"
TESTNET_DIR="${TESTNET_DIR:-$HOMEIOG/testnet}"

# Verify required directories exist
if [ ! -d "$CARDANO_NODE_DIR" ]; then
    echo "ERROR: cardano-node directory not found at: $CARDANO_NODE_DIR"
    echo "Set CARDANO_NODE_DIR environment variable or update HOMEIOG path"
    exit 1
fi

if [ ! -d "$TESTNET_DIR" ]; then
    echo "ERROR: testnet directory not found at: $TESTNET_DIR"
    echo "Set TESTNET_DIR environment variable or update HOMEIOG path"
    exit 1
fi

# Find cardano-db-sync binary
dbsync="$(find "$CARDANO_DB_SYNC_DIR"/ -name cardano-db-sync -type f | head -1)"

if [ -z "$dbsync" ]; then
    echo "ERROR: cardano-db-sync binary not found in: $CARDANO_DB_SYNC_DIR"
    echo "Build the project first with: cabal build cardano-db-sync"
    exit 1
fi

echo "Using cardano-db-sync binary: $dbsync"

# Kill any previous instances
echo "Cleaning up previous instances..."
pkill -f cardano-node || true
pkill -f cardano-db-sync || true
pkill -f prometheus || true
pkill -f telegraf || true
sleep 1

# Setup database permissions for Telegraf
echo "Setting up database permissions for Telegraf..."
psql -d cexplorer -f "$CARDANO_DB_SYNC_DIR/monitoring/scripts/grant-telegraf-permissions.sql" > /dev/null 2>&1 || echo "Warning: Could not grant permissions (database may not be ready)"
sleep 1

echo "Cleanup complete. Starting services..."

# Generate the layout file with actual paths
# Layout: 
#  ┌─────────────┬─────────────┐
#  │ cardano-node│ cardano-db- │
#  │             │    sync     │
#  ├─────────────┼─────────────┤
#  │  telegraf   │ prometheus  │
#  └─────────────┴─────────────┘
zellij --layout <(cat <<EOF
layout {
    pane split_direction="vertical" {
        pane split_direction="horizontal" {
            pane name="cardano-node" focus=true {
                command "bash"
                args "-c" "cd $CARDANO_NODE_DIR/ && cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json"
            }
            pane name="telegraf" {
                command "bash"
                args "-c" "cd $CARDANO_DB_SYNC_DIR/ && echo '=== Telegraf Metrics Collector ===' && echo 'Collecting system, PostgreSQL, and process metrics' && echo 'Metrics endpoint: http://localhost:9273/metrics' && echo '' && telegraf --config $CARDANO_DB_SYNC_DIR/monitoring/config/telegraf.conf"
            }
        }
        pane split_direction="horizontal" {
            pane name="cardano-db-sync" {
                command "bash"
                args "-c" "cd $CARDANO_DB_SYNC_DIR/ && sleep 3 && export PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet-macos && $dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/"
            }
            pane name="prometheus" {
                command "bash"
                args "-c" "cd $CARDANO_DB_SYNC_DIR/ && echo '=== Prometheus Metrics Server ===' && echo 'Web UI: http://localhost:9090' && echo 'Targets: http://localhost:9090/targets' && echo '' && mkdir -p $CARDANO_DB_SYNC_DIR/monitoring/data && prometheus --config.file=$CARDANO_DB_SYNC_DIR/monitoring/config/prometheus.yml --storage.tsdb.path=$CARDANO_DB_SYNC_DIR/monitoring/data --web.listen-address=:9090"
            }
        }
    }
}
EOF
)
