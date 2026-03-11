#!/bin/bash

HOMEIOG=$HOME/Code/IOG
current_dir=$(basename "$PWD")
CARDANO_NODE_DIR="$HOMEIOG/cardano-node"
CARDANO_DB_SYNC_DIR="$HOMEIOG/$current_dir"
TESTNET_DIR="$HOMEIOG/testnet"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

dbsync="$(find "$CARDANO_DB_SYNC_DIR"/ -name cardano-db-sync -type f | head -1)"

# Generate the layout file with actual paths
zellij --layout <(cat <<EOF
layout {
    pane split_direction="vertical" {
        pane name="cardano-node" focus=true {
            command "bash"
            args "-c" "cd $CARDANO_NODE_DIR/ && cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json"
        }
        pane name="cardano-db-sync" {
            command "bash"
            args "-c" "cd $CARDANO_DB_SYNC_DIR/ && sleep 3 && export PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet-macos && PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet-macos $dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/"
        }
    }
}
EOF
)
