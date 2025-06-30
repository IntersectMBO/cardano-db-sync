#!/bin/bash

HOMEIOG=$HOME/Code/IOG
current_dir=$(basename "$PWD")
CARDANO_NODE_DIR="$HOMEIOG/cardano-node"
CARDANO_DB_SYNC_DIR="$HOMEIOG/$current_dir"
TESTNET_DIR="$HOMEIOG/testnet"

dbsync="$(find "$CARDANO_DB_SYNC_DIR"/ -name cardano-db-sync -type f)"
session="IOHK"

# Kill session and processes if session exists
if tmux has-session -t $session 2>/dev/null; then
    tmux kill-session -t $session
    killall cardano-node 2>/dev/null || true
    pkill -f cardano-db-sync 2>/dev/null || true
fi

tmux new-session -d -s $session

tmux rename-window $session
tmux split-window -h

# Cardano Node
tmux send-keys -t 0 "cd $CARDANO_NODE_DIR/" 'C-m'
tmux send-keys -t 0 "cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json" 'C-m'

# Cardano DB-Sync
tmux send-keys -t 1 "cd $CARDANO_DB_SYNC_DIR/" 'C-m'; sleep 3
tmux send-keys -t 1 "export PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet" 'C-m'; sleep 2
tmux send-keys -t 1 "PGPASSFILE=$CARDANO_DB_SYNC_DIR/config/pgpass-mainnet $dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/" 'C-m'
# tmux send-keys -t 1 "$dbsync --config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --schema-dir $CARDANO_DB_SYNC_DIR/schema/ +RTS -p -hc -L200 -RTS" 'C-m'

tmux send-keys -t 0 "cd $HOMEIOG/" 'C-m'

tmux attach-session -t $session
# tmux send-keys -t 1 "$dbsync --config $HOMEIOG/testnet/db-sync-config.json --socket-path $HOMEIOG/testnet/db/node.socket --state-dir $HOMEIOG/testnet/ledger-state --schema-dir $HOMEIOG/cardano-db-sync/schema/ +RTS -p -hc -L200 -RTS" 'C-m'
