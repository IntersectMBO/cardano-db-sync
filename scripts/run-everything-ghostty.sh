#!/bin/bash

HOMEIOG=$HOME/Code/IOG
current_dir=$(basename "$PWD")
CARDANO_NODE_DIR="$HOMEIOG/cardano-node"
CARDANO_DB_SYNC_DIR="$HOMEIOG/$current_dir"
TESTNET_DIR="$HOMEIOG/testnet"

dbsync="$(find "$CARDANO_DB_SYNC_DIR"/ -name cardano-db-sync -type f)"

# Kill existing processes
killall cardano-node 2>/dev/null || true
pkill -f cardano-db-sync 2>/dev/null || true

# Build the cardano-node command
NODE_CMD="cardano-node run --config ${TESTNET_DIR}/config.json --database-path ${TESTNET_DIR}/db/ --socket-path ${TESTNET_DIR}/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology ${TESTNET_DIR}/topology.json"

# Build the cardano-db-sync command
DBSYNC_CMD="PGPASSFILE=${CARDANO_DB_SYNC_DIR}/config/pgpass-mainnet-macos ${dbsync} --config ${TESTNET_DIR}/db-sync-config.json --socket-path ${TESTNET_DIR}/db/node.socket --state-dir ${TESTNET_DIR}/ledger-state --schema-dir ${CARDANO_DB_SYNC_DIR}/schema/"

# Use AppleScript to create native Ghostty splits
osascript - "${CARDANO_NODE_DIR}" "${CARDANO_DB_SYNC_DIR}" "${NODE_CMD}" "${DBSYNC_CMD}" <<'APPLESCRIPT'
on run argv
    set nodeDir to item 1 of argv
    set dbsyncDir to item 2 of argv
    set nodeCmd to item 3 of argv
    set dbsyncCmd to item 4 of argv
    
    tell application "Ghostty"
        activate
        
        set cfg to new surface configuration
        set initial working directory of cfg to nodeDir
        
        set win to new window with configuration cfg
        set nodePane to terminal 1 of selected tab of win
        
        set dbsyncPane to split nodePane direction right with configuration cfg
        
        input text ("cd " & nodeDir) to nodePane
        send key "enter" to nodePane
        input text nodeCmd to nodePane
        send key "enter" to nodePane
        
        input text ("cd " & dbsyncDir) to dbsyncPane
        send key "enter" to dbsyncPane
        delay 3
        input text ("export PGPASSFILE=" & dbsyncDir & "/config/pgpass-mainnet-macos") to dbsyncPane
        send key "enter" to dbsyncPane
        delay 2
        input text dbsyncCmd to dbsyncPane
        send key "enter" to dbsyncPane
        
        focus nodePane
    end tell
end run
APPLESCRIPT
