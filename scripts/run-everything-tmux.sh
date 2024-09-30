#!/usr/bin/env bash

HOMEIOG=$HOME/Code/IOG

dbsync="$(find "$HOMEIOG"/cardano-db-sync/ -name cardano-db-sync -type f)"

session="IOHK"

# Check if the session exists, discarding output
# We can check $? for the exit status (zero for success, non-zero for failure)
tmux has-session -t $session 2>/dev/null

# if there is a session named IOHK then kill it
if [ $? = 1 ]; then
    tmux kill-session -t $session
    killall cardano-node
fi

tmux new-session -d -s $session

tmux rename-window $session
tmux split-window -h
# tmux split-window -v
# tmux split-window -v
# tmux select-layout tiled

# Cardano Node
tmux send-keys -t 0 "cd $HOMEIOG/cardano-node/" 'C-m'
tmux send-keys -t 0 "cardano-node run --config $HOMEIOG/testnet/config.json --database-path $HOMEIOG/testnet/db/ --socket-path $HOMEIOG/testnet/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $HOMEIOG/testnet/topology.json" 'C-m'

# Cardano DB-Sync
tmux send-keys -t 1 "cd $HOMEIOG/cardano-db-sync/" 'C-m'; sleep 3
tmux send-keys -t 1 "export PGPASSFILE=$HOMEIOG/cardano-db-sync/config/pgpass-mainnet" 'C-m'; sleep 2
# tmux send-keys -t 1 "$dbsync --config $HOMEIOG/testnet/db-sync-config.json --socket-path $HOMEIOG/testnet/db/node.socket --state-dir $HOMEIOG/testnet/ledger-state --schema-dir $HOMEIOG/cardano-db-sync/schema/ +RTS -p -hc -L200 -RTS" 'C-m'
tmux send-keys -t 1 "PGPASSFILE=$HOMEIOG/cardano-db-sync/config/pgpass-mainnet $dbsync --config $HOMEIOG/testnet/db-sync-config.json --socket-path $HOMEIOG/testnet/db/node.socket --state-dir $HOMEIOG/testnet/ledger-state --schema-dir $HOMEIOG/cardano-db-sync/schema/" 'C-m'

tmux send-keys -t 0 "cd $HOMEIOG/" 'C-m'

tmux attach-session -t $session
