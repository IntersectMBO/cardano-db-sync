# Testing cardano-tx-submit-webapi

Setting this up for testing and for actual use on a real network is significantly more difficult
than it should be. This is definitely not ready for end users yet.

The biggest thing missing at the moment is documentation of generation of transactions with the
command line tools. That needs to be *thoroughly* documented and this document is not the place
for that.

The following instructions are for generating a testnet and testing tx-submission on that testnet.

* This process generates a lot of state in the `cardano-node` checkout that can be cleaned up using:
    ```
    cardano-node $ rm -rf configuration/GenesisFiles/ db-* logs/ transaction.bin
    ```

* Generated the Genesis file for the testnet:
    ```
    cardano-node $ ./scripts/genesis.sh
    ```
  Part of the output of the above command is somethng like:
    ```
    genesis created with hash = 6f9371...939776
      in directory .../cardano-node/configuration/genesis
    ```

* Run up a `tmux` session (the nodes of the testnet will end up in the tmux session):
    ```
    cardano-node $ tmux new-session -s Demo
    ```

* Run the testnet using the following. The `sleep` command is required to ensure sufficient time
  has elapsed between the creationg of the genesis file and starting of the testnet.
    ```
    cardano-node $ sleep 100 ; ./scripts/shelley-testnet.sh
    ```

* Get a delegate key so that a transaction can be generated:
    ```
    cardano-node $ ./scripts/get-default-key-address.sh \
        configuration/GenesisFiles/delegate-keys.001.key
    ````
  which will output a key like `2cWKMJemoBahs2gd4sQX1XgSLVLyAMrtNy2qgp4t2BYjBFygD3dLo4ejXC4p3C3pnc6qd`.

* Generate a transaction from the Genesis UTxO set with the key from the previous command:
    ```
    cardano-node $ ./scripts/issue-genesis-utxo-expenditure.sh transaction.bin \
        2cWKMJemoBahs2gd4sQX1XgSLVLyAMrtNy2qgp4t2BYjBFygD3dLo4ejXC4p3C3pnc6qd 10000
    ```

* Generate a config file for the `tx-submit-webapi` (note the change of directory):
    ```
    cardano-explorer $ scripts/generate-tx-submit-config.sh --require-magic \
         --genesis-hash 6f9371...939776 --output config.yaml
    ```
    where the Genesis hash is the one output in an previous step.

* Run the `cardano-tx-submit-webapi`:
    ```
    cardano-explorer $ cabal run cardano-tx-submit-webapi -- \
         --config config.yaml \
        --genesis-file ../cardano-node/configuration/GenesisFiles/genesis.json \
        --socket-path ../cardano-node/socket/node-core-0.socket \
        --port 8101
    ```

* Submit the transaction to the webapi:
    ```
    cardano-explorer $ curl -X POST \
        --header "Content-Type:application/cbor" \
        --data-binary @transaction.bin http://localhost:8101/api/submit/tx
    ```
  which returns a status as a chunk of JSON.
