# cardano-tx-submit-webapi

The `cardano-tx-submit-webapi` is a program that provides a webapi that allows transactions
(generated using the `cardano-cli` program in the `cardano-node` repository) to be HTTP POSTed
to the Cardano blockchain.

In order to submit a transaction to the network (mainnet, staging or any of the testnets), the
webapi needs a running `cardano-node` and the Genesis file and Genesis hash value for the network.

The config for `mainnet` is provided at `config/tx-submit-mainnet-config.yaml` of this repository.

Configuration for other networks can be generated using the above as a template using:
```
scripts/gen-tx-submit-config.sh --require-magic --genesis-hash <hash> --output config.yaml
```
where `--require-magic` is required for all test nets, but should be replaced with
`--require-no-magic` for mainnet or staging.

Once the correct configuration has been generated, the webapi can be run using:
```
cabal run cardano-tx-submit-webapi -- \
    --config config.yaml \
    --genesis-file <genesis file> \
    --socket-path <socket path> \
    --port 8101
```

With the webapi running, it is possible to submit a preformed transaction using:
```
curl -X POST \
    --header "Content-Type:application/octet-stream" \
    --data-binary @transaction.bin http://localhost:8101/api/submit/tx
```
where `transaction.bin` is the preformed transaction.
