# Submitting a Transaction to the tx-submit Web API

Building and running the `cardano-tx-submit-webap` is described in [this document][BuildDoc].

Transactions can be generated using either the `cardano-cli` (in the [cardano-node][NodeRepo]) or
with the [`js-wasm` library][JS-Wasm].

The transaction needs to be a raw binary (and not encoded as hex or anything else). Once generated
(in say the file `tx.bin`) the transaction can be submitted to the webapi using:

```
curl -X POST --header "Content-Type:application/cbor" --data-binary @tx.bin http://[host]:[port]/api/submit/tx
```

## Possible errors

If the `Content-Type` is not specified as `application/cbor` the webapi server will respond with
a `415 Unsupported Media Type` HTTP status code.

For other errors, the webapi will respond with a 200 HTTP status code, and a chunk of JSON which
for the success case will be:
```
{ "status": "success"
, "errorMsg": "No error"
}
```
For the fail case, the `"status"` field will contain `"fail"` and the `"errMsg"` field will contain
more information.


[BuildDoc]: https://github.com/input-output-hk/cardano-explorer/blob/master/doc/building.md
[NodeRepo]: https://github.com/input-output-hk/cardano-node
[JS-Wasm]: https://github.com/input-output-hk/js-cardano-wasm
