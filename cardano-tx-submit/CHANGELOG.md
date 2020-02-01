# Revision history for cardano-tx-submit

## 1.3.0 -- January 2020

* API change: require content-type `application/cbor` for posted transactions.
* API change: require raw transaction binary format for posted transactions.
* Add more specific error message when posted transaction is hex encoded.
* Include example testnet configuration and update README on using it.
* Documentation for submission API and how to generate example transactions.
* Update dependencies to latest versions.
* Docker image: log all runit services to stdout
* Initial documentation on how to use build and run the components in docker

## 1.2.2 -- January 2020

* Update dependencies to latest versions.
* Service added to docker files.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.
* Improve logging of tx submit responses.
* Add a README.
* Add QA document on how to test the component.

## 1.2.0 -- December 2019

* First release of the Tx Submission endpoint
