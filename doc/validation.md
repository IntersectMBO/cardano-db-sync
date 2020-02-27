# Validation

This document will detail what validation and consistency checks the db-sync-node does and does
not do, and what assumptions it makes. It will also note what validation is done every time
the db-sync-node is restarted and what validation is only done when new data arrives from the
node.

The data stored in the database is designed to be an accurate but incomplete (eg cryptographic
signatures are elided) representation of the history of the blockchain so that the current
state or any state of the blockchain in the past can be reconstructed with an SQL query.


## Assumptions

The db-sync-node connects to a locally running Cardano node and fully trusts the information that node
provides. This means the db-sync-node can omit validation of the cryptographic signatures on blocks,
transactions etc because the db-sync-node can assume these were checked by the locally running node
which is providing blockchain data.


## Genesis Validation

When the db-sync-node is started for the first time it reads the same JSON Genesis configuration as
used by the Cardano node, validates the hash, extracts the Genesis Distribution and inserts the
Genesis Distribution into the database and prints the total supply at Genesis (in Ada) to the logs.
The Genesis Distribution generates a set of transaction output for which there are no transaction
inputs. Since the database schema requires that all output transaction are connected to block.
Therefore a "fake" genesis block is inserted into the database for the Genesis transaction outputs
to link to.

The first Epoch Boundary Block (EBB) has a previous block hash that is the hash of the JSON Genesis
configuration file. The first EBB cannot be inserted into the database unless the previous hash
already exists in the database. For main net this looks like:
```
Initial genesis distribution populated. Hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb
Total genesis supply of Ada: 31112484745.000000
```

Each time the db-sync-node is started, it again reads the JSON Genesis configuration, checks its hash,
validates that the Genesis Distribution is correct and again prints the total of the Genesis supply.
For main net:
```
Initial genesis distribution present and correct
Total genesis supply of Ada: 31112484745.000000
```


## Block Validation

Block validation is only done when blocks are added to the database. All blocks other than the
"fake" Genesis block used to introduce the Genesis Distribution have a previous block index that
must be the index of a block already present in the database. Insertion of a block into the
database is done in two steps:

* Look up the block's previous block hash in the `Block` table to get its index.
* Insert the new block into the `Block` table with the `previous` field set to the index found
  by the preceding lookup.

This results in a database that contains a chain of blocks as one would expect from a blockchain.


## Transaction Validation

Transaction validation is only done when transactions are added to the database. All transactions
contain an index into the `Block` table specifying the block in which the transaction was included.
Each transaction included in a block (ie specifically ignoring the Genesis Distribution
transactions) have one or more inputs and one or more outputs. Transactions are validated in the
db-sync node by checking that all the transaction inputs exist and that the value of the inputs
is greater than or equal to the value of the transaction outputs. As a by product of this validation
the fees associated with the transaction are recorded in the database.

