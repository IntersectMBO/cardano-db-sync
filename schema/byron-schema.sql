-- Schema to store blocks for the Byron era of the Cardano chain in a PostgreSQL database.
-- This schema will need to be extended for the Shelly era.

-- Coin values in Lovelace.
CREATE DOMAIN lovelace AS bigint
  CHECK (VALUE >= 0 AND VALUE <= 45000000000000000);

-- Many entities are identified by hash. Our hashes are 256bit, 32bytes.
CREATE DOMAIN hash AS bytea
  CHECK (octet_length (VALUE) = 32);

-- Transactions contain lists of input and lists of outputs.
-- So we can identify these elements by the 0-based index within the list.
CREATE DOMAIN txindex AS smallint
  CHECK (VALUE >= 0 AND VALUE < 1024);

-- Unsigned (non-negative) integers, used in many places.
CREATE DOMAIN uinteger AS integer
  CHECK (VALUE >= 0);

-- Schema version number. There should only ever be a single row in this table.
CREATE TABLE version (
  number      uinteger
);

-- There are two kinds of blocks (Main and Epoch Boundary) and we need to store both. This
-- table stores a single chain. When a chain rewrite occurs, the deletion of the old block(s)
-- and its replacement with new block(s) should be done in a single transaction.
-- For EBBs, the 'slot_no' field will be 'NULL' and for all Main blocks, 'slot_no' will be
-- 'NOT NULL'.
CREATE TABLE blocks (
  blockid      hash PRIMARY KEY,        -- PRIMARY KEY implies UNIQUE and NOT NULL.

  epoch_no     uinteger NOT NULL,
  slot_no      uinteger,                -- NULL for EBB and NOT NULL for Main blocks.

  block_no     uinteger NOT NULL,       -- In the cardano-sl code base, this is the 'ChainDifficulty'
                                        -- block header field, but it was never really about difficulty
                                        -- but rather block height. However the cardano-sl code does
                                        -- not increment this value for EBBs, so its not really a
                                        -- block height.

  previous     hash
      REFERENCES blocks (blockid),      -- Needs to be NULL-able because the first block has a previous
                                        -- hash which does not exist in this table.

  merkle_root  hash,                    -- Needs to be NULL-able so we can store EBBs.
  size         uinteger NOT NULL        -- Block size in bytes.
);

CREATE TABLE txs (
  txid         hash     PRIMARY KEY,
  in_blockid   hash     NOT NULL REFERENCES blocks (blockid) ON DELETE CASCADE,
  fee          lovelace NOT NULL
);

CREATE TABLE txouts (
  in_txid      hash     REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  address      hash     NOT NULL,
  value        lovelace NOT NULL,

  PRIMARY KEY (in_txid, index)
);

CREATE TABLE txins (
  in_txid      hash     REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  txout_txid   hash     NOT NULL,
  txout_index  txindex  NOT NULL,

  PRIMARY KEY (in_txid, index),
  FOREIGN KEY (txout_txid, txout_index)
    REFERENCES txouts (in_txid, index)
);

CREATE TABLE utxo (
  txid         hash ,
  index        txindex,

  PRIMARY KEY (txid, index),
  FOREIGN KEY (txid, index)
    REFERENCES txouts (in_txid, index)
);


CREATE VIEW txins_resolved AS
  SELECT txins.*,
         address,
         value
  FROM txins INNER JOIN txouts ON
       (txins.txout_txid  = txouts.in_txid AND
        txins.txout_index = txouts.index);

CREATE VIEW utxo_resolved AS
  SELECT utxo.*,
         address,
         value
  FROM utxo INNER JOIN txouts ON
       (utxo.txid  = txouts.in_txid AND
        utxo.index = txouts.index);
