  ------------------------------------------------------------
--
-- Schema to store blocks for the Shelley era of the Cardano chain in a PostgreSQL database.
-- This schema is based on that for the Byron era, but for Shelley era blocks only.
--
-- Kevin Hammond, IOHK
--       2020-04-10   KH        Adapted original Byron version
--       2020-04-13   KH        Created Shelley-only version
--       2020-04-14   KH        Updated types and included transaction metadata
--       2020-04-14   KH        Worked through with Jared, checked sizes and made minor updates
--       2020-04-15   KH        Updated metadata format, updated address format, added pointer type, added stake key lookup table, added epoch no in block
--       2020-04-16   KH        Changed address format to bytea based on suggestion by Duncan
--       2020-04-17   KH        Added performance data, added era type, covers both Shelley and Byron blocks and transactions
--       2020-06-05   Erik      Update parameter_update table after ledger-specs changes
--
------------------------------------------------------------



------------------------------------------------------------
--
-- Key open issues: X means the issue has been resolved
--
------------------------------------------------------------


-- X 1.  Do we need historic data for pool registrations etc. or just live information?
-- X     (Yes)

-- X 2.  Are withdrawals needed in the transactions?
-- X     (Yes.  We need this to calculate wallet reward totals from the on-chain data.)

-- X 3.  Are there other forms of transaction metadata that need to be recorded?
-- X     (Yes - these have been added.)

-- X 4.  Do we need to record protocol parameter changes
-- X     (Yes. will need to follow the voting procedure to determine this, this could be for later implementation)

-- X 5.  Can script and key hashes be treated identically (address hashes are a different size)
-- X      (No.  script hash is an address, stake key hash is an address, pool operator key hash no - to do: ask Jared to check each of these)
-- X      (don't need to store scripts, since not executed, only used for authorisation)
-- X      (do we need to distinguish script hash from key hash - possibly, since there could in theory be a collision?
-- X      We have decided not to distinguish them.)

-- X 6.  Do we need to record pointer addresses as an additional type, or are they just internal to the ledger?
-- X      (Yes.  Address type will change, but could have (payment credential [hash],staking reference[NULL,hash,pointer - [block,tx,cert] triple])

--   7.  Are signatures variable sized?
--       (probably not, but needs to be confirmed - just wrapped up cryptonite addresses)

--   8.  Are there additional constraints on data formats that should be included in the schema (e.g. data ranges)
--       (To do: check that all the constraints are still correct)

-- X 9.  Are alternatives handled correctly?  Is there a better way to do this in PostgreSQL than just using tables
-- X      (Looks like .)

-- X 10. Can we run a central node to access treasury/reward info rather than reconstructing this information
-- X      (No. We will need to record all the data we need in a local node.  Beware of performance implications in the implementation.)

-- X 11. Do we need to merge with the existing Byron schema or can we maintain two schemas
-- X      (Merging is preferred.)

--   12. Jared has suggested recording the stake that is held by each staking key.  I have included this as a table
--       but this could be expensive to maintain (both time and space), especially if historic data is recorded.

--   13. slot_no is used in various places, but block_no/blockid would be equivalents if these were preferred

--   14. We need to normalise the schema and consider changing some types to tables for e.g. performance/storage reasons

-- X 15. Do the ledger rules accept a Stake Key de-registration for a Stake Key that does not exist?
-- X     No, ledger rules reject this.

-- X 16. Once an address is delegated to a pool can it be undelegated (other than by delegating to another pool)?
-- X     Yes, by de-registering the stake key.

------------------------------------------------------------



----------------------------------------
--
-- Which Cardano Era does this format refer to
-- Used in Blocks
--
----------------------------------------

CREATE TYPE era as ENUM (
       'Byron', 'Shelley'
);



----------------------------------------
--
-- Basic Type Definitions
--
----------------------------------------

-- Coin values in Lovelace.
CREATE DOMAIN lovelace AS bigint
  CHECK (VALUE >= 0 AND VALUE <= 45000000000000000);

-- Unsigned (non-negative) integers, used in many places.
CREATE DOMAIN uinteger AS integer
  CHECK (VALUE >= 0);

-- Positive integers.
CREATE DOMAIN nonnegative AS integer
  CHECK (VALUE > 0);

-- Copied from the CDDL spec.  interval may not be the best name?
CREATE TYPE interval as (
   minval   uinteger,
   maxval   uinteger
   );

CREATE TYPE rational as (
   num      uinteger,
   denom    nonnegative
   );



----------------------------------------
--
-- Transaction Indexes
--
----------------------------------------

-- Transactions contain lists of input and lists of outputs.
-- So we can identify these elements by the 0-based index within the list.
-- To do: confirm limit
CREATE DOMAIN txindex AS smallint
  CHECK (VALUE >= 0 AND VALUE < 1024);


----------------------------------------
--
-- Address and Key Hashes
--
----------------------------------------


-- Many entities are identified by hash. Most hashes are 256bit, 32bytes.
CREATE DOMAIN hash AS bytea
  CHECK (octet_length (VALUE) = 32);


-- Address hashes are, however, 224bit, 28bytes.
CREATE DOMAIN addr_hash AS bytea
  CHECK (octet_length (VALUE) = 28);


-- NEW for SHELLEY
-- Used to separate address hashes for semantic reasons
-- The hashes are credentials, which may be either key hashes or script hashes
-- To do: Are these hashes or addr_hashes?

CREATE DOMAIN payment_addr_hash AS hash;
CREATE DOMAIN staking_addr_hash AS hash;
CREATE DOMAIN script_hash AS hash;


-- NEW for SHELLEY.

--  New format for addresses
CREATE DOMAIN address as bytea
  -- This level of specificity may be overkill, but is probably a useful safety check
  CHECK (  octet_length (VALUE) = 28            -- bootstrap address/enterprise address
        or octet_length (VALUE) = 56            -- Shelley staking address
        or octet_length (VALUE) = 94            -- pointer address
        );



----------------------------------------
--
-- Certificates and Signatures
--
----------------------------------------

-- NEW for SHELLEY: signatures. -- To do: verify the size of this.

CREATE DOMAIN signature AS bytea;


-- NEW for SHELLEY: VRF certificates. To do: verify the size of this.

CREATE DOMAIN vrf_certificate AS bytea;


-- NEW for SHELLEY: operational certificates.
CREATE TYPE operational_certificate as (
    hot_vkey        hash,                       -- KES key hash
    sequence_number uinteger,
    kes_period      uinteger,
    sigma           signature                   -- optional
  );



----------------------------------------
--
-- Other Types used with Transactions
--
----------------------------------------

-- NEW for SHELLEY: protocol version. To do: confirm that this is needed in the database
CREATE TYPE protocol_version as (
  major     uinteger,
  minor     uinteger
  );

CREATE DOMAIN nonce as hash;

-- NEW for SHELLEY: transaction metadata.
-- For simplicity, we store the metadata as JSON.  This will need to be converted when stored

CREATE TYPE tx_metadata as (
    label         uinteger,
    metadata      jsonb
    );

----------------------------------------
--
-- Schema Version
--
----------------------------------------

-- Schema version number. There should only ever be a single row in this table.
-- UNCHANGED FOR SHELLEY
CREATE TABLE version (
  number      uinteger      -- should this be NOT NULL?
);





----------------------------------------
--
-- Blocks
--
----------------------------------------


-- SHELLEY only - no NULL blocks
CREATE TABLE blocks (
  -- id
  blockid          hash             PRIMARY KEY,    -- PRIMARY KEY implies UNIQUE and NOT NULL.

  -- kind of block
  era              era               NOT NULL,      -- Which kind of block are we dealing with: Shelley, Byron etc

   -- header body
  epoch_no         uinteger          NOT NULL,       -- The epoch can in principle be derived from the slot number,
                                                     -- but since the mapping might change through protocol parameter updates,
                                                     -- it is sensible to record it here
  slot_no          uinteger          NOT NULL,

  block_no         uinteger          NOT NULL,       -- In the cardano-sl code base, this is the 'ChainDifficulty'
                                                     -- block header field, but it was never really about difficulty
                                                     -- but rather block height. However the cardano-sl code does
                                                     -- not increment this value for EBBs, so its not really a
                                                     -- block height.


  previous         hash
      REFERENCES blocks (blockid),                    -- Needs to be NULL-able because the first block has a previous
                                                      -- hash which does not exist in this table.

  block_issuer     hash,                              -- SHELLEY: Which pool produced the block.
  vrfKey           hash,                              -- SHELLEY: VRF verification key

  nonce_vrf        vrf_certificate,                   -- SHELLEY: nonce proof
  leader_vrf       vrf_certificate,                   -- SHELLEY: leader election value

  op_cert          operational_certificate,           -- SHELLEY: operational certificate
  proto_version    protocol_version,                  -- SHELLEY: protocol version

  merkle_root      hash,                              -- Could be NULL-able in BYRON era blocks to store EBBs
                                                      -- Will not be NULL in SHELLEY era blocks

  size             uinteger          NOT NULL,        -- Block size in bytes.

   -- signature
   body_signature signature                           -- SHELLEY: KES signature
);


----------------------------------------
--
-- Withdrawals - used to record deductions from rewards for accounting purposes
--
----------------------------------------


-- NEW for Shelley
-- Merges key and script hashs

CREATE TYPE withdrawal as (
       withdraw_from   hash,            -- may be either script or key hash
       amount          lovelace         -- amount withdrawn
   );


----------------------------------------
--
-- Transactions
--
----------------------------------------


-- Transaction Body
-- ADDED metadata/hash and certificates for SHELLEY.
-- TTL (time to live) is not included since the transaction will only be on the chain if it has succeeded
-- May need to add payment verification key map, and script map.
-- Optional protocol parameter updates are not included - they are recorded separately

CREATE TABLE txs (
  txid            hash                   PRIMARY KEY,
  in_blockid      hash                   NOT NULL REFERENCES blocks (blockid) ON DELETE CASCADE,

  fee             lovelace               NOT NULL,
  withdrawals     withdrawal ARRAY,                   -- SHELLEY: NULL in BYRON
  metadata        tx_metadata                         -- SHELLEY: NULL in BYRON
);



-- Transaction output
-- output address changed from hash to address type FOR SHELLEY

CREATE TABLE txouts (
  in_txid      hash                REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  address      address             NOT NULL,
  value        lovelace            NOT NULL,

  PRIMARY KEY (in_txid, index)
);


-- Transaction input
-- UNCHANGED FOR SHELLEY

CREATE TABLE txins (
  in_txid      hash               REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  txout_txid   hash               NOT NULL,
  txout_index  txindex            NOT NULL,

  PRIMARY KEY (in_txid, index),
  FOREIGN KEY (txout_txid, txout_index)
    REFERENCES txouts (in_txid, index)
);


-- UTxO
-- UNCHANGED FOR SHELLEY

CREATE TABLE utxo (
  txid         hash ,
  index        txindex,

  PRIMARY KEY (txid, index),
  FOREIGN KEY (txid, index)
    REFERENCES txouts (in_txid, index)
);


-- Resolved transaction input - which transaction inputs appear in the transaction output
-- UNCHANGED FOR SHELLEY

CREATE VIEW txins_resolved AS
  SELECT txins.*,
         address,
         value
  FROM txins INNER JOIN txouts ON
       (txins.txout_txid  = txouts.in_txid AND
        txins.txout_index = txouts.index);


-- Resolved UTXO - which UTXOs appear in the transaction output
-- UNCHANGED FOR SHELLEY

CREATE VIEW utxo_resolved AS
  SELECT utxo.*,
         address,
         value
  FROM utxo INNER JOIN txouts ON
       (utxo.txid  = txouts.in_txid AND
        utxo.index = txouts.index);




------------------------------------------------------------------------------------------------------------------------
--
--
-- The remaining data is derived from Shelley transactions or ledger state, but is provided as separate tables to make it simpler to use
--
--
------------------------------------------------------------------------------------------------------------------------



----------------------------------------
--
-- Treasury and Reserves
--
----------------------------------------


-- NEW for SHELLEY
-- This records the total value of the treasury and reserves at the beginning of the specified epoch.

CREATE TABLE central_funds (
   epoch_no           uinteger        PRIMARY KEY,
   treasury           lovelace        NOT NULL,
   reserves           lovelace        NOT NULL
   );


----------------------------------------
--
-- Rewards and Delegation
--
----------------------------------------


-- NEW for SHELLEY
-- Basic rewards data.
-- What reward has been earned in the epoch by delegating to the specified pool id.
-- This design allows rewards to be discriminated based on how they are earned.

CREATE TABLE rewards (
   rewards_address      staking_addr_hash NOT NULL,
   instantaneous        boolean           NOT NULL,
   pool_id              hash              NOT NULL,                         -- Arbitrary if an instantaneous reward - needed for primary key
   epoch_no             uinteger          NOT NULL,
   reward               lovelace          NOT NULL,

   PRIMARY KEY (rewards_address,pool_id,epoch_no,instantaneous)
   );


-- NEW for SHELLEY
-- Basic delegation data.
-- What has been delegated to a pool and when.  Where will the rewards be recorded.
-- The epoch can be deduced from the unique slot number.
-- Note the overlap with the rewards table in the first two fields.

CREATE TABLE delegation (
   rewards_address      staking_addr_hash  NOT NULL,
   pool_id              hash               NOT NULL,
   slot_no              uinteger           NOT NULL,      -- technically redundant, but useful
   amount               lovelace           NOT NULL,
   in_tx                hash               NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (rewards_address,pool_id,in_tx)
   );


-- NEW for SHELLEY
-- When was a staking key registered

CREATE TABLE stake_key_registration (
   stakekey_address     staking_addr_hash NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (stakekey_address,in_tx)
   );


-- NEW for SHELLEY
-- When was a staking key unregistered

CREATE TABLE stake_key_deregistration (
   stakekey_address     staking_addr_hash NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (stakekey_address,in_tx)
   );


-- NEW for SHELLEY
-- When was a script registered

CREATE TABLE script_registration (
   script_address       script_hash       NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (script_address,in_tx)
   );


-- NEW for SHELLEY
-- When was a script unregistered

CREATE TABLE script_deregistration (
   script_address       script_hash       NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (script_address,in_tx)
   );


-- NEW for SHELLEY
-- Parameter update - there can only be one successful update per epoch.

CREATE TABLE parameter_update (
   epoch_no             uinteger          PRIMARY KEY,
   min_fee              uinteger          NOT NULL,
   max_fee              uinteger          NOT NULL,
   max_block_size       uinteger          NOT NULL,
   max_tx_size          uinteger          NOT NULL,
   max_bh_size          uinteger          NOT NULL,
   key_deposit          lovelace          NOT NULL,
   pool_deposit         lovelace          NOT NULL,
   max_epoch            uinteger          NOT NULL,
   n_optimal            uinteger          NOT NULL,
   influence            rational          NOT NULL,
   monetary_expansion_rate interval       NOT NULL,
   treasury_growth_rate interval          NOT NULL,
   decentralisation     interval          NOT NULL,
   entropy              nonce,                              -- NULL if the nonce is not present
   protocol_version     protocol_version  NOT NULL,
   min_utxo_value       lovelace          NOT NULL
   );




----------------------------------------
--
-- Pool Specific Information
--
----------------------------------------


-- Tickers may be 1-5 bytes
CREATE DOMAIN ticker AS bytea
  CHECK (octet_length (VALUE) = 5);

-- URL for pool description.
CREATE DOMAIN url AS bytea
  CHECK (octet_length (VALUE) = 64);

-- Percentages in the range 0%-100% -- recorded to two decimal places
CREATE DOMAIN percentage AS decimal (5,2)
  CHECK (VALUE >= 0 AND VALUE <= 100);


-- NEW for SHELLEY
-- Pool Parameters.
-- Records important pool-specific data.

CREATE TABLE pool_params (
   pool_id              hash              NOT NULL,          -- hash of the pool VRF key
   owners               hash ARRAY        NOT NULL,          -- key hashes for the owners - at least one owner is needed
   ticker_id            ticker            NOT NULL,
   pledge               lovelace          NOT NULL,
   reward_address       staking_addr_hash NOT NULL,          -- overall pool rewards
   pool_url             url,
   metadata_hash        hash,
   margin               percentage        NOT NULL,
   fixed_cost           uinteger          NOT NULL,
   registered           uinteger          NOT NULL,          -- in which slot was this metadata registered/re-registered

   PRIMARY KEY (pool_id,registered)
   );


-- NEW for SHELLEY
-- Pool Retirement

CREATE TABLE pool_retired (
   pool_id              hash NOT NULL,                        -- hash of the pool VRF key
   retired              uinteger NOT NULL,                    -- when retirement occurred (epoch no)

   PRIMARY KEY (pool_id,retired)
   );


-- NEW for SHELLEY
-- Announcement of Pool Retirement

CREATE TABLE pool_retiring (
   pool_id              hash NOT NULL,                        -- hash of the pool VRF key
   announced            uinteger NOT NULL,                    -- which slot was retirement announced
   retiring             uinteger NOT NULL,                    -- in which epoch is retirement planned

   PRIMARY KEY (pool_id,announced)
  );


-- NEW for SHELLEY
-- How much stake is held by a staking key.

CREATE TABLE stake (
   stakekey_address     staking_addr_hash NOT NULL,
   slot_no              uinteger          NOT NULL,           -- when was the change made
   stake                lovelace          NOT NULL,

   PRIMARY KEY (stakekey_address,slot_no)
);



-- NEW for SHELLEY
-- Pool Performance Data, recorded at the end of the epoch
-- influence factor, active slot coefficient, decentralisation can be obtained from the
-- protocol update parameter table (with some effort to choose those in force for the correct epoch)

CREATE TABLE pool_performance (
   pool_id                  hash              NOT NULL,         -- hash of the pool VRF key
   epoch_no                 uinteger          NOT NULL,         -- what epoch does the data refer to
   desirability             float             NOT NULL,         -- what is the relative desirability of the pool
   blocks_produced          uinteger          NOT NULL,         -- How many blocks did the pool produce
   apparent_performance     percentage        NOT NULL,         -- what is the apparent performance of the pool
   total_stake              lovelace          NOT NULL,         -- how much stake does the pool control
   delegated_stake          lovelace          NOT NULL,         -- how much stake was delegated to the pool during the epoch
   pledge_honoured          boolean           NOT NULL,         -- was the pledge honoured at the end of the epoch - could be important to know since could impact rewards

   PRIMARY KEY (pool_id,epoch_no)
);
