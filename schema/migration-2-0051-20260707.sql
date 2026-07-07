-- Leios certificate persistence (Dijkstra LeiosCert).
-- block.leios_cert_signers:   the committee-signer bitfield (one bit per committee seat).
-- block.leios_cert_signature: the 48-byte BLS12-381 aggregate signature.
-- leios_cert_signer:          one row per pool that signed a block's LeiosCert, resolved from the
--                             signer bitfield against the committee (stake-ordered pool distribution
--                             of the parent ledger state, apOldLedger). Enables per-pool
--                             certification participation and certifying-stake-per-block queries.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 51 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "leios_cert_signers" BYTEA NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "leios_cert_signature" BYTEA NULL' ;
    EXECUTE '
      CREATE TABLE "leios_cert_signer" (
        "id"           SERIAL8    PRIMARY KEY,
        "block_id"     BIGINT     NOT NULL,
        "pool_hash_id" BIGINT     NOT NULL,
        "seat_index"   word31type NOT NULL
      )' ;
    EXECUTE 'CREATE INDEX "idx_leios_cert_signer_block_id" ON "leios_cert_signer" ("block_id")' ;
    EXECUTE 'CREATE INDEX "idx_leios_cert_signer_pool_hash_id" ON "leios_cert_signer" ("pool_hash_id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
