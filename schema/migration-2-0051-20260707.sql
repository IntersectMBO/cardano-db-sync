-- Leios certificate persistence (Dijkstra LeiosCert), consolidated.
-- has_leios_cert:            true for Dijkstra blocks that carry a LeiosCert.
-- eb_announcement_hash/size: extracted from the Praos header's leios announcement.
-- leios_cert_signers:        the committee-signer bitfield (one bit per committee seat).
-- leios_cert_signature:      the 48-byte BLS12-381 aggregate signature.
-- leios_cert_signer:         one row per pool that signed, resolved from the signer bitfield
--                            against the stake-ordered committee of the parent ledger state.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 51 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "has_leios_cert" BOOLEAN NOT NULL DEFAULT false' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "eb_announcement_hash" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "eb_announcement_size" word31type NULL' ;
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
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
