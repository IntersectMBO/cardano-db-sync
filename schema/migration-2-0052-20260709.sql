-- Leios committee persistence (Dijkstra), per epoch.
-- leios_committee: one row per committee seat for each epoch, recording the full
--                  "everyone votes" stake-ordered pool distribution (not just the
--                  seats that signed a given block's LeiosCert). seat_index is the
--                  committee position (ascending active stake, ties by pool-id) and
--                  weight is the pool's normalised active stake in that distribution.
--                  block_id is the epoch-boundary block that populated the committee,
--                  used to key rollbacks (identical to leios_cert_signer).

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 52 THEN
    EXECUTE '
      CREATE TABLE "leios_committee" (
        "id"           SERIAL8          PRIMARY KEY,
        "block_id"     BIGINT           NOT NULL,
        "epoch_no"     word31type       NOT NULL,
        "seat_index"   word31type       NOT NULL,
        "pool_hash_id" BIGINT           NOT NULL,
        "weight"       DOUBLE PRECISION NOT NULL
      )' ;
    EXECUTE 'CREATE INDEX "idx_leios_committee_block_id" ON "leios_committee" ("block_id")' ;
    EXECUTE 'CREATE INDEX "idx_leios_committee_epoch_no" ON "leios_committee" ("epoch_no")' ;
    EXECUTE 'CREATE INDEX "idx_leios_committee_pool_hash_id" ON "leios_committee" ("pool_hash_id")' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
