-- Leios (Dijkstra) protocol parameters, made governance-configurable in w36 (CIP-0164):
-- committee size, quorum stake threshold, and the announcement / vote / diffusion period
-- lengths (in milliseconds). Recorded per epoch on epoch_param; NULL before the Dijkstra era.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 56 THEN
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_committee_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_quorum_stake_threshold" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_announcement_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_vote_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_diffusion_period_length" word64type NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
