-- Give each column the domain its Haskell field already has: word64type where the field
-- is a Word64, as param_proposal already stores the matching parameters.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 55 THEN

    ALTER TABLE "committee_member" ALTER COLUMN "expiration_epoch" TYPE word64type USING "expiration_epoch"::numeric ;

    ALTER TABLE "committee" ALTER COLUMN "quorum_numerator" TYPE word64type USING "quorum_numerator"::numeric ;
    ALTER TABLE "committee" ALTER COLUMN "quorum_denominator" TYPE word64type USING "quorum_denominator"::numeric ;

    ALTER TABLE "epoch_param" ALTER COLUMN "min_fee_a" TYPE word64type USING "min_fee_a"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "min_fee_b" TYPE word64type USING "min_fee_b"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "max_block_size" TYPE word64type USING "max_block_size"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "max_tx_size" TYPE word64type USING "max_tx_size"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "max_bh_size" TYPE word64type USING "max_bh_size"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "max_epoch" TYPE word64type USING "max_epoch"::numeric ;
    ALTER TABLE "epoch_param" ALTER COLUMN "optimal_pool_count" TYPE word64type USING "optimal_pool_count"::numeric ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
