-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 2 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "epoch_no" uinteger NULL' ;

    -- Hand written SQL to insert the epoch_no field.
    -- Update all non-EBBs.
    UPDATE block SET epoch_no = div (slot_no, 21600) WHERE slot_no IS NOT NULL ;

    -- Update EBBs.
    UPDATE block SET epoch_no =
      CASE WHEN previous_block.epoch_no IS NULL
        THEN 0
        ELSE previous_block.epoch_no + 1
        END
      FROM block AS previous_block
      WHERE block.previous = previous_block.id AND block.epoch_no IS NULL ;

    -- Update the initial EBB.
    UPDATE block SET epoch_no = 0 where block.previous = 1 ;
    --

    UPDATE schema_version SET stage_two = 2 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
