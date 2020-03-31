-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 12 THEN
    EXECUTE 'ALTER TABLE "epoch" ADD COLUMN "blk_count" uinteger' ;
    -- -------------------------------------------------------------------------
    -- Hand written SQL to update the blk_count column and then set it NOT NULL.

	update epoch
	  set blk_count =
	    (select count (*)
	      from block join epoch as epoch_inner on block.epoch_no = epoch_inner.no
	      where epoch.id = epoch_inner.id
	      ) ;

    alter table epoch alter column blk_count set not null;

    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_two = 12 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
