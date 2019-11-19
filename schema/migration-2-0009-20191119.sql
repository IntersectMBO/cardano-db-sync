-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 9 THEN
    EXECUTE 'ALTER TABLE "slot_leader" ADD COLUMN "description" VARCHAR' ;

    -- -------------------------------------------------------------------------
    -- Custom SQL to clean up tables.
    update slot_leader
      set description =
    	(select desciption
    	  from slot_leader as slot_leader_inner
          where slot_leader.id = slot_leader_inner.id
          ) ;

	alter table slot_leader alter column description set not null ;
    alter table slot_leader drop column desciption ;
    -- -------------------------------------------------------------------------
    UPDATE schema_version SET stage_two = 9 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
