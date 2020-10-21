-- Drop all VIEWs linked to the 'cexplorer' table.

-- Many schema migration changes will fail if they operate on a table that is part of a VIEW.
-- So we drop all VIEWs here and recreate them later.

CREATE FUNCTION drop_cexplorer_views () RETURNS void language plpgsql as $$

DECLARE vname text;
DECLARE next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 2 THEN
    -- Only update the schema version if the versions are as expected.
    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;

  -- Always run the following.
  for vname in
      select '"' || table_name || '"'
	    from information_schema.views
        where table_catalog = current_database ()
          and table_schema = 'public'
          and table_name !~* 'pg_stat_.*'
    loop
      execute format ('drop view if exists %s cascade ;', vname) ;
      raise notice 'Dropping view : %', vname ;
    end loop ;
end $$ ;

SELECT drop_cexplorer_views () ;

DROP FUNCTION drop_cexplorer_views () ;
