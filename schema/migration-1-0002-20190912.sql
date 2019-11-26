-- Drop all VIEWs linked to the 'cexplorer' table.

-- Many schema migration changes will fail if they operate on a table that is part of a VIEW.
-- So we drop all VIEWs here and recreate them later.

CREATE FUNCTION drop_cexplorer_views () RETURNS void language plpgsql as $$

DECLARE vname text;

BEGIN
  for vname in
      select '"' || table_name || '"'
	    from information_schema.views
        where table_catalog like '%explorer' and table_schema = 'public'
    loop
      execute format ('drop view if exists %s cascade ;', vname) ;
      raise notice 'Dropping view : %', vname ;
    end loop ;
end $$ ;

SELECT drop_cexplorer_views () ;

DROP FUNCTION drop_cexplorer_views () ;
