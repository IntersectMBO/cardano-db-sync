-- Leios visibility on the block table.
-- has_leios_cert: true for Dijkstra blocks that carry a LeiosCert in dbbLeiosCert.
-- eb_announcement_hash / eb_announcement_size: extracted from the Praos header's
-- hbLeiosEbAnnouncement when the block announces a new EB.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 48 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "has_leios_cert" BOOLEAN NOT NULL DEFAULT false' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "eb_announcement_hash" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "eb_announcement_size" word31type NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
