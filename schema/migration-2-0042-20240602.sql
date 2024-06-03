-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 42 THEN
    EXECUTE 'ALTER TABLE "off_chain_vote_data" DROP COLUMN "title"' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" DROP COLUMN "abstract"' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" DROP COLUMN "motivation"' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" DROP COLUMN "rationale"' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_gov_action_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"title" VARCHAR NOT NULL,"abstract" VARCHAR NOT NULL,"motivation" VARCHAR NOT NULL,"rationale" VARCHAR NOT NULL)' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_drep_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"payment_address" VARCHAR NULL,"given_name" VARCHAR NOT NULL,"objectives" VARCHAR NULL,"motivations" VARCHAR NULL,"qualifications" VARCHAR NULL,"image_url" VARCHAR NULL,"image_hash" VARCHAR NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
