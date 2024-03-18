-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 36 THEN
    EXECUTE 'ALTER TABLE "voting_anchor" ADD COLUMN "type" anchorType NOT NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" DROP CONSTRAINT "unique_voting_anchor"' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD CONSTRAINT "unique_voting_anchor" UNIQUE("data_hash","url","type")' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "language" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "comment" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "title" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "abstract" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "motivation" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "rationale" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "is_valid" BOOLEAN NULL' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_author"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"name" VARCHAR NULL,"witness_algorithm" VARCHAR NOT NULL,"public_key" VARCHAR NOT NULL,"signature" VARCHAR NOT NULL,"warning" VARCHAR NULL)' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_reference"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"label" VARCHAR NOT NULL,"uri" VARCHAR NOT NULL,"hash_digest" VARCHAR NULL,"hash_algorithm" VARCHAR NULL)' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_external_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"title" VARCHAR NOT NULL,"uri" VARCHAR NOT NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
