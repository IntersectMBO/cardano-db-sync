-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 36 THEN
    EXECUTE 'CREATe TABLE "committee_hash"("id" SERIAL8  PRIMARY KEY UNIQUE,"raw" hash28type NOT NULL,"has_script" BOOLEAN NOT NULL)' ;
    EXECUTE 'ALTER TABLE "committee_hash" ADD CONSTRAINT "unique_committee_hash" UNIQUE("raw","has_script")' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_author"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"name" VARCHAR NULL,"witness_algorithm" VARCHAR NOT NULL,"public_key" VARCHAR NOT NULL,"signature" VARCHAR NOT NULL,"warning" VARCHAR NULL)' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_reference"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"label" VARCHAR NOT NULL,"uri" VARCHAR NOT NULL,"hash_digest" VARCHAR NULL,"hash_algorithm" VARCHAR NULL)' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_external_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"off_chain_vote_data_id" INT8 NOT NULL,"title" VARCHAR NOT NULL,"uri" VARCHAR NOT NULL)' ;
    EXECUTE 'CREATe TABLE "epoch_state"("id" SERIAL8  PRIMARY KEY UNIQUE,"committee_id" INT8 NULL,"no_confidence_id" INT8 NULL,"constitution_id" INT8 NULL,"epoch_no" word31type NOT NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
