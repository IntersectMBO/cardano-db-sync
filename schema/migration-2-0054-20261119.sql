-- Widen the protocol version minors: CIP-0203 packs a software tag into the header
-- minor, so the value needs the full unsigned 32 bits. proto_major rides along in the
-- same statement, so block is rewritten once rather than once per column. Existing
-- negative rows are left alone on purpose; the sign is the only marker of a truncated
-- row, and repairing the high bits needs the node's headers. See issue #2190.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 54 THEN

    ALTER TABLE "block"
      ALTER COLUMN "proto_minor" TYPE word63type
      , ALTER COLUMN "proto_major" TYPE word63type ;
    ALTER TABLE "epoch_param" ALTER COLUMN "protocol_minor" TYPE word63type ;
    ALTER TABLE "param_proposal" ALTER COLUMN "protocol_minor" TYPE word63type ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
