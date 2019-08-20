-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 1 THEN
    EXECUTE 'CREATe TABLE "block"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"slot_no" uinteger NULL,"block_no" uinteger NOT NULL,"previous" INT8 NULL,"merkel_root" hash32type NULL,"size" uinteger NOT NULL)' ;
    EXECUTE 'ALTER TABLE "block" ADD CONSTRAINT "unique_block" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "block" ADD CONSTRAINT "block_previous_fkey" FOREIGN KEY("previous") REFERENCES "block"("id")' ;
    EXECUTE 'CREATe TABLE "tx"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"block" INT8 NOT NULL,"fee" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "tx" ADD CONSTRAINT "unique_tx" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "tx" ADD CONSTRAINT "tx_block_fkey" FOREIGN KEY("block") REFERENCES "block"("id")' ;
    EXECUTE 'CREATe TABLE "tx_out"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" txindex NOT NULL,"address" hash28type NOT NULL,"value" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD CONSTRAINT "unique_txout" UNIQUE("tx_id","index")' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD CONSTRAINT "tx_out_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "tx_in"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_in_id" INT8 NOT NULL,"tx_out_id" INT8 NOT NULL,"tx_out_index" txindex NOT NULL)' ;
    EXECUTE 'ALTER TABLE "tx_in" ADD CONSTRAINT "unique_txin" UNIQUE("tx_out_id","tx_out_index")' ;
    EXECUTE 'ALTER TABLE "tx_in" ADD CONSTRAINT "tx_in_tx_in_id_fkey" FOREIGN KEY("tx_in_id") REFERENCES "tx"("id")' ;
    EXECUTE 'ALTER TABLE "tx_in" ADD CONSTRAINT "tx_in_tx_out_id_fkey" FOREIGN KEY("tx_out_id") REFERENCES "tx"("id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 1 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
