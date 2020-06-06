-- Hand written migration because I cann't get the schema migration tool to work.
-- see doc/schema-management.md for context.
-- `cabal run cardano-db-sync-db-tool -- create-migration --mdir schema/` does not work.
-- I've tried running it from within nix-shell.  I get the following:
--
--
-- cabal: Could not resolve dependencies:
-- [__0] trying: cardano-binary-test-1.3.0 (user goal)
-- [__1] unknown package: quickcheck-instances (dependency of
-- cardano-binary-test)
-- [__1] fail (backjumping, conflict set: cardano-binary-test,
-- quickcheck-instances)
-- After searching the rest of the dependency tree exhaustively, these were the
-- goals I've had most trouble fulfilling: cardano-binary-test,
-- quickcheck-instances

CREATE FUNCTION migrate() RETURNS void AS $$

BEGIN
  EXECUTE 'create table tx_body (id serial8 primary key unique, 
                                 hash hash32type not null, 
                                 body bytea not null)';
  EXECUTE 'CREATE INDEX idx_tx_body_hash ON tx_body(hash);';
  EXECUTE 'ALTER TABLE "tx_body" ADD CONSTRAINT "unique_tx_body" UNIQUE("hash")' ;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
