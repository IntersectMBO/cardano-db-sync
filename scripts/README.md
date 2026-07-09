# Scripts

Helper scripts for checking and maintaining a db-sync database, plus a few
development utilities. Run them from the repository root.

## Schema and data checks

Read-only checks against a populated database. `run-schema-checks.sh` runs the
three SQL scripts in turn; each can also be run on its own with
`psql -d <dbname> -f scripts/<script>.sql`.

- **run-schema-checks.sh** `<dbname>` runs the three check scripts below against a database.
- **test-uniqueness.sql** asserts UNIQUE invariants hold (no duplicate tx/block hashes and the like).
- **test-referential-integrity.sql** asserts foreign-key invariants hold.
- **test-value-domains.sql** asserts stored values are sane: ranges, non-negativity, epoch totals against a recompute, and duplicate or missing rows. On a mainnet-sized database a full run takes about 10 to 15 minutes.

## Epoch table repair

For the `epoch`-table corruption from issue #2118 (db-sync 13.7.0.0 to 13.7.0.4).

- **validate-epoch-table.sql** recomputes the `epoch` table from `tx`/`block` and reports rows that disagree. Read-only.
- **fix-epoch-table.sql** repairs the `epoch` table in place by recomputing and upserting. Modifies data.
- **check-and-fix-epoch-table.sh** wrapper that validates first, then fixes if there are mismatches.

## Database setup

- **postgresql-setup.sh** creates, starts, dumps, or restores a local PostgreSQL instance for db-sync. Used by the snapshot workflow.
- **postgresql-test.sh** checks that PostgreSQL is running and reachable.

## Development helpers

- **fourmolize.sh** formats all Haskell sources in place with fourmolu.
- **git-pre-commit-hook** git pre-commit hook that runs stylish-haskell and hlint on staged Haskell files.
- **secp256k1-setup.sh** `<git-sha>` installs a specific secp256k1 version on Linux and macOS.
- **gen-tx-submit-config.sh** generates a tx-submit config file from a genesis hash.
- **run-everything-tmux.sh** dev helper that starts cardano-node and db-sync together in a tmux session.
