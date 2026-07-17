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
Since 13.7.1.0 this repair runs automatically on first startup after upgrade
(migration `2-0048`); these scripts are for operators on older release lines and
for manual verification.

- **validate-epoch-table.sql** recomputes the `epoch` table from `tx`/`block` and reports rows that disagree. Read-only.
- **fix-epoch-table.sql** repairs the `epoch` table in place by recomputing and upserting. Modifies data.
- **check-and-fix-epoch-table.sh** wrapper that validates first, then fixes if there are mismatches.

## Database setup

- **postgresql-setup.sh** administers the db-sync database: create/drop/recreate, create the user, run migrations, dump the schema, and create/restore state snapshots (`--createdb`, `--run-migrations`, `--dump-schema`, `--create-snapshot`, `--restore-snapshot`, ...). Embedded in the NixOS service and used by the snapshot workflow.
- **postgresql-test.sh** `start`/`stop` initializes and runs (or stops) a local throwaway PostgreSQL server for testing. Used by CI.

## Development helpers

- **fourmolize.sh** formats all Haskell sources in place with fourmolu, then fails if that produced any changes. Run it before pushing to reproduce the formatting check CI enforces (CI runs the `haskell-actions/run-fourmolu` action pinned to fourmolu 0.17.0.0, so use that same version locally).
- **git-pre-commit-hook** git pre-commit hook that runs hlint on staged Haskell files and aborts the commit on warnings, mirroring the CI hlint check (Hydra `checks.hlint`, hlint 3.8, `.hlint.yaml`). Install by symlinking it to `.git/hooks/pre-commit` (see `doc/hlint-stylish-haskell.md`). Formatting is a separate CI gate handled by `fourmolize.sh`.
- **secp256k1-setup.sh** `<git-sha>` installs a specific secp256k1 version on Linux and macOS.
- **run-everything-tmux.sh** dev helper that starts cardano-node and db-sync together in a tmux session.
