# Schema Management

Schema management for the Cardano PostgreSQL database is a little more complicated than we would
like, but the scheme chosen allows for easy development, evolution and management of the database.

The database schema is defined in three stages, each stage consisting of one or more SQL migrations.
The stages are:

- `stage 1`: introduces basic postgres types. These cannot be modified or extended.
- `stage 2`: introduces basic tables and their constraints. `13.1.0.x` brings many
changes here, as it removes foreign, unique keys and a few fields. These files cannot
be modified or extended.
- `stage 3`: introduces only the indexes necessary to db-sync. Having unecessary
indexes during syncing slows down db-sync and so they are added later. Index
creation is idempotent and the `schema_version.stage_tree` field is ignored.
These files cannot be modified but they can be extended, in case users want to
introduce their own indexes from the begining.
- `stage 4`: introduces all the other indexes. By default these are the indexes
that were created by previous db-sync versions. This stage is executed when
db-sync has reached 30mins before the tip of the chain. It is advised to increase
the `maintenance_work_mem` from Postgres config to 0.5GB - 1GB to speed this
process (default is 64MB). Also use the default (2) or higher
`max_parallel_maintenance_workers`. These files can be modified or extended
by users.

All of the schema migrations in these three stages are written to be idempotent (so that they
"know" if they have already been applied).

The migration files all have file names of the form:
```
migration-1-0000-20190730.sql
```
where the `1` denotes "stage 1" of the SQL migration, the `0000` is the migration version and the
last number is the date. Listing the directory containing the schema and sorting the list will
order them in the correct order for applying to the database.

## Creating a Migration

Whenever the Haskell schema definition in `Cardano.Db.Schema` is updated, a schema migration can
be generated using the command:
```
export PGPASSFILE=config/pgpass-mainnet
cabal run cardano-db-tool -- create-migration --mdir schema/
```
which will only generate a migration if one is needed. It is usually best to run the test suite
(`cabal test cardano-db-sync db` which tests the migrations) first and then generate the migration.

[Persistent]: https://hackage.haskell.org/package/persistent
