# Migrations

Release `13.1.0.0` first introduces a new way to enumerate releases, based on how it affects the db.
This is `a.b.c.d`. where
- `a`: schema breaking change, needs resync from genesis.
- `b`: schema change with a migraton.
- `c`: semantic change without a schema change. Resyncing would result in a semanticaly different db, ie
different values.
- `d`: no semantic change to the db.

## Upgrading to 13.1.1.x

In order to upgrade from 13.0.x or 13.1.0.x to 13.1.1.x resyncing is not necessary and no special
flags are required from the user. DBSync will automatically spawn a fixing procedure, which fixes
old values related to plutus data and scripts. This process when upgrading from 13.0.x takes a few
hours and can be skipped with `skip-fix`. Ather that, if upgrading from 13.0.x schema migrations
will run on top of the existing db. Finally a ledger replay follows.

### Ledger replay

Release 13.1.1.x drops the ledger snaphot serialisation compatibility. This means it's not able to
parse older ledger snapshots. DBSync will delete any existing snapshot and will replay the ledger
rules from genesis. This doesn't mean a rollback to genesis. No db data are deleted.

During the ledger replay DBSync updates only some values of the db at the `ada_pots` table, as it
fixes in place the issue [#942].

## Upgrading to 13.1.0.x

In order to upgrade from 13.0.x to 13.1.0.x resyncing is not necessary and requires no special flags
from the user. DBSync will automatically spawn a fixing procedure, which fixes old values
related to plutus data. After that schema migrations will run on top of the existing db.

## Migrations stages
Release `13.1.0.x` and later uses 4 stages of migrations.
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

