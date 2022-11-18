# Migrations

Release `13.1.0.0` introduces a new way to enumerate releases, based on how it affects the db.
This is `a.b.c.d`. where
- `a`: schema breaking change, needs resync from genesis.
- `b`: schema change with a migraton.
- `c`: semantic change without a schema change. Resyncing would result in a semanticaly different db, ie
different values.
- `d`: no semantic change to the db.

## Upgrading to 13.1.0.0

In order to upgrade from 13.0.x to 13.1.0.0 resyncing is not necessary and requires no special flags
from the user. DBSync will automatically spawn a fixing procedure, which fixes old values
related to plutus data. After that schema migrations will run on top of the existing db.

Release `13.1.0.0` uses 4 stages of migrations.
- `stage 1`: introduces basic postgres types. These cannot be modified or extended.
- `stage 2`: introduces basic tables and their constraints. `13.1.0.0` brings many
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

