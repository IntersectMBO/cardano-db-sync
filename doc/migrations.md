# Migrations

Release `13.1.0.0` first introduces a new way to enumerate releases, based on how it affects the db.
This is `a.b.c.d`. where
- `a`: schema breaking change, needs resync from genesis.
- `b`: schema change with a migraton.
- `c`: semantic change without a schema change. Resyncing would result in a semanticaly different db, ie
different values.
- `d`: no semantic change to the db.

## Upgrading to 13.2.0.x

In order to upgrade from 13.1.x.x to 13.2.0.x resyncing is not necessary and no special
flags are required from the user. DBSync will automatically perform the new migrations under `schema`
directory. Details about these migrations can be found in the release `Changelog.md`, in the `schema.md`
annotated as `13.2` or in the schema files directly. New stage 1 files starts from 1-10 and stage 2 starts from 2-26.
These migrations takes a couple minutes, mosty because the instant rewards are moved from the `reward` table to a
new `instant_reward` table. After that there is a ledger replay (read below), which takes a few hours.

Upgrading from 13.0.x to 13.2.0.x should be possible but hasn't been tested.

### Ledger replay

Release 13.2.0.1 drops the ledger snaphot serialisation compatibility. This means it's not able to
parse older ledger snapshots. DBSync will delete any existing snapshot and will replay the ledger
rules from genesis. This doesn't mean a rollback to genesis. No db data are deleted.

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
