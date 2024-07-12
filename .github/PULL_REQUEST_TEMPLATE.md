# Description

Add your description here, if it fixes a particular issue please provide a [link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=) to the issue.

# Checklist

- [ ] Commit sequence broadly makes sense
- [ ] Commits have useful messages
- [ ] New tests are added if needed and existing tests are updated
- [ ] Any changes are noted in the [changelog](https://github.com/IntersectMBO/cardano-db-sync/blob/master/db-sync/CHANGELOG.md)
- [ ] Code is formatted with [`fourmolu`](https://github.com/fourmolu/fourmolu) on version 0.10.1.0 (which can be run with `scripts/fourmolize.sh`)
- [ ] Self-reviewed the diff

# Migrations

- [ ] The pr causes a [breaking change](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/migrations.md) of type a,b or c
- [ ] If there is a breaking change, the pr includes a database migration and/or a fix process for old values, so that upgrade is possible
- [ ] Resyncing and running the migrations provided will result in the same database semantically

If there is a breaking change, especially a big one, please add a justification here. Please elaborate
more what the migration achieves, what it cannot achieve or why a migration is not possible.
