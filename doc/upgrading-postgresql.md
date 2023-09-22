# Upgrading PostgreSQL

For major releases of PostgreSQL, the internal storage format is typically changed,
complicating upgrades.

## Requirements

 * 350GiB free space
 
This guide uses examples for Debian, but should apply to any Linux distribution.

## Install a New PostgreSQL Version

If you're using Debian (or Ubuntu), make sure you've enabled the [PostgreSQL APT
repository](https://wiki.postgresql.org/wiki/Apt), which will enable you to install
multiple versions.

Install a new PostgreSQL version alongside the existing:

```bash
sudo apt install postgresql-15
```

## Upgrade the Schemas

Stop Cardano DB Sync PostgreSQL

```bash
sudo systemctl stop postgresql
sudo pkill -f cardano-db-sync
```

Upgrade the existing schemas

```bash
cd /tmp
sudo -u postgres /usr/lib/postgresql/15/bin/pg_upgrade \
  --old-bindir=/usr/lib/postgresql/11/bin \
  --new-bindir=/usr/lib/postgresql/15/bin \
  --old-datadir=/var/lib/postgresql/11/main \
  --new-datadir=/var/lib/postgresql/15/main \
  --old-options="--config_file=/etc/postgresql/11/main/postgresql.conf" \
  --new-options="--config_file=/etc/postgresql/14/main/postgresql.conf"
```

Start PostgreSQL

```bash
sudo systemctl start postgresql
```

## Clean Up

Remove the old PostgreSQL version

```bash
sudo -u postgres ./delete-old-cluster.sh
sudo apt remove postgresql
```
