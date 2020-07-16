
# Testing

## Unable to connect to Unix domain socket /var/run/postgresql/.s.PGSQL.5432

When running on MacOS, you get the following error:

```text
      Exception: libpq: failed (could not connect to server: No such file or directory
      	Is the server running locally and accepting
      	connections on Unix domain socket "/var/run/postgresql/.s.PGSQL.5432"?
      )
```

This may indiciate that you do not have `postgres` installed, or that the `postgres`
daemon is not running.

You can install `postgres` using Homebrew using:

```bash
brew install postgres
```

And start the `postgres` daemon using:

```bash
brew services start postgresql
```

And check that it is running using:

```bash
brew services
Name       Status  User Plist
postgresql started jky  /Users/jky/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
```

If the daemon is running and the problem persists, then it could be because `postgres`
is configured to create its unix domain socket on a path different to that expected by
`cardano-db-sync`.

Assuming the daemon is running, the actual path to the unix domain socket can
be discovered by like this:

```bash
$ lsof -p "$(ps -ef | grep postgres | grep '[b]in/postgres' | xargs | cut -d ' ' -f 2)" | grep 5432
postgres 9050  jky    7u   unix 0xb7eadc9d471eb839      0t0                     /tmp/.s.PGSQL.5432
```

We can work around the problem by sym-linking the expected path to the actual path:

```bash
sudo ln -s /tmp/.s.PGSQL.5432 /var/run/postgresql/.s.PGSQL.5432
```
