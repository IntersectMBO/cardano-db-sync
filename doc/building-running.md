**Validated: 2023/05/18**

# Building and Running the Cardano DB Sync Node

The cardano-db-sync node is built and tested to run on Linux. It may run on macOS or Windows but
that is unsupported.

Running the db sync node will require Nix and either multiple terminals or a multi terminal
emulator like GNU Screen or TMux.
Setup [IOHK binary cache](https://github.com/IntersectMBO/cardano-node/blob/master/doc/getting-started/building-the-node-using-nix.md#iohk-binary-cache)
to avoid several hours of build time.

The db sync node is designed to work with a locally running Cardano Node. The two git repositories need to be checked out so that
they are both at the same level. eg:

```
> tree -L 1
.
├── cardano-db-sync
├── cardano-node
```
These instuction assume than only blocks and transactions for a single blockchain are inserted in
the db. If you want to to use a single database for more than one chain, you will need to duplicate
`config/pgpass` and provide a unique db name for each chain (ie mainnet uses `cexplorer`).

### Set up and run a local node

Regular users should almost never attempt building and running from the `master` branch. Instead,
they should build and run the latest release tag. The tags can be listed using the `git tag`
command. Pre-release tags (eg things like `12.0.0-preX`) should also be avoided in most cases.
```
git clone https://github.com/IntersectMBO/cardano-node
cd cardano-node
git checkout <latest-official-tag> -b tag-<latest-official-tag>
nix run .#mainnet/node
```

More detailed instructions on GHC, Cabal, libraries and `cardano-node` setup can be found here:
- [Installing Cardano Node from source](https://github.com/IntersectMBO/cardano-node/blob/master/doc/getting-started/install.md)
- [Building Cardano Node with nix](https://github.com/IntersectMBO/cardano-node/blob/master/doc/getting-started/building-the-node-using-nix.md)

### Set up and run the db-sync node

- Install secp256k1 library as a prerequisite for building with cabal:

``` shell
./scripts/secp256k1-setup.sh ac83be33d0956faf6b7f61a60ab524ef7d6a473a
# Check ./github/workflows/haskell.yml to validate the git sha above.

# On Linux
sudo make install

# On macOS
make install

```

- with cabal:

```
git clone https://github.com/IntersectMBO/cardano-db-sync
cd cardano-db-sync
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh --createdb
git checkout <latest-official-tag> -b tag-<latest-official-tag>

cabal build cardano-db-sync
PGPASSFILE=config/pgpass-mainnet cabal run cardano-db-sync -- \
    --config config/mainnet-config.yaml \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --state-dir ledger-state/mainnet \
    --schema-dir schema/
```

to find `cardano-db-sync` executable location use:

```
find . -name cardano-db-sync -executable -type f
./dist-newstyle/build/x86_64-linux/ghc-8.10.4/cardano-db-sync-12.0.0/build/cardano-db-sync/cardano-db-sync
```

On macOS `brew install postgresl openssl@1.1` and extend PKG_CONFIG_PATH with
`PKG_CONFIG_PATH=/usr/local/opt/postgresql/lib/pkgconfig:/usr/local/opt/openssl/lib/pkgconfig cabal build all`
when running cabal build

- with nix:

```
git clone https://github.com/IntersectMBO/cardano-db-sync
cd cardano-db-sync
git checkout <latest-official-tag> -b tag-<latest-official-tag>
nix build -v .#cardano-db-sync -o db-sync-node
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh --createdb
PGPASSFILE=config/pgpass-mainnet db-sync-node/bin/cardano-db-sync \
    --config config/mainnet-config.yaml \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --state-dir ledger-state/mainnet \
    --schema-dir schema/
```

### Network Configuration, Genesis and Topology Files

The latest supported networks can be found at [world book](https://book.world.dev.cardano.org/environments.html)
