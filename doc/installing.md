# Installing from Source (without Nix)

## Prerequisites

This guide assumes you have the following tools:

 * [Git](https://git-scm.com/download)
 * [Cardano Node](https://github.com/IntersectMBO/cardano-node/blob/master/doc/getting-started/install.md)
 * [Postgres Development Libraries (libpq)](https://www.postgresql.org/download/)
 * [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)

In addition, Cardano DB Sync requires the following software (instructions below):

 * [GHC](https://www.haskell.org/ghcup/install/) >= 8.10.7
 * [Cabal](https://www.haskell.org/ghcup/install/) >= 3.10.1.0
 * [libsodium-vrf](https://github.com/IntersectMBO/libsodium)
 * [secp256k1](https://github.com/bitcoin-core/secp256k1)
 * [blst](https://github.com/supranational/blst)

 macOS:

* [x-code](https://developer.apple.com/xcode/resources/)

Create a working directory for your builds:

```bash
mkdir -p ~/src
cd ~/src
```

## Install Dependencies

### Install GHC and Cabal

The recommended way to install the Haskell tools is via
[GHCup](https://www.haskell.org/ghcup/). Check [this
page](https://www.haskell.org/ghcup/install/) for installation instructions. Its
installation script will guide you through the installation, and warn you about package
dependencies.

Once GHCup is installed, open a new terminal (to get an updated environment) and run:

```bash
ghcup install ghc 8.10.7
ghcup install cabal 3.10.1.0
ghcup set ghc 8.10.7
ghcup set cabal 3.10.1.0
```

Check that you will use the GHCup tools (and not any other installation on the system):

```bash
which cabal
```

It should print a path of this shape: `/home/<user>/.ghcup/bin/cabal`.

### Install Sodium

Cardano uses a [fork](https://github.com/IntersectMBO/libsodium) of `libsodium` which
exposes some internal functions and adds some other new functions.

Enter a working directory for your builds:

```bash
cd ~/src
```

Take note of the latest revision from [iohk-nix](https://github.com/input-output-hk/iohk-nix):

```bash
REV=$(curl -L https://github.com/input-output-hk/iohk-nix/releases/latest/download/INFO \
  | awk '$1 == "debian.libsodium-vrf.deb" { rev = gensub(/.*-(.*)\.deb/, "\\1", "g", $2); print rev }')
```

Checkout libsodium:

```bash
git clone https://github.com/IntersectMBO/libsodium
cd libsodium
git checkout $REV
```

Build and install it:
```bash
./autogen.sh
./configure
make
make check
sudo make install
```

Add the following to your `~/.bashrc` file and source it (or re-open the terminal):

```bash
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

On Linux, running the `ldconfig` command is required to make the dynamic linker aware of
the new library.

### Installing Secp256k1

Enter the working directory for your builds:

```bash
cd ~/src
```

Take note of the latest revision from [iohk-nix](https://github.com/input-output-hk/iohk-nix):

```bash
REV=$(curl -L https://github.com/input-output-hk/iohk-nix/releases/latest/download/INFO \
  | awk '$1 == "debian.libsecp256k1.deb" { rev = gensub(/.*-(.*)\.deb/, "\\1", "g", $2); print rev }')
```

Checkout libsecp256k1:

```bash
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout $REV
```

Build and install it:

```bash
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
```

### Installing blst

Enter the working directory for your builds:

```bash
cd ~/src
```

Take note of the latest revision from [iohk-nix](https://github.com/input-output-hk/iohk-nix):

```bash
REV=$(curl -L https://github.com/input-output-hk/iohk-nix/releases/latest/download/INFO \
  | awk '$1 == "debian.libblst.deb" { rev = gensub(/.*-(.*)\.deb/, "\\1", "g", $2); print rev }')
```

Checkout blst:

```bash
git clone https://github.com/supranational/blst
cd blst
git checkout $REV
```

Build it:

```bash
./build.sh
```
Install it:

```bash
cat > libblst.pc << EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF

sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod 644 \
  /usr/local/lib/libblst.* \
  /usr/local/include/{blst.*,blst_aux.h}
```

#### MACOS:

If you hit the following error on macOS:
```bash
+ cc -O2 -fno-builtin -fPIC -Wall -Wextra -Werror -c ./src/server.c
In file included from ./src/server.c:7:
In file included from ./src/keygen.c:7:
In file included from ./src/consts.h:8:
./src/vect.h:402:11: fatal error: 'stdlib.h' file not found
# include <stdlib.h>
```

check you have x-code command line tools installed or try install them by running:
```bash
xcode-select --install
```

If you are using homebrew you might need to unistall `llvm@13` as this clashes with x-code:
```bash
brew uninstall llvm@13
```

## Install Cardano DB Sync

### Download the Source

Enter the working directory for your builds:

```bash
cd ~/src
```

Find the latest release here: https://github.com/IntersectMBO/cardano-db-sync/releases

Check out the latest release version:

```bash
git clone https://github.com/IntersectMBO/cardano-db-sync.git
cd cardano-db-sync
git fetch --all --tags
git checkout tags/<VERSION>
```

### Configure Build Options

Explicitly set the GHC version that we installed earlier. This avoids defaulting to a
system version of GHC that might be different than the one you have installed.

```bash
echo "with-compiler: ghc-8.10.7" >> cabal.project.local
```

macOS installs OpenSSL in a different location than expected by default. If you have
installed OpenSSL via homebrew then run:

```
sudo mkdir -p /usr/local/opt/openssl
sudo ln -s /opt/homebrew/opt/openssl@3/lib /usr/local/opt/openssl/lib
sudo ln -s /opt/homebrew/opt/openssl@3/include /usr/local/opt/openssl/include
```

If you are on an M1 Mac, run:

```bash
echo "package HsOpenSSL" >> cabal.project.local
echo "  flags: -homebrew-openssl" >> cabal.project.local
echo "" >> cabal.project.local
```

### Build and Install

Build Cardano DB Sync with `cabal`:

```bash
cabal update
cabal build all
```

Install it in the `~/.local/bin` directory:

```bash
mkdir -p ~/.local/bin
cp -p \
  "$(find . -name cardano-db-sync -executable -type f)" \
  ~/.local/bin/
```

**Note:** `~/.local/bin` should be in the `$PATH`.

Note, we avoid using `cabal install` because that method prevents the installed binaries from reporting
the git revision with the `--version` switch.

Check the version that has been installed:

```bash
cardano-db-sync --version
```
