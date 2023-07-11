# Installing with Nix

[Nix](https://nixos.org/download.html) is a purely functional package manager that creates
reproducible, declarative and reliable systems. This is the only dependency required to
build Cardano DB Sync.

## Prerequisites

This guide assumes you have the following tools:

 * [Nix](https://nixos.org/download.html)
 * [Cardano Node](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/building-the-node-using-nix.md)
 
Nix will handle all other dependencies.

Create a working directory for your builds:

```bash
mkdir -p ~/src
cd ~/src
```

## Configure Nix

Enable [Flakes](https://nixos.wiki/wiki/Flakes) (and IFD support):

```bash
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
EOF
```

Check [this page](https://nixos.wiki/wiki/Flakes#Enable_flakes) for further instructions.

## Add the Binary Cache

```bash
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

## Download the Source

Enter the working directory for your builds:

```bash
cd ~/src
```

Find the latest release here: https://github.com/input-output-hk/cardano-db-sync/releases

Check out the latest release version:

```bash
git clone https://github.com/input-output-hk/cardano-db-sync.git
cd cardano-db-sync
git fetch --all --tags
git checkout tags/<VERSION>
```

## Build and Install

Build Cardano DB Sync with `nix`:

```bash
nix build .
```

This will build the executable and link it in `./result`.

Install it in your nix proile:

```bash
nix profile install .
```

Check the version that has been installed:

```bash
cardano-db-sync --version
```
