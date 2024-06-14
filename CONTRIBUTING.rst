*******************************
Contributing to Cardano DB Sync
*******************************

.. contents:: Table of Contents

Development Tools
=================

Nix
---

Cardano DB Sync development is primarily based on Nix in order to provision the complete
toolchain needed to build all repository artifacts.

For instructions on how to install and configure Nix, please refer to *Configure Nix* in
`Installing With Nix <installing-with-nix_>`_.

Once Nix is installed and configured, enter a development shell by running::

  nix develop .

Developing Without Nix
----------------------

It is possible to build and run DB Sync without Nix, but

* We rely on forked or new versions of system libraries
* Building will use system GHC and libraries

For instruction on how to set up a development environment without Nix, please refer to
*Install Dependencies* in `Installing from Source <installing_>`_.

Building
--------

In development shell, DB Sync can be built by running::

  cabal build all

Testing
-------

We have to classes of test-suites, unit/property tests and database integration tests. To
run the unit/property tests::

  cabal test cardano-db-sync
  cabal test cardano-db:test:test

To run the database integration tests::

  cabal test cardano-chain-gen:test:cardano-chain-gen
  cabal test cardano-db:test:test-db

To run a subset of tests::

  cabal run cardano-chain-gen:test:cardano-chaingen -- --pattern "Babbage unit tests.consumed-tx-out"

Profiling
---------

Profiled builds are only available with Nix::

  nix build .#profiled.cardano-db-sync
  nix build .#profiled.cardano-smash-server

Once this is built, run the executable with profiling::

  ./result/cardano-db-sync <args> +RTS -p -h -RTS

haskell-language-server
-----------------------

The recommended way to use haskell-language-server is with `direnv`_. The nix
development shell provides the haskell-language-server binary for the correct version of
GHC. direnv allows your editor to use the nix shell.

To enable direnv, run::

   echo "use flake" > .envrc
   direnv allow

Then use the appropriate editor extension:

* Visual Studio Code: `Nix VS Code Extension Pack <nix-extension-pack_>`_
* Vim: `direnv.vim`_
* Emacs: `direnv-mode`_

Hoogle Server
-------------

To start a local Hoogle with all dependencies, run::

   nix develop .#ghc96 --command hoogle server --local --port 8000

Conventions
===========

Formatting
----------

We use `Fourmolu <fourmolu_>_`, version 0.10.1.0, for Haskell code formatting. The
executable is provided by the nix development shell. To apply the formatting rules to all
modifyied files, run::

  ./script/fourmolize.sh

To run the checks via nix, run::

  nix build .#checks.<cpu_arch>.fourmolu

Linting
-------

We use `HLint <hlint_>` for Haskell static code analysis, and `Shellcheck <shellcheck_>`_
for shell scripts. To run the checks via nix, run::

  nix build .#checks.<cpu_arch>.hlint
  nix build .#checks.<cpu_arch>.shellcheck

Communication
=============

You can discuss development or find help at the following places:

* Intersect Discord `#db-sync <discord_>`_ channel, if new to server invite `here <discord-invite_>`
* `GitHub Issues <issues_>`_

Roles and Responsibilities
==========================

Regular contributors, all of whom can review and merge PRs are:

* @kderme
* @Cmdv
* @sgillespie

In addition, the `CODEOWNERS file <CODEOWNERS_>`_ provides information on who should
review a contributing PR.

Updating Dependencies
=====================

From Hackage
------------

Updating package dependencies from Hackage should work like normal in a Haskell
project.  The most important thing to note is that we pin the ``index-state`` of
the Hackage package index in ``cabal.project``.  This means that cabal will
always see Hackage “as if” it was that time, ensuring reproducibility.  But it
also means that if you need a package version that was released *after* that
time, you need to bump the ``index-state`` (and to run ``cabal update``
locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you
will also need to pull in the Nix equivalent of the newer ``index-state``.  You
can do this by running ``nix flake lock --update-input hackageNix``.

From the Cardano Haskell Package repository
-------------------------------------------

Many Cardano packages are not on Hackage and are instead in the `Cardano Haskell
Package repository <cardano-haskell-packages_>`_ (CHaP), see the README for
(lots) more information.  Getting new packages from there works much like
getting them from Hackage.  The differences are that it has an independent
``index-state``, and that there is a different Nix command you need to run
afterwards: ``nix flake lock --update-input CHaP``.

Using unreleased versions of dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes we need to use an unreleased version of one of our dependencies,
either to fix an issue in a package that is not under our control, or to
experiment with a pre-release version of one of our own packages.  You can use a
``source-repository-package`` stanza to pull in the unreleased version.  Try
only to do this for a short time, as it does not play very well with tooling,
and will interfere with the ability to release cardano-db-sync itself.

For packages that we do not control, we can end up in a situation where we have
a fork that looks like it will be long-lived or permanent (e.g. the maintainer
is unresponsive, or the change has been merged but not released).  In that case,
release a patched version to the `Cardano Haskell Package repository
<cardano-haskell-packages_>`_, which allows us to remove the
``source-repository-package`` stanza.  See the README for instructions.

Releasing a New Version
=======================

See `<doc/release-process.md>`__

.. _installing-with-nix: doc/installing-with-nix.md
.. _installing: doc/installing.md#install-dependencies
.. _direnv: https://direnv.net/
.. _direnv-mode: https://github.com/wbolster/emacs-direnv
.. _direnv.vim: https://github.com/direnv/direnv.vim
.. _nix-extension-pack: https://marketplace.visualstudio.com/items?itemName=pinage404.nix-extension-pack
.. _fourmolu: https://github.com/fourmolu/fourmolu
.. _discord: https://discord.com/channels/1136727663583698984/1239888910537064468
.. _discord-invite: https://discord.gg/3DYwebJv
.. _issues: https://github.com/IntersectMBO/cardano-db-sync/issues
.. _CODEOWNERS: https://github.com/IntersectMBO/cardano-db-sync/blob/master/CODEOWNERS
.. _cardano-haskell-packages: https://github.com/IntersectMBO/cardano-haskell-packages
