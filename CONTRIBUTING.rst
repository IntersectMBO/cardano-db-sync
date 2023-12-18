***********************************************
Contributing to the ``cardano-db-sync`` project
***********************************************

The ``cardano-db-sync`` development is primarily based on the Nix infrastructure
(https://nixos.org/ ), which enables packaging, CI, development environments and
deployments.

On how to set up Nix for ``cardano-db-sync`` development, please see `Building Cardano
Node with nix
<https://github.com/IntersectMBO/cardano-node/tree/master/doc/getting-started/building-the-node-using-nix.md>`_.

Roles and Responsibilities
==========================

We maintain a `CODEOWNERS file <CODEOWNERS_>`_ which provides information who
should review a contributing PR.  Note that you might need to get approvals
from all code owners (even though GitHub doesn't give a way to enforce it).

Supplementary Tooling
=====================

Fourmolu
--------

`Fourmolu <fourmolu_>`_ is our formatter of choice, version 0.10.1.0. We have a
script `scripts/fourmolize.sh` that can be run once fourmolu is installed
locally.

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
---------------------------------------------

Many Cardano packages are not on Hackage and are instead in the `Cardano Haskell
Package repository <cardano-haskell-packages_>`_ (CHaP), see the README for
(lots) more information.  Getting new packages from there works much like
getting them from Hackage.  The differences are that it has an independent
``index-state``, and that there is a different Nix command you need to run
afterwards: ``nix flake lock --update-input CHaP``.

Using unreleased versions of dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

.. _CODEOWNERS: https://github.com/IntersectMBO/cardano-db-sync/blob/master/CODEOWNERS
.. _cardano-haskell-packages: https://github.com/IntersectMBO/cardano-haskell-packages
.. _fourmolu: https://github.com/fourmolu/fourmolu
