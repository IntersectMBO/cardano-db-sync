***********************************************
Contributing to the ``cardano-db-sync`` project
***********************************************

The ``cardano-db-sync`` development is primarily based on the Nix infrastructure (https://nixos.org/ ), which enables packaging, CI, development environments and deployments.

On how to set up Nix for ``cardano-db-sync`` development, please see `Building Cardano Node with nix <https://github.com/input-output-hk/cardano-node/tree/master/doc/getting-started/building-the-node-using-nix.md>`_.

Our formater of choice is `fourmolu <https://github.com/fourmolu/fourmolu>`_ on version 0.10.1.0 We have a script `scripts/fourmolize.sh` that can be run once fourmolu is intalled locally.

Roles and Responsibilities
==========================

We maintain a `CODEOWNERS file <CODEOWNERS>`_ which provides information who
should review a contributing PR.  Note that you might need to get approvals
from all code owners (even though GitHub doesn't give a way to enforce it).

.. _CODEOWNERS https://github.com/input-output-hk/cardano-db-sync/blob/master/CODEOWNERS
