# Auto-running hlint and stylish-haskell on Commit

This document describes how to have `hlint` and `stylish-haskell` locally on every commit.

# Installing Them

Installing these two is as simple as `cabal install hlint stylish-haskell` which will install them
in `$HOME/.cabal/bin` which should then be added to your `PATH` environment variable.

# Install the Git Pre-commit Hook

In the `db-sync` repo, there is a shell script at `scripts/git-pre-commit-hook`. This can be
symlinked to run as a Git pre-commit hook. If you go to the `.git` directory of your
`cardano-db-sync` Git checkout and create the symlink as follows:
```
(cd .git/hooks/ && ln -s ../../scripts/git-pre-commit-hook pre-commit)
```
