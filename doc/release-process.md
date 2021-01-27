# Release Process

The rationale for this specific release process is:

* Releases are **always** done from release branches and never from `master` (it is theoretcically
  possible in some instances to tag from `master` but tagging release branches is always
  possible, valid and correct).
* All non-release specific fixes should be fixed on `master` first and then rebased onto the
  release branch.
* Release tags should always go on the release branch and the tag should go on the commit that
  includes the change logs, cabal version and the docker compose changes.

The process:

* Make a branch named eg `release/8.0.x`.
* Update version numbers in all cabal files.
* Write the change log entries.
* Update the `docker-compose.yml` file to point at the new release and specify the node version.
* QA the release.
* If any fixes are required, get them merged to `master` first and then rebase the fix onto the
  release branch so they sit below the change log and cabal update commit.
* If QA and everyone else is happy, tag the release and do a formal Github release.
* Cherry pick the changes that update the cabal version numbers, change log entries, docker config
  etc back onto `master`.
