# Release Process

The rationale for this specific release process is:

* Releases are **always** done from release branches and never from `master` (it is theoretcically
  possible in some instances to tag from `master` but tagging release branches is always
  possible, valid and correct).
* All non-release specific fixes should be fixed on `master` first and then cherry picked onto the
  release branch.
* Release tags should always go on the release branch and the tag should go on the commit that
  includes the change logs, cabal version and the docker compose changes.

The process:

* Make sure the latest release branch has had its changes cherry picked to `master`.
* Devs make a branch named eg `release/12.0.x`.
* Update version numbers in all cabal files.
* Write the change log entries.
* Update the `docker-compose.yml` file to point at the new release and specify the node version.
* Run `cabal run gen-schema-docs > doc/schema.md` to update the schema documentation.
* Devs then do a full sync of mainnet to make sure everything is still fine.
* Dev then does a pre-release tag named something like `12.0.1-preX`.
* QA tests the pre-release tag.
* If any fixes are required, they get merged to `master` first and then cherry picked onto the
  release branch so they sit below the change log and cabal update commit and then do a new
  `12.0.1-preY` tag which QA then tests.
* If QA and everyone else is happy, the Release Manager tags the same commit which had the
  pre-release tag and then does a formal Github release.
* Finally, some time after the release, the devs will cherry pick the changes that update the cabal
  version numbers, change log entries, docker config etc back onto `master`.
