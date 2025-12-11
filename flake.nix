{
  description = "cardano-db-sync";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
    in
      inputs.utils.lib.eachSystem supportedSystems (system:
        let
          nixpkgs = import inputs.nixpkgs {
            inherit system;
            inherit (inputs.haskellNix) config;

            overlays =
              builtins.attrValues inputs.iohkNix.overlays ++
              [ inputs.haskellNix.overlay

                (final: prev: {
                  haskell-nix = prev.haskell-nix // {
                    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
                      "libpq" = [ "libpq" ];
                    };
                  };

                  haskellBuildUtils = prev.haskellBuildUtils.override {
                    compiler-nix-name = "ghc96";
                    index-state = "2025-06-25T13:51:34Z";
                  };
                })

                (final: prev: {
                  inherit (project.hsPkgs.cardano-node.components.exes) cardano-node;
                  inherit (project.hsPkgs.cardano-cli.components.exes) cardano-cli;
                })

                (final: prev:
                  let
                    profiled = project.profiled.exes;
                  in {
                    # The cardano-db-sync NixOS module (nix/nixos/cardano-db-sync-service.nix)
                    # expects these to be here
                    inherit (project.exes)
                      cardano-db-sync
                      cardano-db-tool
                      cardano-smash-server;

                    cardano-db-sync-profiled = profiled.cardano-db-sync;
                    cardano-smash-server-profiled = profiled.cardano-smash-server;

                    schema = ./schema;
                  })

                (final: prev: {
                  hlint = final.haskell-nix.tool "ghc96" "hlint" {
                    version = "3.8";
                  };

                  fourmolu = final.haskell-nix.tool "ghc96" "fourmolu" {
                    version = "0.17.0.0";
                  };
                })

                (final: prev: {
                  libpq =
                    final.lib.pipe prev.libpq [
                      (p: p.override {
                        gssSupport = false;
                      })

                      (p: p.overrideAttrs (old:
                        final.lib.optionalAttrs (final.stdenv.hostPlatform.isMusl) {
                          dontDisableStatic = true;
                          NIX_LDFLAGS = "--push-state --as-needed -lstdc++ --pop-state";
                          # without this collate.icu.utf8, and foreign_data will fail.
                          LC_CTYPE = "C";
                          # libpq from nixpkgs will either remove static or dynamic 
                          # libs, but we need to keep them both
                          postInstall = "";
                        }))
                    ];
                })
              ];
          };

          # Set up and start Postgres before running database tests
          preCheck = ''
            echo pre-check
            initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=postgres $NIX_BUILD_TOP/db-dir
            postgres -D $NIX_BUILD_TOP/db-dir -c listen_addresses="" -k $TMP &
            PSQL_PID=$!
            echo $PSQL_PID > postgres.pid
            sleep 10
            if (echo '\q' | psql -h $TMP postgres postgres); then
              echo "PostgreSQL server is verified to be started."
            else
              echo "Failed to connect to local PostgreSQL server."
              exit 2
            fi
            ls -ltrh $NIX_BUILD_TOP
            DBUSER=$(whoami)
            DBNAME=$DBUSER
            export PGPASSFILE=$NIX_BUILD_TOP/pgpass
            echo "$TMP:5432:$DBUSER:$DBUSER:*" > $PGPASSFILE
            cp -vir ${./schema} ../schema
            chmod 600 $PGPASSFILE
            psql -h $TMP postgres postgres <<EOF
              create role $DBUSER with createdb login password '$DBPASS';
              alter user $DBUSER with superuser;
              create database $DBNAME with owner = $DBUSER;
              \\connect $DBNAME
              ALTER SCHEMA public   OWNER TO $DBUSER;
            EOF
          '';

          # Clean up Postgres after running database tests
          postCheck = ''
            echo post-check
            DBNAME=$(whoami)
            NAME=db_schema.sql
            mkdir -p $out/nix-support
            echo "Dumping schema to db_schema.sql"
            pg_dump -h $TMP -s $DBNAME > $out/$NAME
            kill $(cat postgres.pid)
            echo "Adding to build products..."
            echo "file binary-dist $out/$NAME" > $out/nix-support/hydra-build-products
          '';

          isCross = pkgs: 
            with pkgs.haskell-nix.haskellLib; isNativeMusl || isCrossHost;

          project = (nixpkgs.haskell-nix.cabalProject' ({ config, lib, pkgs, ... }: rec {
            src = ./.;
            name = "cardano-db-sync";
            compiler-nix-name = lib.mkDefault "ghc96";
            flake.variants =
              let
                extraCompilers = 
                  # To avoid excessive jobs, only build for default compiler on macOS
                  lib.optionals (system == "x86_64-linux")
                    [ "ghc910" "ghc912" ];
              in
                lib.genAttrs extraCompilers (c: { compiler-nix-name = c; });

            # cardano-cli is needed when building the docker image
            cabalProjectLocal = ''
              extra-packages: cardano-cli
            '';

            crossPlatforms = p:
              lib.optional (system == "x86_64-linux") p.musl64;

            inputMap = {
              "https://chap.intersectmbo.org/" = inputs.CHaP;
            };

            shell = {
              tools = {
                cabal = "3.14.2.0";

                haskell-language-server = {
                  src = nixpkgs.haskell-nix.sources."hls-2.11";
                };
              } // lib.optionalAttrs (config.compiler-nix-name == "ghc967") {
                # These versions work with GHC 9.6, but not with 9.10 and 9.12
                fourmolu = "0.17.0.0";
                hlint = "3.8";

                weeder = "latest";
              };

              # Now we use pkgsBuildBuild, to make sure that even in the cross
              # compilation setting, we don't run into issues where we pick tools
              # for the target.
              buildInputs = with nixpkgs.pkgsBuildBuild; [
                git
              ];

              withHoogle = true;

              crossPlatforms = _: [];
            };

            modules = [
              ({ lib, pkgs, ... }: {
                package-keys = [ "ekg" ];
                # Ignore version bounds
                packages.katip.doExactConfig = true;
                # Split data to reduce closure size
                packages.ekg.components.library.enableSeparateDataOutput = true;
              })

              ({
                # Override extra-source-files
                packages.cardano-db-sync.package.extraSrcFiles =
                  [ "../schema/*.sql" ];
                packages.cardano-db.package.extraSrcFiles =
                  ["../config/pgpass-testnet"];
                packages.cardano-db.components.tests.schema-rollback.extraSrcFiles =
                  [ "src/Cardano/Db/Schema/BaseSchema.hs" "src/Cardano/Db/Operations/Delete.hs" ];
                packages.cardano-db.components.tests.test-db.extraSrcFiles =
                  [ "../config/pgpass-mainnet" ];
                packages.cardano-chain-gen.package.extraSrcFiles =
                  [ "../schema/*.sql" ];
              })

              ({ lib, pkgs, config, ... }: {
                # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
                # to call out to all kinds of silly tools that GHC doesn't really provide.
                # For this reason, we try to get away without re-installing lib:ghc for now.
                reinstallableLibGhc = false;
              })

              (pkgs.lib.mkIf pkgs.hostPlatform.isMusl
                (let
                  ghcOptions = [
                    # libpq static is pretty broken in nixpkgs. We can't rely on the
                    # pkg-config, so we have to add the correct libraries ourselves
                    #
                    "-optl-Wl,-lpgcommon"
                    "-optl-Wl,-lpgport"
                    "-optl-Wl,-lm"

                    # Since we aren't using pkg-config, it won't automatically include
                    # OpenSSL
                    "-L${pkgs.openssl.out}/lib"

                    # The ordering of -lssl and -lcrypto below is important. Otherwise,
                    # we'll get:
                    #
                    #     (.text+0x2c3d): undefined reference to `COMP_get_type'
                    #     (.text+0x2de6): undefined reference to `COMP_get_name'
                    #     (.text+0x4af4): undefined reference to `COMP_CTX_free'
                    #     (.text+0x4bdd): undefined reference to `COMP_CTX_get_method'
                    "-optl-Wl,-lssl"
                    "-optl-Wl,-lcrypto"
                  ];
                in {
                  packages.cardano-chain-gen.components.tests.cardano-chain-gen.ghcOptions = ghcOptions;
                  packages.cardano-db-sync.ghcOptions = ghcOptions;
                  packages.cardano-db.ghcOptions = ghcOptions;
                  packages.cardano-db-tool.ghcOptions = ghcOptions;
                  packages.cardano-smash-server.ghcOptions = ghcOptions;

                  doHoogle = false;
                  doHaddock = false;
                }))

              ({
                packages.double-conversion.ghcOptions = [
                  # stop putting U __gxx_personality_v0 into the library!
                  "-optcxx-fno-rtti"
                  "-optcxx-fno-exceptions"
                  # stop putting U __cxa_guard_release into the library!
                  "-optcxx-std=gnu++98"
                  "-optcxx-fno-threadsafe-statics"
                ];
              })

              (lib.mkIf pkgs.haskell-nix.haskellLib.isCrossHost {
                packages.text-icu.patches = [
                  # Fix the following compilation error when cross-compiling:
                  #
                  # Char.hsc: In function ‘_hsc2hs_test45’:
                  # Char.hsc:1227:20: error: storage size of ‘test_array’ isn’t constant
                  # Char.hsc:1227:20: warning: unused variable ‘test_array’ [8;;https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wunused-variable-Wunused-variable8;;]
                  # compilation failed
                  #
                  # U_NO_NUMERIC_VALUE is defined to be `((double)-123456789.)` which gcc can't
                  # figure out at compile time.
                  #
                  # More context:
                  #
                  # https://github.com/haskell/text-icu/issues/7
                  # https://gitlab.haskell.org/ghc/ghc/-/issues/7983
                  # https://gitlab.haskell.org/ghc/ghc/-/issues/12849
                  (builtins.toFile "text-icu.patch" ''
                    diff --git c/Data/Text/ICU/Char.hsc i/Data/Text/ICU/Char.hsc
                    index 6ea2b39..a0bf995 100644
                    --- c/Data/Text/ICU/Char.hsc
                    +++ i/Data/Text/ICU/Char.hsc
                    @@ -1223,7 +1223,7 @@ digitToInt c
                     -- fractions, negative, or too large to fit in a fixed-width integral type.
                     numericValue :: Char -> Maybe Double
                     numericValue c
                    -    | v == (#const U_NO_NUMERIC_VALUE) = Nothing
                    +    | v == -123456789 = Nothing
                         | otherwise                        = Just v
                         where v = u_getNumericValue . fromIntegral . ord $ c
                  '')
                ];
              })

              ({ lib, pkgs, ... }:
                # Database tests
                let
                  inherit (pkgs.haskell-nix) haskellLib;

                  postgresTest = {
                    # Keep postgresql static builds out of shells
                    build-tools = lib.optional (!isCross pkgs) pkgs.postgresql;
                    inherit preCheck;
                    inherit postCheck;
                  };
                in {
                  packages.cardano-db.components.tests.test-db = postgresTest;
                  packages.cardano-chain-gen.components.tests.cardano-chain-gen =
                    postgresTest;
                })

              ({ lib, pkgs, ... }: with pkgs.haskell-nix.haskellLib;
                # There's no need to run PostgreSQL integration tests on static builds
                lib.mkIf (isCross pkgs) {
                  packages.cardano-chain-gen.components.tests.cardano-chain-gen.doCheck = false;
                  packages.cardano-db.components.tests.test-db.doCheck = false;
                })

              ({ lib, pkgs, config, ... }: lib.mkIf pkgs.hostPlatform.isMacOS {
                # PostgreSQL tests fail in Hydra on MacOS with:
                #
                #     FATAL:  could not create shared memory segment: No space left on device
                #     DETAIL:  Failed system call was shmget(key=639754676, size=56, 03600).
                #     HINT:  This error does *not* mean that you have run out of disk space.
                #       It occurs either if all available shared memory IDs have been taken,
                #       in which case you need to raise the SHMMNI parameter in your kernel,
                #       or because the system's overall limit for shared memory has been reached.
                #
                # So disable them for now
                packages.cardano-chain-gen.components.tests.cardano-chain-gen.doCheck = false;
                packages.cardano-db.components.tests.test-db.doCheck = false;
              })

            ];
          })).appendOverlays [
            # Collect local package `exe`s
            nixpkgs.haskell-nix.haskellLib.projectOverlays.projectComponents

            (final: prev: {
              profiled = final.appendModule {
                modules = [{
                  enableLibraryProfiling = true;
                  enableProfiling = true;
                  packages.cardano-db-sync.configureFlags =
                    ["--ghc-option=-fprof-auto"];
                  packages.cardano-smash-server.configureFlags =
                    ["--ghc-option=-fprof-auto"];
                }];
              };
            })
          ];

          staticChecks =
            let
              inherit (project.args) src compiler-nix-name;
            in
              with nixpkgs; {
                hlint = callPackage hlintCheck { inherit src hlint; };
                fourmolu = callPackage fourmoluCheck { inherit src fourmolu; };
                shellcheck = callPackage shellCheck { inherit src; };
              };

          flake = project.flake {};
        in with nixpkgs; lib.recursiveUpdate flake (
          let
            mkDist = platform: project:
              let
                exes = with lib; pipe project.exes [
                  (collect isDerivation)
                  (map setGitRev)
                ];
                name = "cardano-db-sync-${version}-${platform}";
                version = project.exes.cardano-db-sync.identifier.version;
                env = {
                  nativeBuildInputs = with nixpkgs; [ haskellBuildUtils bintools ];
                };
              in
                nixpkgs.runCommand name env
                  ''
                    mkdir -p $out release/bin
                    cd release

                    # Copy exes to intermediate dir
                    cp \
                      --no-clobber \
                      --no-preserve=mode \
                      --remove-destination \
                      --verbose \
                      ${lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} \
                       ./bin

                    # Copy magrations to intermediate dir
                    cp \
                      --no-clobber \
                      --no-preserve=mode \
                      --remove-destination \
                      --verbose \
                      --recursive \
                      ${./schema} \
                      ./schema

                    # Rewrite libs on macos (from iohk-utils)
                    ${lib.optionalString
                       (platform == "macos")
                       (lib.concatMapStrings
                         (exe: "rewrite-libs . ${exe}/bin/*")
                         exes)}

                    # Package distribution
                    dist_file=${name}.tar.gz
                    tar -czf $out/$dist_file .

                    # Write summary file
                    mkdir $out/nix-support
                    echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
                  '';

            cardano-db-sync-linux = mkDist "linux" project.projectCross.musl64;
            cardano-db-sync-macos = mkDist "macos" project;
            cardano-smash-server-no-basic-auth = (project.appendModule {
              modules = [{packages.cardano-smash-server.flags.disable-basic-auth = true;}];
            }).exes.cardano-smash-server;

            docker = callPackage ./nix/docker.nix {
              inherit (inputs.iohkNix.lib) evalService;
            };
            inherit (docker) cardano-db-sync-docker cardano-smash-server-docker;

            profiled = {
              inherit (project.profiled.exes) cardano-db-sync cardano-smash-server;
            };

            # TODO: macOS builders are resource-constrained and cannot run the detabase
            # integration tests. Add these back when we get beefier builders.
            nonRequiredMacOSPaths = [
              "checks.cardano-chain-gen:test:cardano-chain-gen"
              "checks.cardano-db:test:test-db"
              "ghc96.checks.cardano-chain-gen:test:cardano-chain-gen"
              "ghc96.checks.cardano-db:test:test-db"
              "ghc98.checks.cardano-chain-gen:test:cardano-chain-gen"
              "ghc98.checks.cardano-db:test:test-db"
            ];

            nonRequiredPaths =
              if hostPlatform.isMacOS then
                nonRequiredMacOSPaths
              else [];

            exeSetGitRev = drvKey: drv:
              if nixpkgs.lib.hasInfix ":exe:" drvKey then
                setGitRev drv
              else
                drv;

            setGitRev = nixpkgs.setGitRev (self.rev or "dirty");

          in rec {
            checks = staticChecks;

            devShells = 
              let
                mkVariantShell = compiler: variant: 
                  lib.nameValuePair "profiled-${compiler}" v.shell;
              in 
                # "Profiled" devshell variants for each extra compiler
                lib.mapAttrs' mkVariantShell project.profiled.projectVariants // {
                  # A "profiled" variant shell for the default compiler
                  profiled = project.profiled.shell;
                };

            hydraJobs = callPackages inputs.iohkNix.utils.ciJobsAggregates {
              ciJobs = flake.hydraJobs;
              nonRequiredPaths = map lib.hasPrefix nonRequiredPaths;
            } // lib.optionalAttrs (system == "x86_64-linux") {
              inherit
                cardano-db-sync-linux
                cardano-db-sync-docker
                cardano-smash-server-docker;

              # No need to run static checks on all architectures
              checks = staticChecks;
            } // lib.optionalAttrs (system == "x86_64-darwin") {
              inherit cardano-db-sync-macos;
            } // {
              inherit cardano-smash-server-no-basic-auth profiled;
            };

            legacyPackages = pkgs;

            packages = lib.optionalAttrs (system == "x86_64-linux") {
              inherit
                cardano-db-sync-linux
                cardano-db-sync-docker
                cardano-smash-server-docker
                project;

              default = flake.packages."cardano-db-sync:exe:cardano-db-sync";
            } // lib.optionalAttrs (system == "x86_64-darwin") {
              inherit cardano-db-sync-macos;
            } // {
              inherit cardano-smash-server-no-basic-auth profiled;
            # Run setGitRev on all packages that have ":exe:" in their key
            } // builtins.mapAttrs exeSetGitRev flake.packages;
          })) // {
            nixosModules = {
              cardano-db-sync = { pkgs, lib, ... }: {
                imports = [ ./nix/nixos/cardano-db-sync-service.nix ];
                services.cardano-db-sync.dbSyncPkgs =
                  let
                    pkgs' = self.legacyPackages.${pkgs.system};
                  in {
                    inherit (pkgs')
                      cardanoLib
                      schema
                      cardano-db-sync
                      cardano-db-sync-profiled;

                    # cardano-db-tool
                    cardanoDbSyncHaskellPackages.cardano-db-tool.components.exes.cardano-db-tool =
                      pkgs'.cardano-db-tool;
                };
              };
            };
          };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
    experimental-features = [ "nix-command" "flakes" "fetch-closure" ];
  };
}
