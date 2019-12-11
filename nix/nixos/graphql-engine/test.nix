let
  sources = import ../../sources.nix;
  nixpkgsHasuraBaseSrc = sources.nixpkgs-hasura-base;
  nixpkgsHasuraAttr = import nixpkgsHasuraBaseSrc {};
  graphqlEngineAttr = nixpkgsHasuraAttr.pkgs.callPackage ./. {};
  graphqlEngine = graphqlEngineAttr.graphql-engine;
in graphqlEngine
