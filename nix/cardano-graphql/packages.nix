{ mkYarnPackage, stdenv, lib, python, nodejs, cardano-graphql-src }:

{
  cardano-graphql = mkYarnPackage {
    pname = "cardano-graphql";
    name = "cardano-graphql";
    version = "0.4.0";
    packageJSON = cardano-graphql-src + "/package.json";
    yarnLock = cardano-graphql-src + "/yarn.lock";
    src = cardano-graphql-src;
    yarnPreBuild = ''
      mkdir -p $HOME/.node-gyp/${nodejs.version}
      echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
      ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
    '';
    pkgConfig = {
      node-sass = {
        buildInputs = [ python ];
        postInstall = ''
          yarn --offline run build
        '';
      };
    };

    installPhase = ''
      unpackPhase
      cd $sourceRoot

      export PATH="$PATH:$node_modules/.bin"

      yarn run build

      cp -r ../deps/cardano-graphql/dist $out

      mkdir -p $out/bin
      cat <<EOF > $out/bin/cardano-graphql
      #!${stdenv.shell}
      exec ${nodejs}/bin/node $out/index.js
      EOF
      chmod +x $out/bin/cardano-graphql
      ln -s $node_modules $out/node_modules
    '';

    distPhase = ''
      cp -r . $out
    '';
  };
}
