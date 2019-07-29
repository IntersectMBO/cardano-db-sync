{ iohkSkeletonPackages ? import ./.. {}
, pkgs ? iohkSkeletonPackages.pkgs
}:
with pkgs;

{
  example = stdenv.mkDerivation {
    name = "iohk-skeleton-docs";
    buildInputs = [ (texlive.combine {
                      inherit (texlive)
                        scheme-small

                        # TODO: replace with your own LaTeX package dependencies

                        # libraries
                        stmaryrd lm-math amsmath extarrows cleveref semantic xcolor appendix

                        # bclogo and dependencies
                        bclogo mdframed xkeyval etoolbox needspace pgf

                        # font libraries `mathpazo` seems to depend on palatino
                        # , but it isn't pulled.
                        mathpazo palatino microtype

                        # libraries for marginal notes
                        xargs todonotes

                        # build tools
                        latexmk
                        ;
                    })
                  ];
    src = ./.;
    buildPhase = "make";

    meta = with lib; {
      description = "IOHK Skeleton Example Document";
      license = licenses.asl20;
      platforms = platforms.linux;
    };
  };

  # This lets Hydra find build jobs in nested attrsets.
  recurseForDerivations = true;
}
