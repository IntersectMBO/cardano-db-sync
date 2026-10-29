{ runCommand, python3 }:
let
  python = python3.withPackages (ps: [ ps.bech32 ]);
in runCommand "smash-startup-test" {} ''
  export PYTHONDONTWRITEBYTECODE=1
  ${python}/bin/python ${./smash-startup-test.py} ${../smash-startup.py}
  touch $out
''
