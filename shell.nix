{ lib
, haskellPackages
, mkShell
, zsh
}:

let
  name = "haskell-env";

  ghcWithEnv = haskellPackages.ghcWithPackages (ps:

    lib.pipe (import ./.) [
      lib.functionArgs
      builtins.attrNames
      (lib.subtractLists [ "base" "mkDerivation" ])
      (lib.intersectLists (builtins.attrNames ps))
      (map (n: ps.${n}))
    ]
    ++ builtins.attrValues {
      inherit
        (ps)
        cabal-install
        haskell-language-server
        hpack
        implicit-hie
        ormolu
        ;
    });

in
mkShell {
  inherit name;

  packages = [
    ghcWithEnv
  ];

  shellHook = ''
    export NIX_SHELL_NAME="${name}"
    eval $(egrep ^export ${ghc}/bin/ghc)
    ${zsh}/bin/zsh
    exit 0
  '';
}
