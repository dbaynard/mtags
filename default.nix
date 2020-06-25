{ nixpkgs ?
  import
    ( ({ owner, repo, rev }: builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    }) (import ./pinned-nixpkgs.nix)) { }
, compiler ? "default"
, root ? ./mtags
}:
let
  hPkgs = nixpkgs.pkgs.haskellPackages;
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ".git" ];
  # accepts a path as input
  # it will traverse the directory tree starting at `root`
  # the result is an attribute set, where the values are paths
  # of directories directly containing cabal files.
  # the keys are the names of the packages.
  # hidden folders and blacklisted folders will be skipped
  findHaskellPackages = root:
    let
      items = builtins.readDir root;
      fn = file: type:
        if type == "directory" && isNull (builtins.match "\\..*" file) && !(builtins.elem file [ "dist" "dist-newstyle" ]) then (findHaskellPackages (root + (/. + file)))
        else (
          if type == "regular" then (
            let m = (builtins.match "(.*)\\.cabal" file); in if !(isNull m) then { "${builtins.elemAt m 0}" = root; } else { }
          )
          else { }
        );
    in
    builtins.foldl' (x: y: x // y) { } (builtins.attrValues (builtins.mapAttrs fn items));
  develop = hPkgs.developPackage {
    inherit root;
    modifier = drv:
      if nixpkgs.pkgs.lib.inNixShell then nixpkgs.haskell.lib.addBuildTools drv (builtins.attrValues { inherit (hPkgs) cabal-install ghcid; }) else drv;
    overrides = self: super:
      let inherit (import ./extra-deps.nix) library test executable; in
      builtins.mapAttrs (name: version: nixpkgs.pkgs.haskell.lib.dontCheck (super.callHackage name version { })) (library // test // executable)
      // builtins.mapAttrs (name: path: super.callCabal2nix name (gitignore path) { }) (findHaskellPackages ./.);
  };
in
if nixpkgs.pkgs.lib.inNixShell then develop else nixpkgs.pkgs.haskell.lib.dontCheck develop
