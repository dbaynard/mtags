{ nixpkgs ?
  import
    ( ({ owner, repo, rev }: builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    }) (import ../pinned-nixpkgs.nix)) { }
}:
let mtags = nixpkgs.pkgs.haskell.lib.justStaticExecutables (
  import ../default.nix { }
); in
nixpkgs.dockerTools.buildImage {
  name = "mtags-build";
  tag = "latest";
  contents = [ mtags ] ++ builtins.attrValues { inherit (nixpkgs) cacert coreutils bash; };
  config = {
    Cmd = [ "${mtags}/bin/mtags" ];
    ExposedPorts = {
      "3000/tcp" = { };
    };
  };
}
