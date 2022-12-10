{
  description = "Markdown tags, i.e. mtags";

  inputs = {
    dbaynard.url = "github:dbaynard/flakes";
    dbaynard.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, dbaynard, self, ... }:
    let
      inherit (dbaynard) lib;

    in
    {
      overlays.default = final: prev: {
        default = prev.haskellPackages.callPackage ./. { };
      };
    }
    // lib.foldFor [ "aarch64-darwin" "x86_64-linux" "aarch64-linux" ]
      (system: {
        packages.${system} = self.overlays.default
          self.packages.${system}
          nixpkgs.legacyPackages.${system};

        devShells.${system}.default =
          nixpkgs.legacyPackages.${system}.callPackage ./shell.nix {
            inherit (self.packages.${system}) default;
          };
      });
}
