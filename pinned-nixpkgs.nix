{ pkgs ? import <nixpkgs> { } }:
pkgs.fetchFromGitHub {
  owner = "nixos";
  repo = "nixpkgs";
  name = "nixpkgs-unstable-2020-06-03";
  rev = "00df2371122aeb96b4ca80451b1ac7b9b7b8d847";
  sha256 = "0k35kh30gjqvr6j9j395drmrxnl7knkc9nsizwsclggh7r7ml50w";
}
