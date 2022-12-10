{ mkDerivation, base, cmark, doctest, doctest-driver-gen
, generic-lens, genvalidity-containers, genvalidity-hspec
, genvalidity-text, hspec, lib, optparse-generic, prettyprinter
, raw-strings-qq, rio, validity, validity-containers, validity-text
}:
mkDerivation {
  pname = "mtags";
  version = "0.1.0.0";
  src = ./mtags;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cmark generic-lens prettyprinter raw-strings-qq rio validity
    validity-containers validity-text
  ];
  executableHaskellDepends = [ base optparse-generic rio ];
  testHaskellDepends = [
    base doctest doctest-driver-gen genvalidity-containers
    genvalidity-hspec genvalidity-text hspec rio
  ];
  homepage = "https://github.com/dbaynard/mtags#readme";
  description = "Generate ctags-compatible tags for (pandoc) markdown files";
  license = "(BSD-3-Clause OR Apache-2.0)";
  mainProgram = "mtags";
}
