{ ghc
, nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz) {}
}:
with nixpkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "hs-abci";
  buildInputs = [ git hlint protobuf haskellPackages.stylish-haskell zlib ];
}
