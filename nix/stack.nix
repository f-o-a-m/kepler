{ ghc
, nixpkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-PR-71165";
    url = https://github.com/nixos/nixpkgs/archive/e3b740516f05307b55bcdb7e46c5d55da5fe0443.tar.gz;
    sha256 = "0ssl12nl3n5bp23yng3naji0yw012a12xb2sa8kwdp2ln0qf8mcy";
  }) {}
}:
with nixpkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "hs-abci";
  buildInputs = [ git hlint protobuf secp256k1 haskellPackages.stylish-haskell zlib ];
}
