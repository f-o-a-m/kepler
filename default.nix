{ }:
let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-master-2019-06-19";
    url = https://github.com/nixos/nixpkgs/archive/7a569985214906d618ed4b2ad4f0629842cc2ec2.tar.gz;
    sha256 = "0laddzjvzbgdwxbvvpk98plwchyi6ymnypb7ymr50d2bwjw6r4qc";
  }) { inherit config; };

  packages = {
    hs-abci-example = ./hs-abci-example;
    hs-abci-extra = ./hs-abci-extra;
    hs-abci-server = ./hs-abci-server;
    hs-abci-types = ./hs-abci-types;
    hs-tendermint-client = ./hs-tendermint-client;
  };

  repos = {
    avl-auth = pkgs.fetchFromGitHub {
      owner  = "oscoin";
      repo   = "avl-auth";
      rev    = "dfc468845a82cdd7d759943b20853999bc026505";
      sha256 = "005j98hmzzh9ybd8wb073i47nwvv1hfh844vv4kflba3m8d75d80";
    };
  };

  repoPackages = {
    inherit (repos) avl-auth;
  };

  extra-build-inputs = with pkgs; {
    hs-abci-types = [protobuf];
  };

  addBuildInputs = inputs: { buildInputs ? [], ... }: { buildInputs = inputs ++ buildInputs; };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
          (self: super: builtins.mapAttrs (name: path: (self.callCabal2nix name path {})) (repoPackages // packages))
          (self: super: builtins.mapAttrs (name: deps: super.${name}.overrideAttrs (addBuildInputs deps)) extra-build-inputs)
          (self: super: {
            proto-lens = super.proto-lens_0_5_0_1;
            proto-lens-protoc = super.proto-lens-protoc_0_5_0_0;
            proto-lens-runtime = super.proto-lens-runtime_0_5_0_0;

            # dependency of avl-auth
            # marked as broken, fails with some `ld` error
            xxhash =
              let
                # from recent nixpkgs
                unmarkBroken = drv: pkgs.haskell.lib.overrideCabal drv (drv: { broken = false; });
              in
                unmarkBroken super.xxhash;
          })
        ];
      };
    };
  };

in { inherit (pkgs.haskellPackages)
       #hs-abci-example
       hs-abci-extra
       hs-abci-server
       hs-abci-types
       hs-tendermint-client
     ;
   }
