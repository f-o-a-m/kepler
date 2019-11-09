{ }:
let
  pkgs = import (builtins.fetchTarball {
    name = "nixpkgs-PR-71165";
    url = https://github.com/nixos/nixpkgs/archive/e3b740516f05307b55bcdb7e46c5d55da5fe0443.tar.gz;
    sha256 = "0ssl12nl3n5bp23yng3naji0yw012a12xb2sa8kwdp2ln0qf8mcy";
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

  hackageOverrides = self: super: {

    # dependency of avl-auth
    # marked as broken, fails with some `ld` error
    xxhash = self.unmarkBroken super.xxhash;
  };

  localOverrides = self: super:
    builtins.mapAttrs (name: path: (self.callCabal2nix name path {})) packages;

  repoOverrides = self: super:
    builtins.mapAttrs (name: path: (self.callCabal2nix name path {})) repoPackages;

  overrides = self: super:
    let allOverrides =
          hackageOverrides self super
          // repoOverrides self super
          // localOverrides self super;
    in
      builtins.mapAttrs (name: pkg: pkg.overrideAttrs (addBuildInputs (extra-build-inputs.${name} or []))) allOverrides;

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
          overrides
        ];
      };
    };
  };

in {
  inherit overrides;

  packages = {
    inherit (pkgs.haskellPackages)
      #hs-abci-example
      hs-abci-extra
      hs-abci-server
      hs-abci-types
      hs-tendermint-client
    ;
  };
}
