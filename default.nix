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

  overrides = self: super:
    let
      callHackageDirect = {pkg, ver, sha256}@args:
        let pkgver = "${pkg}-${ver}";
        in self.callCabal2nix pkg (pkgs.fetchzip {
          url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
          inherit sha256;
        }) {};
    in {
      proto-lens = callHackageDirect {
        pkg = "proto-lens";
        ver = "0.5.0.1";
        sha256 = "1730b7p7yhp60spbmgflikkx98smfarz7h7wzrpric5pj7si6x44";
      };
      proto-lens-protoc = callHackageDirect {
        pkg = "proto-lens-protoc";
        ver = "0.5.0.0";
        sha256 = "05g9kdmwcv216l90w6r47hbmn0yx35w7lbj41gxrnha8axjzrxrq";
      };
      proto-lens-runtime = callHackageDirect {
        pkg = "proto-lens-runtime";
        ver = "0.5.0.0";
        sha256 = "15prbfk10xkb2q5ij5dajbcjwgbkw26h34i330kf3867h0mprs08";
      };
      proto-lens-setup = callHackageDirect {
        pkg = "proto-lens-setup";
        ver = "0.4.0.2";
        sha256 = "17lhdp6pcpk2ifcmnafr39150740ayjnyfbwvjc8wvbmlrd47d1n";
      };
      proto-lens-arbitrary = callHackageDirect {
        pkg = "proto-lens-arbitrary";
        ver = "0.1.2.6";
        sha256 = "17hksng65gdyg0rabv5xnfgwdv1vsq7sph3fwyq9wmgfk4dzxf3r";
      };

      # dependency of avl-auth
      # marked as broken, fails with some `ld` error
      xxhash =
        let
          # from recent nixpkgs
          unmarkBroken = drv: pkgs.haskell.lib.overrideCabal drv (drv: { broken = false; });
        in
          unmarkBroken super.xxhash;
    };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
          overrides
          (self: super: builtins.mapAttrs (name: path: (self.callCabal2nix name path {})) (repoPackages // packages))
          (self: super: builtins.mapAttrs (name: deps: super.${name}.overrideAttrs (addBuildInputs deps)) extra-build-inputs)
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
