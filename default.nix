{ }:
let
  pkgs = import (builtins.fetchTarball {
    name = "release-19.09";
    url = https://github.com/nixos/nixpkgs/archive/64a3ccb852d4f34abb015503affd845ef03cc0d9.tar.gz;
    sha256 = "0jigsyxlwl5hmsls4bqib0rva41biki6mwnswgmigwq41v6q7k94";
  }) { inherit config; };

  iavl = pkgs.callPackage ./iavl.nix {};
  tendermint = pkgs.callPackage ./tendermint.nix {};

  packages = {
    hs-abci-extra = ./hs-abci-extra;
    hs-abci-sdk = ./hs-abci-sdk;
    hs-abci-server = ./hs-abci-server;
    hs-abci-test-utils = ./hs-abci-test-utils;
    hs-abci-types = ./hs-abci-types;
    hs-iavl-client = ./hs-iavl-client;
    hs-tendermint-client = ./hs-tendermint-client;
    nameservice = ./hs-abci-docs/nameservice;
    simple-storage = ./hs-abci-docs/simple-storage;
  };

  repos = {
    avl-auth = pkgs.fetchFromGitHub {
      owner  = "oscoin";
      repo   = "avl-auth";
      rev    = "dfc468845a82cdd7d759943b20853999bc026505";
      sha256 = "005j98hmzzh9ybd8wb073i47nwvv1hfh844vv4kflba3m8d75d80";
    };
    http2-client-grpc = pkgs.fetchFromGitHub {
      owner  = "lucasdicioccio";
      repo   = "http2-client-grpc";
      rev    = "6a1aacfc18e312ef57552133f13dd1024c178706";
      sha256 = "0zqzxd6x3hlhhhq24pybjy18m0r66d9rddl9f2zk4g5k5g0zl906";
    };
  };

  repoPackages = {
    inherit (repos) avl-auth http2-client-grpc;
  };

  extra-build-inputs = with pkgs; {
    hs-abci-sdk = [protobuf];
    hs-abci-types = [protobuf];
    hs-iavl-client = [protobuf];
    simple-storage = [protobuf];
  };

  addBuildInputs = inputs: { buildInputs ? [], ... }: { buildInputs = inputs ++ buildInputs; };

  hackageOverrides = self: super: {
    polysemy = self.callHackageDirect {
      pkg = "polysemy";
      ver = "1.2.3.0";
      sha256 = "1smqaj57hkz5ldv5mr636lw6kxxsfn1yq0mbf8cy2c4417d6hyhm";
    } {};

    # polysemy-plugin 0.2.3.0 has doctest errors
    polysemy-plugin = self.callHackageDirect {
      pkg = "polysemy-plugin";
      ver = "0.2.4.0";
      sha256 = "1bjngyns49j76hgvw3220l9sns554jkqqc9y00dc3pfnik7hva56";
    } {};

    polysemy-zoo = self.callHackageDirect {
      pkg = "polysemy-zoo";
      ver = "0.6.0.0";
      sha256 = "1p0qd1zgnvx7l5m6bjhy9qn6dqdyyfz6c1zb79jggp4lrmjplp7j";
    } {};

    prometheus = self.callHackageDirect {
      pkg = "prometheus";
      ver = "2.1.3";
      sha256 = "04w3cm6r6dh284mg1lpzj4sl6d30ap3idkkdjzck3vcy5p788554";
    } {};

    proto3-suite = self.callHackageDirect {
      pkg = "proto3-suite";
      ver = "0.4.0.0";
      sha256 = "1s2n9h28j8rk9h041pkl4snkrx1ir7d9f3zwnj25an2xmhg5l0fj";
    } {};

    proto3-wire = self.callHackageDirect {
      pkg = "proto3-wire";
      ver = "1.1.0";
      sha256 = "0z8ifpl9vxngd2qaqj6bgg68z52m5i1shhd6j072g3mfdmiin0kv";
    } {};
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

  keplerTests = pkg: { runIavl ? false, runABCI ? null, runTendermint ? null}: pkgs.lib.overrideDerivation pkg (drv:
    let
      iavlScript = ''
        ${iavl}/bin/iavlserver -db-name "test" -datadir "." -grpc-endpoint "0.0.0.0:8090" -gateway-endpoint "0.0.0.0:8091" &
        sleep 3
      '';
      abciScript = ''
        ${runABCI} &
        sleep 3
      '';
      tendermintScript = ''
        ${tendermint}/bin/tendermint init --home $TMPDIR
        ${tendermint}/bin/tendermint node --home $TMPDIR --proxy_app=${runTendermint} &
        sleep 3
      '';
    in {
      checkPhase = pkgs.lib.concatStrings [
        (pkgs.lib.optionalString runIavl iavlScript)
        (pkgs.lib.optionalString (runABCI != null) abciScript)
        (pkgs.lib.optionalString (runTendermint != null) tendermintScript)
        drv.checkPhase
      ];
    });

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
          overrides
          (self: super: {
            avl-auth = pkgs.haskell.lib.dontCheck super.avl-auth;  # https://github.com/haskell-haskey/xxhash-ffi/issues/2
            bloodhound = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.unmarkBroken super.bloodhound); # tight bounds
            katip-elasticsearch = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken super.katip-elasticsearch); # needs elastic-search for tests
            proto3-suite = pkgs.haskell.lib.dontCheck super.proto3-suite; # needs old 'tasty'

            hs-tendermint-client = pkgs.haskell.lib.dontCheck super.hs-tendermint-client; # last test fails frequently
            hs-iavl-client = keplerTests super.hs-iavl-client { runIavl = true; };
            hs-abci-sdk = keplerTests super.hs-abci-sdk { runIavl = true; };
            simple-storage = keplerTests super.simple-storage {
              runIavl = true;
              runABCI = "IAVL_HOST=localhost IAVL_PORT=8090 dist/build/simple-storage/simple-storage";
              runTendermint= "tcp://localhost:26658";
            };
            nameservice = pkgs.haskell.lib.dontCheck super.nameservice; # TODO
          })
        ];
      };
    };
  };

in {
  inherit pkgs overrides;

  packages = {
    inherit (pkgs.haskellPackages)
      hs-abci-extra
      hs-abci-sdk
      hs-abci-server
      hs-abci-test-utils
      hs-abci-types
      hs-iavl-client
      hs-tendermint-client
      nameservice
      simple-storage
    ;
  };
}
