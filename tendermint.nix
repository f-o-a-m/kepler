{ stdenv, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "tendermint";
  version = "0.32.8";

  src = fetchFromGitHub {
    owner = "tendermint";
    repo = pname;
    rev = "v${version}";
    sha256 = "1bk9hb8whvlvxn8ijrp7rg6m83wxj4ipp5v36yr47ibqirbngh5s";
  };

  modSha256 = "08f03haxzpi57gaxymsbzs0nbbgnf6z4gmpal476xy3gvc0dyi3r";
}
