{ stdenv, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "iavl";
  version = "0.12.4";

  src = fetchFromGitHub {
    owner = "tendermint";
    repo = pname;
    rev = "de73d47fa87401bd935dffe3eca93ddaf0e3e105";
    sha256 = "1phf9jacgh30n4vgm5grncmk1yn80x32l7gpnlbscimmizyhrdl5";
  };

  modSha256 = "1czpf180drwi7i3v8n6kwbx7sm68nbyjm09x73ddfj7wk0dyqw1q";
}
