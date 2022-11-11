{ pkgs ? import <nixpkgs> }:

with pkgs;
buildGoModule rec {
  pname = "monitoring-mixins";
  version = "29fe1d8";

  src = fetchFromGitHub {
    owner = pname;
    repo = "mixtool";
    rev = "${version}";
    sha256 = "sha256-8KWZCVQaPxDPQItnmIK3I5yvrdYeZfRizYK2V3sgdoY=";
  };

  doCheck = false;
  subPackages = [ "cmd/mixtool" ];
  vendorSha256 = "sha256-pshD0Rb+vkkYVcJRQ98TCWZn2kO99u+mbrA9npVlrrs=";

  meta = with lib; {
    description = "Helper for easily working with Jsonnet mixins";
    license = licenses.asl20;
  };
}
