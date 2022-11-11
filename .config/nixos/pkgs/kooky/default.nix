{ pkgs ? import <nixpkgs> }:

with pkgs;
buildGoModule rec {
  pname = "kooky";
  version = "0.2.0";

  doCheck = false;
  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  src = fetchFromGitHub {
    owner = "zellyn";
    repo = pname;
    rev = "${version}";
    sha256 = "sha256-0p8elWEg+X9/HkqHXeSr0RHg1PZx1ase/URbgrBvvBU=";
  };
  vendorSha256 = "sha256-WcKJwubsPreUDM0HBrCUYfMkoQCu+tEMhkAe+h7Gq24=";
}
