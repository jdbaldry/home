{ pkgs ? import <nixpkgs> }:
with pkgs;
buildGoModule rec {
  pname = "docsonnet";
  version = "v0.0.3";

  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  src = fetchFromGitHub {
    owner = "jsonnet-libs";
    repo = pname;
    rev = version;
    sha256 = "sha256-ufiGEAn0PO48CO6esD2lKWxkiU2iCv25jAUqJq8TTTs=";
  };
  vendorSha256 = "sha256-24vkXvPsjp4CJJc7ajqAbK66ywXyIaBj/8r8FLYVXPE=";
}
