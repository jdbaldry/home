{ pkgs ? import <nixpkgs> }:
with pkgs;
buildGoModule rec {
  pname = "docsonnet";
  version = "3e17576";

  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  src = fetchFromGitHub {
    owner = "jsonnet-libs";
    repo = pname;
    rev = version;
    sha256 = "sha256-1tOPmCtQ/7S+nYByxHdVRmGSFBoHVjWSVvX+CAycWp8=";
  };
  vendorSha256 = "sha256-24vkXvPsjp4CJJc7ajqAbK66ywXyIaBj/8r8FLYVXPE=";
}
