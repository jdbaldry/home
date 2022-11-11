{ pkgs ? import <nixpkgs> }:

with pkgs;
buildGoModule rec {
  pname = "mimir";
  version = "93e5d9426248c93681f4545026b8c80bc9fc37e4";

  subPackages = ["tools/listblock"];

  src = fetchgit {
    url = "ssh://git@github.com:grafana/mimir.git";
    rev = version;
    sha256 = lib.fakeSha256;
  };

  vendorSha256 = lib.fakeSha256;
}
