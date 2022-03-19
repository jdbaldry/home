{ pkgs ? import <nixpkgs> }:

with pkgs;
nodePackages.buildNodePackage rec {
  name = "jsdoc-to-markdown";
  version = "7.0.0";
  src = fetchFromGitHub {
    owner = "jsdoc2md";
    repo = name;
    rev = "v${version}";
    sha256 = "sha256-jUjl/NbWd5/+QaozLRNA5kajeva10QJ+gTN8jqV7h+U=";
  };
  distPhase = "true";
}
