{ pkgs ? import <nixpkgs> }:
with pkgs;
buildGoModule rec {
  pname = "jsonnetmod";
  version = "0.0.4";

  doCheck = false;
  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  src = fetchFromGitHub {
    owner = "octohelm";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-rZ2HDaFWewOVbLJazg+TQLoD60wtLCgk9t44VxfOhgM=";
  };
  vendorSha256 = "sha256-6Ar9VKraCo0h9QzYkMRvkb11PYrqXPTJkQ0hfu3IoiQ=";
}
