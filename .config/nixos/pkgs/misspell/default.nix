{ pkgs }:

with pkgs;
buildGoPackage rec {
  pname = "misspell";
  version = "master";

  src = fetchFromGitHub {
    owner = "jdbaldry";
    repo = pname;
    rev = "${version}";
    sha256 = "0icg1n522i34jfaqyyvld343p0x6ivpgfgjj6hhjf2bbdwd9nqa0";
  };

  goDeps = "${src}/deps.nix";
  goPackagePath = "github.com/client9/misspell";

  meta = with stdenv.lib; {
    description = "Correct commonly misspelled English words in source files";
  };
}
