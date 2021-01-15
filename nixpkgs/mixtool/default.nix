{ stdenv, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "monitoring-mixins";
  version = "ae18e31";

  src = fetchFromGitHub {
    owner = pname;
    repo = "mixtool";
    rev = "${version}";
    sha256 = "sha256-ZRvgwyEKBYsbff/MnUVRwMJAB7kco4D9Lfs9yQiqofI=";
  };

  subPackages = [ "cmd/mixtool" ];
  vendorSha256 = "sha256-ahixB0pjBRJbCZp/r8MmbowTRLKySL1dfVxC9Z75wnw=";

  meta = with stdenv.lib; {
    description = "Helper for easily working with Jsonnet mixins";
    license = licenses.asl20;
  };
}
