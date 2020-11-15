{ stdenv, bash, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "complete-alias";
  version = "1.7.0";

  src = fetchFromGitHub {
    owner = "cykerway";
    repo = pname;
    rev = "${version}";
    sha256 = "0ryrc7rjv3ypq6i7vbjsq8ibgs8m6jgxm9r38xikb36wrlkvg3l9";
  };

  phases = "installPhase fixupPhase";

  installPhase = ''
    mkdir -p $out/bin
    cp ${src}/complete_alias $out/bin/
    chmod +x $out/bin/complete_alias
  '';

  meta = with stdenv.lib; {
    description = "automagical shell alias completion";
    license = licenses.gpl3;
  };
}
