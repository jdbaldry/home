{ pkgs ? import <nixpkgs> }:
with pkgs;
stdenv.mkDerivation rec {
  pname = "complete-alias";
  version = "1.8.0";

  src = fetchFromGitHub {
    owner = "cykerway";
    repo = pname;
    rev = "${version}";
    sha256 = "sha256-BOfnFnxUZfsblE4qXUX6pzcuweC+b927egm2f1IogoM=";
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
