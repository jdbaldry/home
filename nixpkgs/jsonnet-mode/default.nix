{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "jsonnet-mode";
  version = "master";

  src = fetchFromGitHub {
    owner = "jdbaldry";
    repo = pname;
    rev = version;
    sha256 = "1vxsv1fy4yg1nwhknfdx2y14jddgd4m58j1gsqn0bq5ghp8zj74j";
  };
  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    cp *.el *.elc $out/share/emacs/site-lisp/
  '';

  meta = {
    description =
      "Emacs major mode for editin Jsonnet files. Forked from https://github.com/mgyucht/jsonnet-mode.";
    homepage = "https://github.com/jdbaldry/jsonnet-mode";
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
