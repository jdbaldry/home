{ pkgs ? import <nixpkgs> }:

with pkgs;
stdenv.mkDerivation rec {
  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  name = "whatsapp-for-linux";
  version = "1.2.0";

  buildInputs = [ glib-networking gtkmm3 libappindicator-gtk3 webkitgtk ];
  nativeBuildInputs = [ cmake pkg-config ];
  src = fetchFromGitHub {
    owner = "eneshecan";
    repo = name;
    rev = "v${version}";
    sha256 = "sha256-dB+NsoUEYM3cT0cg5ZOkBGW7ozRGFWSsYQMja3CjaHM=";
  };
}
