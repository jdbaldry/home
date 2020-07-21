{ stdenv, rustPlatform, fetchFromGitHub, ncurses }:

rustPlatform.buildRustPackage rec {
  pname = "cm";
  version = "master";

  src = fetchFromGitHub {
    owner = "tsoding";
    repo = pname;
    rev = version;
    sha256 = "1v5s8y34j1q9nj8iyicy4vixwjsh524nzv7q8cfcmgf6ava5v6i6";
  };

  cargoSha256 = "13mgrdpwrns51isxainq4gba0vbvpqbkqjxl5gia1swgbmljap7w";

  buildInputs = [
    ncurses ];

  meta = with stdenv.lib; {
    description = "Emacs' compilation-mode-like TUI application";
    homepage = "https://github.com/tsoding/cm";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
