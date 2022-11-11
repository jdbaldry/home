{ pkgs ? import <nixpkgs> }:
with pkgs;
buildGoModule rec {
  pname = "faillint";
  version = "1.7.0";

  doCheck = false;
  meta = with lib; { maintainers = with maintainers; [ jdbaldry ]; };
  src = fetchFromGitHub {
    owner = "fatih";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-eeHEUqVGTlJilwrxS+ehZ7061YjBJAkECuDGppeA8P8=";
  };
  vendorSha256 = "sha256-bnqVsVPQHu1xumbLrWVMV5aIoZaefL2+o5WUsqZ0Cwc=";
}
