{ pkgs ? import <nixpkgs> }:
with pkgs;
buildGoModule rec {
  pname = "dyff";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "homeport";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-VcaPTUZBkpimYxaSZMFsbOTeYj8b59Lo0cYv6vIX4UQ=";
  };

  subPackages = [ "cmd/dyff" ];
  vendorSha256 = "sha256-XcYKgU1IjVtWR0OBVz2xBIPZbAx2/li3X7bAWF4Zpec=";

  meta = with stdenv.lib; {
    description = "/ˈdʏf/ - diff tool for YAML files, and sometimes JSON";
    license = licenses.mit;
  };
}
