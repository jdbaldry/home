{ stdenv, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "tanka";
  version = "0.11.1";

  src = fetchFromGitHub {
    owner = "grafana";
    repo = pname;
    rev = "v${version}";
    sha256 = "0hp10qgalglsdhh6z6v4azh2hsr89mdrv1g5lssfl5jyink409yd";
  };

  subPackages = [ "cmd/tk" ];
  vendorSha256 = "0hgyibmxv4pkgwnw2ijnlga9mx2qj9liq529nvqm4j4hmj1xg4l5";

  meta = with stdenv.lib; {
    description = "Flexible, reusable and concise configuration for Kubernetes";
    license = licenses.asl20;
  };
}
