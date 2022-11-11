{ stdenv, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "cortex";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "cortexproject";
    repo = pname;
    rev = "v${version}";
    sha256 = "1wwf19x18wdcdrpz9hdkiw0dhfic5gxiqwsqr08jp8rm8chcqbqk";
  };

  CGO_ENABLED = 0;
  subPackages = "cmd/cortex cmd/query-tee cmd/test-exporter";
  modSha256 = "1f55m6dcmmmy7li6qdjx6xzb4yzr8kdd00b64v723cysa66647fj";

  meta = with stdenv.lib; {
    description =
      "A horizontally scalable, highly available, multi-tenant, long term Prometheus.";
    license = licenses.asl20;
  };
}
