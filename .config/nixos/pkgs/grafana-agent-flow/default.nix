{ pkgs ? import <nixpkgs> }:

with pkgs;
let
  version = "0.29.0";
  src = fetchFromGitHub {
    owner = "grafana";
    repo = "agent";
    rev = "v${version}";
    sha256 = "sha256-6CnYoUECT6vcQw2v7GLRzOtlL4tKKpz4VADuz9MxseM=";
  };
in
let
  modules =
    stdenv.mkDerivation {
      inherit src version;
      name = "flow-ui";
      phases = [ "unpackPhase" "configurePhase" "installPhase" ];

      # A version is required for mkYarnModules.
      configurePhase = ''
        sed -i '3i  "version": "0.0.0",' "web/ui/package.json"
      '';

      installPhase = ''
        mkdir -p $out
        cp -r web/ui/* web/ui/.env web/ui/.env.production $out/
      '';
    };
in
let
  ui = mkYarnModules rec {
    inherit version;
    name = "${pname}-${version}";
    pname = "grafana-agent-flow-ui";

    packageJSON = "${modules}/package.json";
    yarnLock = "${modules}/yarn.lock";

    postBuild = ''
      cp -r "${modules}/.env" \
            "${modules}/.env.production" \
            "${modules}/package.json" \
            "${modules}/public" \
            "${modules}/src" \
            "${modules}/tsconfig.json" \
            $out
      cd $out
      yarn --offline run build
    '';
  };
in
(grafana-agent.override rec {
  buildGoModule = args: buildGo118Module (args // {
    inherit src version;
    doCheck = false;
    ldflags = let prefix = "github.com/grafana/agent/pkg/build"; in [
      "-s"
      "-w"
      "-X ${prefix}.Branch=main"
      "-X ${prefix}.Version=${version}"
      "-X ${prefix}.Revision=v${version}"
      "-X ${prefix}.BuildUser=jdb"
      "-X ${prefix}.BuildDate=1970-01-01T00:00:00Z"
    ];
    preBuild = ''
      ln -sf ${ui}/node_modules web/ui/node_modules
      cp -r ${ui}/build web/ui/build
    '';
    tags = [
      "builtinassets"
      "nodocker"
      "noebpf"
      "nonetwork"
    ];
    vendorSha256 = "sha256-FSxkldMYMmyjVv6UYeZlceygkfKFzZK2udeUNBbpYnc=";
  });
})
