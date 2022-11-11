{ pkgs ? import <nixpkgs> }:

with pkgs;
let
  version = "0.28.0";
  src = fetchFromGitHub {
    owner = "grafana";
    repo = "agent";
    rev = "v${version}";
    sha256 = "sha256-UuDRnpb9JpghGDFsrlU7+iMboqiWVyT7qFSSPlLSFGs=";
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
        cp -r web/ui/* $out/
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
      cp -r "${modules}/src" $out
      cd $out
      yarn --offline run build
    '';
  };
in
(grafana-agent.override rec {
  buildGoModule = args: buildGo118Module (args // {
    inherit src version;
    doCheck = false;
    ldflags = [
      "-X github.com/grafana/agent/pkg/build.Branch=main"
      "-X github.com/grafana/agent/pkg/build.Version=${version}"
      "-X github.com/grafana/agent/pkg/build.Revision=v${version}"
      "-X github.com/grafana/agent/pkg/build.BuildUser=jdb"
    ];
    nativeBuildInputs = [ yarn ];
    preBuild = ''
      ln -sf ${ui}/node_modules web/ui/node_modules
      ln -sf ${ui}/build web/ui/build
    '';
    # tags = [ "builtinassets" ];
    vendorSha256 = "sha256-UEQYZbP3dzi7wZwX+InJrgHrFB1wfSUNmUMkit+Y1Lo=";
  });
}).overrideAttrs
  (old: rec {
    buildInputs = (old.buildInputs or [ ]) ++ [ bcc ];
  })
