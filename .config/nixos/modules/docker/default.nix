{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.docker;
in
with lib;
{
  options.jdb.docker = {
    enable = mkEnableOption "jdb.docker";
  };

  config = mkIf cfg.enable {
    environment.etc."docker/daemon.json" = {
      mode = "0644";
      text = ''
        {
          "cgroup-parent": "docker.slice"
        }
      '';
    };
  };
}
