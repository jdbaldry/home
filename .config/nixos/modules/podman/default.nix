# Use Podman instead of Docker.
{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.podman;
in
with lib;
{
  options.jdb.podman = {
    enable = mkEnableOption "jdb.podman";
  };

  config = mkIf cfg.enable {
    virtualisation.podman.enable = true;
  };
}
