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
    virtualisation = {
      podman = {
        enable = true;

        # Create a `docker` alias for podman, to use it as a drop-in replacement
        dockerCompat = true;

        # Required for containers under podman-compose to be able to talk to each other.
        defaultNetwork.dnsname.enable = true;
      };
    };
  };
}
