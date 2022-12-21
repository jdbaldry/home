{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.ulimit;
in
with lib;
{
  options.jdb.ulimit = {
    enable = mkEnableOption "jdb.ulimit";
  };

  config = mkIf cfg.enable {
    # Increase ulimit.
    security.pam.loginLimits = [
      {
        domain = "*";
        type = "soft";
        item = "nofile";
        value = "524288";
      }
      {
        domain = "*";
        type = "hard";
        item = "nofile";
        value = "524288";
      }
    ];
  };
}
