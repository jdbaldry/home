# Add Android virtualization.
{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.android;
in
with lib;
{
  options.jdb.android = {
    enable = mkEnableOption "jdb.android";
  };

  config = mkIf cfg.enable {
    virtualisation.waydroid.enable = true;
  };
}
