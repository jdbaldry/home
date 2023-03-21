{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.sway;
in
with lib;
{
  options.jdb.sway = {
    enable = mkEnableOption "jdb.sway";
  };

  config = mkIf cfg.enable {
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
    # xdg-desktop-portal-wlr is required for screensharing.
    environment.systemPackages = with pkgs; [ xdg-desktop-portal xdg-desktop-portal-wlr ];
    programs.sway.enable = true;
    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "sway";
        autoLogin.enable = true;
        autoLogin.user = "jdb";
      };
      layout = "gb";
      xkbOptions = "compose:caps";
    };
  };
}
