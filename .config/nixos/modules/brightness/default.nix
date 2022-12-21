# Allow users to control backlight brightness.
{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.brightness;
in
with lib;
{
  options.jdb.brightness = {
    enable = mkEnableOption "jdb.brightness";
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = with pkgs;''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${coreutils}/bin/chown root:users /sys/class/backlight/%k/brightness", RUN+="${coreutils}/bin/chmod 0660 /sys/class/backlight/%k/brightness"
    '';
  };
}
