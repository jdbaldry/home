{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.pulseaudio;
in
with lib;
{
  options.jdb.pulseaudio = {
    enable = mkEnableOption "pulseaudio";
  };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    services.pipewire.enable = false;
  };
}
