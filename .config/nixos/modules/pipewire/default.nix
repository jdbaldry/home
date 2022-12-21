{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.pipewire;
in
with lib;
{
  options.jdb.pipewire = {
    enable = mkEnableOption "pipewire";
  };

  # https://nixos.wiki/wiki/PipeWire
  config = mkIf cfg.enable {
    # It is reported that the pipewire service conflicts with sound.enable = true.
    sound.enable = false;

    # rtkit is optional but recommended.
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
