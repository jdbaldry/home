{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.emacs;
in
with lib;
{
  options.jdb.emacs = {
    enable = mkEnableOption "jdb.emacs";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.emacsGit
    ];
  };
}
