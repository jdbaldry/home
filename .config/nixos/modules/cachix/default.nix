# Configure Cachix nixos-community binary cache.
{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.cachix;
in
with lib;
{
  options.jdb.cachix = {
    enable = mkEnableOption "jdb.cachix";
  };

  config = mkIf cfg.enable {
    nix.settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
    };
  };
}
