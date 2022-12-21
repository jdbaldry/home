{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.printing;
in
with lib;
{
  options.jdb.printing = {
    enable = mkEnableOption "jdb.printing";
  };

  config = mkIf cfg.enable {
    services = {
      printing = {
        enable = true;

        drivers = with pkgs; [ brlaser brgenml1lpr brgenml1cupswrapper ];
      };
      vsftpd = {
        enable = true;

        extraConfig = ''
          log_ftp_protocol=Yes
          pasv_enable=Yes
          pasv_min_port=51000
          pasv_max_port=51005
        '';
        localUsers = true;
        userlist = [ "jdb" ];
        userlistDeny = false;
        writeEnable = true;
      };
    };
  };
}
