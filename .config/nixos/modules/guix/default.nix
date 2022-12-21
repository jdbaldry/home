{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.guix;
in
with lib;
{
  options.jdb.guix = {
    enable = mkEnableOption "jdb.guix";
  };

  config = mkIf cfg.enable {
    systemd.services.guix-daemon = {
      enable = true;
      description = "Build daemon for GNU Guix";
      serviceConfig = {
        ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
        Environment = "GUIX_LOCPATH=/root/.guix-profile/lib/locale";
        RemainAfterExit = "yes";
        StandardOutput = "syslog";
        StandardError = "syslog";
        TaskMax = 8192;
      };
      wantedBy = [ "multi-user.target" ];
    };

    systemd.mounts = [{
      description = "Read-only /gnu/store for GNU Guix";

      unitConfig = {
        DefaultDependencies = "no";
        ConditionPathExists = "/gnu/store";
        Before = "guix-daemon.service";
      };
      what = "/gnu/store";
      where = "/gnu/store";
      type = "none";
      options = "bind,ro";
      wantedBy = [ "guix-daemon.service" ];

    }];

    groups.guixbuild.name = "guixbuild";
    users.users =
      (map
        (i: {
          "guixbuilder${i}" = {
            group = "guixbuild";
            extraGroups = [ "guixbuild" ];
            home = "/var/empty";
            shell = pkgs.shadow;
            description = "Guix build user ${i}";
            isSystemUser = true;
          };
        }) [ "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" ]);
  };
}
