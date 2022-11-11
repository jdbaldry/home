{ config, pkgs, lib, ... }:

let
  cfg = config.services.grafana-agent-flow;
  stateDir = "grafana-agent";
  user = "grafana-agent";
in
with lib;
{
  imports = [ ];

  options.services.grafana-agent-flow = {
    enable = mkEnableOption "grafana-agent-flow";

    configFile = mkOption {
      description = "Path to configuration file.";
      type = types.path;
    };
  };

  config = mkIf cfg.enable {
    users.groups.grafana-agent.gid = null;
    users.users.grafana-agent = {
      description = "Grafana Agent daemon user";
      group = config.users.groups.grafana-agent.name;
      isSystemUser = true;
    };
    systemd.services.grafana-agent-flow = {
      description = "Monitoring system and forwarder";
      documentation = [ "https://grafana.com/docs/agent/latest/" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      script = ''
        export HOSTNAME=$(< /proc/sys/kernel/hostname)
        export EXPERIMENTAL_ENABLE_FLOW=true

        exec ${pkgs.grafana-agent-flow}/bin/agent run ${cfg.configFile} --storage.path /var/lib/${stateDir} --disable-reporting
      '';

      serviceConfig = {
        Restart = "always";
        User = user;
        StateDirectory = stateDir;
        TimeoutStopSec = "20s";
      };

      wantedBy = [ "multi-user.target" ];
    };
  };
}
