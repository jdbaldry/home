{ config, pkgs, lib, ... }:

let cfg = config.services.grafana-agent-flow;
in
with lib;
{
  imports = [ ];

  options = {
    enable = mkEnableOption "grafana-agent-flow";

    configFile = mkOption {
      description = "Path to configuration file.";
      type = types.path;
    };
  };

  config = mkIf cfg.enable {
    systemd.services.grafana-agent-flow = {
      description = "Monitoring system and forwarder";
      documentation = [ "https://grafana.com/docs/agent/latest/" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      script = ''
        export HOSTNAME=$(< /proc/sys/kernel/hostname)
      '';

      serviceConfig = {
        Restart = "always";
        DynamicUser = true;

        Environment = [
          "EXPERIMENTAL_ENABLE_FLOW=true"
        ];

        StateDirectory = "grafana-agent";
        ExecStart = "${pkgs.grafana-agent}/bin/agent --config.file=${cfg.configFile}";
        TimeoutStopSec = "20s";
      };

      wantedBy = [ "multi-user.target" ];
    };
  };
}
