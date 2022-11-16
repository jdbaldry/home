{ config, pkgs, lib, ... }:

let
  cfg = config.services.grafana-agent-flow;
  stateDir = "grafana-agent";
  user = "grafana-agent";
in
with lib;
{
  options.services.grafana-agent-flow = {
    enable = mkEnableOption "grafana-agent-flow";

    configFile = mkOption {
      description = "Path to configuration file.";
      type = types.path;
    };
    disableReporting = mkOption {
      description = "Disable reporting of enabled components to Grafana.";
      default = false;
      type = types.bool;
    };
    serverHTTPListenAddr = mkOption {
      description = "The address to listen for HTTP traffic on.";
      default = "127.0.0.1:12345";
      type = types.str;
    };
    serverHTTPUIPathPrefix = mkOption {
      description = "Prefix for the UI URL path. Should begin with a `/`";
      # TODO: Understand why this actually isn't runtime configurable.
      # Likely a problem with my packaging.
      # Agent default is "/".
      default = "/public";
      type = types.path;
    };
    storagePath = mkOption {
      description = "Base directory where components can store data";
      # Agent default is "data-agent/"
      default = "/var/lib/${stateDir}";
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

        exec ${pkgs.grafana-agent-flow}/bin/agent run ${cfg.configFile} \
      '' +
      (if cfg.disableReporting then
        "  --disable-reporting \\"
      else
        " ") +
      ''
        --server.http.listen-addr=${cfg.serverHTTPListenAddr} \
        --server.http.ui-path-prefix=${cfg.serverHTTPUIPathPrefix} \
        --storage.path=${cfg.storagePath}
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
