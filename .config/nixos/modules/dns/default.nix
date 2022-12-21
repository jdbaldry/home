{ config, pkgs, lib, ... }:

let
  cfg = config.jdb.dns;
in
with lib;
{
  options.jdb.dns = {
    enable = mkEnableOption "jdb.dns";
  };

  config = mkIf cfg.enable {
    networking = {
      nameservers = [ "127.0.0.1" ];
      resolvconf.enable = true;
    };

    environment.etc."dnscrypt-proxy/cloaking.txt" = {
      mode = "0664";
      group = "users";
      text = ''
      '';
    };

    services.dnscrypt-proxy2 = {
      enable = true;
      settings = {
        ipv6_servers = true;
        listen_addresses = [ "127.0.0.1:53" "[::1]:53" ];
        require_dnssec = true;
        cloaking_rules = "/etc/dnscrypt-proxy/cloaking.txt";
        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
      };
    };
  };
}
