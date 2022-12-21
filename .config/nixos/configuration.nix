{ config, lib, pkgs, inputs, ... }:

with lib;
{
  imports = [ ./hardware-configuration.nix ./exwm.nix ];

  # Used to emulate aarch64-linux when initially building nixos-mobile.
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.initrd.luks.devices.nixos = {
    device = "/dev/disk/by-uuid/4f0e3f06-fbcb-4ba5-8598-3fee1deeafc5";
    preLVM = true;
    allowDiscards = true;
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # systemd.unified_cgroup_hierarchy needed because of https://github.com/moby/moby/issues/42275.
  boot.kernel.sysctl = {
    "dev.i915.perf_stream_paranoid" = 0;
  };
  boot.kernelParams = [ "systemd.unified_cgroup_hierarchy=0" "cgroup_enable=memory" "cgroup_enable=cpuset" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  # TODO: migrate to home-manager.
  environment.systemPackages = import ./packages.nix { inherit pkgs; };

  jdb = {
    android.enable = true;
    dns.enable = true;
    pipewire.enable = true;
    podman.enable = true;
    ulimit.enable = true;
  };

  networking.firewall.allowedTCPPortRanges = [{ from = 51000; to = 51005; }]; # vsftpd
  networking.firewall.allowedTCPPorts = [ 21 ]; # ftp
  networking.hostName = "nixos";
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixUnstable;
    settings = {
      auto-optimise-store = true;
      allowed-users = [ "root" "jdb" ];
      trusted-users = [ "root" "jdb" ];
    };
  };
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.kooky.overlay
    inputs.snowball.overlay
    inputs.xinput_exporter.overlay
    (final: prev: { sudo = prev.sudo.override { withInsults = true; }; })
    (import ./overlay.nix)
  ];

  # Install extensions for chromium based browsers.
  programs.chromium = {
    enable = true;
    extensions = [
      "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader.
      "godiecgffnchndlihlpaajjcplehddca" # GhostText.
      "dndlcbaomdoggooaficldplkcmkfpgff" # New Tab, New Window.
      "oemmndcbldboiebfnladdacbdfmadadm" # PDF Viewer.
      "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix.
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium.
    ];
  };
  programs.emacs.enable = true;
  programs.sway.enable = true;
  programs.systemtap.enable = true;

  fonts = {
    fonts = with pkgs; [ fira-code fira-code-symbols powerline-fonts ];
    fontconfig.defaultFonts.monospace = [ "Fira Code" ];
  };

  hardware.bluetooth.enable = true;

  services = {
    # Configure ClamAV for Grafana endpoint antivirus.
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
    fstrim.enable = true;
    k3s.enable = false;
    logind.lidSwitch = "hibernate";
    logind.lidSwitchDocked = "ignore";
    picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };
    grafana-agent-flow = {
      enable = true;
      configFile = ./agent.river;
      disableReporting = true;
    };
    prometheus = {
      exporters.node = {
        enable = true;
        enabledCollectors = [ "logind" "systemd" "hwmon" ];
        disabledCollectors = [ "rapl" "textfile" ];
        openFirewall = true;
        firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
      };
    };
    # Run xinput_exporter.
    xinput_exporter = {
      enable = true;
      user = "jdb";
    };
    xserver = {
      enable = true;
      displayManager = {
        defaultSession = "sway";
        autoLogin.enable = true;
        autoLogin.user = "jdb";
      };
      layout = "gb";
      xkbOptions = "compose:caps";
    };
  };

  time.timeZone = "Europe/London";

  users.users.jdb = {
    extraGroups =
      [ "disk" "wheel" "systemd-network" "docker" "networkmanager" ];
    isNormalUser = true;
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}

