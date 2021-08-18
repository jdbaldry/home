{ config, lib, pkgs, ... }:

lib.mkMerge [
  # Core
  {
    environment.systemPackages = with pkgs; [
      modem-manager-gui
      sgtpuzzles
    ];

    fonts = {
      fonts = with pkgs; [ fira-code fira-code-symbols powerline-fonts ];
      fontconfig.defaultFonts.monospace = [ "Fira Code" ];
    };

    hardware.pulseaudio.enable = true;

    networking.hostName = "mobile";

    # Force userdata for the target partition. It is assumed it will not
    # fit in the `system` partition.
    mobile.system.android.system_partition_destination = "userdata";

    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      (final: prev: { sudo = prev.sudo.override { withInsults = true; }; })
    ];
    nixpkgs.system = "aarch64-linux";

    powerManagement.enable = true;

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+i3";
        autoLogin.enable = true;
        autoLogin.user = "jdb";
      };
      libinput.enable = true;
      videoDrivers = lib.mkDefault [ "modesetting" "fbdev" ];
      windowManager = {
        i3.enable = true;
      };
    };

    time.timeZone = "Europe/London";

    users.users.jdb = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "video" ];
    };
  }

  # Set simple, known passwords for now.
  # TODO: Implement something more secure.
  {
    users.users.jdb.password = "nixos";
    users.users.root.password = "nixos";
  }

  # Bluetooth.
  {
    services.blueman.enable = false;
    hardware.bluetooth.enable = false;
  }

  # Networking, modem and misc.
  {
    users.extraUsers.jdb.extraGroups = [ "dialout" ];
    networking.interfaces.wlan0.useDHCP = true;
    networking.networkmanager.enable = true;
    networking.networkmanager.unmanaged = [ "rndis0" "usb0" ];
    networking.useDHCP = false;
    networking.wireless.enable = false;

    # Setup USB gadget networking in initrd.
    mobile.boot.stage-1.networking.enable = lib.mkDefault true;
  }

  # SSH.
  {
    networking.firewall.allowedTCPPorts = [ 22 ];
    services.openssh.enable = true;
    services.openssh.permitRootLogin = lib.mkForce "yes";
    systemd.services.sshd.wantedBy = lib.mkOverride 10 [ "multi-user.target" ];
  }

  # Flakes.
  {
    nix = {
      extraOptions = ''
        experimental-features = nix-commmand flakes
      '';
      package = unstable.nixFlakes;
      trustedUsers = [ "root" "jdb" ];
    };
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      (final: prev: { unstable = nixpkgs-unstable.legacyPackages.aarch64-linux; })
    ];
  }
]
