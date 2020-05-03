# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/f1651c60-28df-4518-a1f9-07b4cf110cee";
      preLVM = true;
      allowDiscards = true;
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Install 32 bit driver support for Steam.
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  networking.hostName = "nixos";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    bash-completion
    bind
    brave
    fzf
    git
    gnumake
    go
    gopass
    gnupg
    jq
    jsonnet
    kubectl
    nixfmt
    pandoc
    pinentry
    python3
    ripgrep
    rofi
    sshuttle
    unzip
    vim
    xclip
    yadm
  ];

  fonts = {
    fonts = with pkgs; [ jetbrains-mono powerline-fonts ];

    fontconfig = { defaultFonts = { monospace = [ "JetBrains Mono" ]; }; };
  };

  # Install extensions for chromium based browsers.
  programs.chromium = {
    enable = true;
    extensions = [
      "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix.
    ];
  };

  # List services that you want to enable:
  services.compton = {
    enable = true;
    backend = "glx";
    vSync = true;
  };
  services.xserver = {
    enable = true;
    # Enable touchpad support.
    libinput.enable = true;
    displayManager.defaultSession = "none+i3";
    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "jdb";
    };
    layout = "gb";
    windowManager.i3.enable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.jdb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
