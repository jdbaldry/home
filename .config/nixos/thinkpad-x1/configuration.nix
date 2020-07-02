# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  boot.kernel.sysctl = { "net.ipv4.ip_forward" = 1; };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices.nixos = {
    device = "/dev/disk/by-uuid/d846aa7f-5cb4-4d3a-b62f-b7f30ec16366";
    preLVM = true;
    allowDiscards = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
  };
  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;
  networking.interfaces.wwp0s20f0u6i12.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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
    direnv
    fzf
    git
    gnumake
    gopass
    gnupg
    jq
    jsonnet
    kubectl
    lorri
    niv
    nixfmt
    pandoc
    pinentry
    python3
    ripgrep
    rofi
    unzip
    vim
    virtmanager
    xclip
    yadm
  ];

  fonts = {
    fonts = with pkgs; [ jetbrains-mono powerline-fonts ];

    fontconfig.defaultFonts.monospace = [ "JetBrains Mono" ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = true;
  };

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  services.lorri.enable = true;

  services.xserver = {
    enable = true;
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

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jdb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
  };

  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}

