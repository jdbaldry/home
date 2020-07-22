{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.luks.devices.nixos = {
    device = "/dev/disk/by-uuid/4f0e3f06-fbcb-4ba5-8598-3fee1deeafc5";
    preLVM = true;
    allowDiscards = true;
  };

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  time.timeZone = "Europe/London";

  environment.systemPackages = with pkgs; [
    alacritty
    bash-completion
    bind
    brave
    fzf
    gopass
    gnupg
    git
    gnumake
    jq
    jsonnet
    kubectl
    nixfmt
    nix-prefetch-git
    pinentry
    ripgrep
    rofi
    tmux
    unzip
    vim
    xclip
    yadm
  ];

  # Install extensions for chromium based browsers.
  programs.chromium = {
    enable = true;
    extensions = [
      "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader.
      "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix.
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium.
    ];
  };

  fonts = {
    fonts = with pkgs; [ fira-code fira-code-symbols powerline-fonts ];
    fontconfig.defaultFonts.monospace = [ "Fira Code" ];
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.fstrim.enable = true;
  services.compton.enable = true;
  services.printing.enable = true;
  # Enable the X11 windowing system.
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

  users.users.jdb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "network" ]; # Enable ‘sudo’ for the user.
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
