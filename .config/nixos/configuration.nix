{ config, pkgs, inputs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.initrd.luks.devices.nixos = {
    device = "/dev/disk/by-uuid/4f0e3f06-fbcb-4ba5-8598-3fee1deeafc5";
    preLVM = true;
    allowDiscards = true;
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "cgroup_enable=memory" "cgroup_enable=cpuset" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  environment.systemPackages = with pkgs; [
    alacritty
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    autorandr
    bash-completion
    bc
    bind
    brave
    complete-alias
    cue
    direnv
    (emacsWithPackages (epkgs:
      with epkgs; [
        bug-hunter
        company
        counsel
        dockerfile-mode
        eglot
        envrc
        evil
        expand-region
        fira-code-mode
        format-all
        flymake-shellcheck
        go-autocomplete
        go-mode
        graphviz-dot-mode
        gruber-darker-theme
        haskell-mode
        ivy
        js2-mode
        jsonnet-mode
        keychain-environment
        magit
        markdown-mode
        multiple-cursors
        nix-mode
        nixpkgs-fmt
        org-gcal
        org-pomodoro
        org-roam
        origami
        perspective
        pinentry
        projectile
        rainbow-delimiters
        ripgrep
        slack
        smartparens
        smex
        swiper
        terraform-mode
        w3m
        yaml-mode
      ]))
    file
    firefox
    fwupd
    fzf
    gopass
    gron
    gnupg
    git
    gitAndTools.diff-so-fancy
    git-crypt
    gnumake
    grafana-loki
    iftop
    iotop
    ispell
    jq
    jsonnet
    jsonnet-bundler
    keychain
    k9s
    lorri
    kubectl
    mtr
    niv
    nixfmt
    nix-prefetch-git
    nyxt
    pass
    pinentry
    powerline-go
    ripgrep
    rnix-lsp
    rofi
    shfmt
    shellcheck
    scrot
    sqlite # Used by Emacs org-roam.
    tanka
    tcpdump
    telnet
    tmux
    unzip
    vim
    vlc
    wireshark
    xclip
    yadm
    zoom-us
  ];

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-commmand flakes
    '';
    trustedUsers = [ "root" "jdb" ];
  };
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.jdb.overlay
    (final: prev: { sudo = prev.sudo.override { withInsults = true; }; })
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

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.fstrim.enable = true;
  services.k3s.enable = false;
  services.logind.lidSwitch = "hibernate";
  services.logind.lidSwitchDocked = "hibernate";
  services.lorri.enable = true;
  services.printing.enable = true;
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [ "logind" "systemd" "hwmon" ];
    disabledCollectors = [ "textfile" ];
    openFirewall = true;
    firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
  };
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
  };
  services.prometheus = {
    enable = true;
    configText = if builtins.pathExists ./prometheus.yml.secret then
      (builtins.readFile ./prometheus.yml.secret)
    else
      "";
  };
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager = {
      defaultSession = "none+i3";
      autoLogin.enable = true;
      autoLogin.user = "jdb";
    };
    layout = "gb";
    windowManager.i3.enable = true;
    xkbOptions = "compose:caps";
  };

  time.timeZone = "Europe/London";

  users.users.jdb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "systemd-network" "docker" "networkmanager" ];
  };

  virtualisation.docker.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
