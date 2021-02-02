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
        org-pomodoro
        org-roam
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
    pass
    pinentry
    (powerline-go.overrideAttrs (old: rec {
      version = "ignore-namespace";

      src = fetchFromGitHub {
        owner = "jdbaldry";
        repo = "powerline-go";
        rev = "21d2b69a10d29e09f0c836ac086b4cd689bd7fb3";
        sha256 = "18cdkii3kx80hrk4l25blmpxl66nh27f5d6gy6pb8k15fcm76d75";
      };
    }))
    ripgrep
    rnix-lsp
    rofi
    scrot
    sqlite # Used by Emacs org-roam.
    tanka
    tcpdump
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
  nixpkgs.overlays = [ inputs.emacs-overlay.overlay inputs.jdb.overlay ];

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
  # TODO: configure user access declaratively.
  services.kubernetes = {
    easyCerts = true;
    masterAddress = "localhost";
    roles = ["master" "node" ];
  };
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
    extraGroups = [ "wheel" "network" "docker" ];
  };

  virtualisation.docker.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
