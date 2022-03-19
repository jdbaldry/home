{ config, lib, pkgs, inputs, ... }:

lib.mkMerge [
  # Core. Imports need to go at the end.
  {
    # Used to emulate aarch64-linux when initially building nixos-mobile.
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
    boot.initrd.luks.devices.nixos = {
      device = "/dev/disk/by-uuid/4f0e3f06-fbcb-4ba5-8598-3fee1deeafc5";
      preLVM = true;
      allowDiscards = true;
    };
    boot.kernelPackages = pkgs.linuxPackages_latest;
    # systemd.unified_cgroup_hierarchy needed because of https://github.com/moby/moby/issues/42275.
    boot.kernel.sysctl = { "net.ipv6.conf.all.disable_ipv6" = 1; };
    boot.kernelParams = [ "systemd.unified_cgroup_hierarchy=0" "cgroup_enable=memory" "cgroup_enable=cpuset" ];
    # Use the systemd-boot EFI boot loader.
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot.enable = true;

    console = {
      font = "Lat2-Terminus16";
      keyMap = "uk";
    };

    environment.etc."docker/daemon.json" = {
      mode = "0644";
      text = ''
        {
          "cgroup-parent": "docker.slice"
        }
      '';
    };
    # TODO: migrate to home-manager.
    environment.systemPackages = with pkgs;
      let recordingTools = [ avidemux obs-studio ]; in
      recordingTools ++
      [
        alacritty
        aspell
        aspellDicts.en
        aspellDicts.en-computers
        autorandr
        bash-completion
        bc
        bind
        brave
        chromium
        complete-alias
        cue
        direnv
        file
        firefox
        fwupd
        fzf
        git
        git-crypt
        gitAndTools.diff-so-fancy
        gnumake
        gnupg
        go-jsonnet
        gopass
        gron
        iftop
        iotop
        ispell
        jq
        jsonnet-bundler
        jsonnet-language-server
        keychain
        keynav
        kooky
        kubectl
        libretro.beetle-gba
        mtr
        mupdf
        niv
        nix-prefetch-git
        nixpkgs-fmt
        nyxt
        oil
        pandoc
        pass
        pinentry
        python3
        quakespasm
        retroarch
        ripgrep
        rnix-lsp
        rofi
        screenkey
        scrot
        scsh
        shellcheck
        shfmt
        slack
        snowball
        tanka
        tcpdump
        tmux
        unzip
        vcsh
        vim
        vlc
        (vscode-with-extensions.override {
          vscodeExtensions = with vscode-extensions;
            [ ms-vsliveshare.vsliveshare ]
            ++ vscode-utils.extensionsFromVscodeMarketplace [
              {
                name = "emacs-mcx";
                publisher = "tuttieee";
                version = "0.27.0";
                sha256 = "sha256-AzTie/R55hjdI4T4I0ePCvZqUuKU/Ipsmjy1wvg6uIw=";
              }
              {
                name = "go";
                publisher = "golang";
                version = "0.24.2";
                sha256 = "sha256-R34n3TRvIKGfG7x+OVVBDd3JlolPwyWZ7EEWih9xI0Y=";
              }
            ];
        })
        wireshark
        xclip
        yadm
        zoom-us
      ];

    networking.firewall.allowedTCPPortRanges = [{ from = 51000; to = 51005; }]; # vsftpd
    networking.firewall.allowedTCPPorts = [ 21 ]; # ftp
    networking.hostName = "nixos";
    networking.interfaces.wlp0s20f3.useDHCP = true;
    networking.networkmanager.enable = true;
    networking.useDHCP = false;

    nix = {
      allowedUsers = [ "root" "jdb" ];
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      gc.automatic = true;
      package = pkgs.nixUnstable;
      trustedUsers = [ "root" "jdb" ];
    };
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
      inputs.jdb.overlay
      inputs.jsonnet-language-server.overlay
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
    programs.systemtap.enable = true;

    fonts = {
      fonts = with pkgs; [ fira-code fira-code-symbols powerline-fonts ];
      fontconfig.defaultFonts.monospace = [ "Fira Code" ];
    };

    hardware.bluetooth.enable = true;

    services.autorandr = {
      enable = true;
      defaultTarget = "laptop";
    };
    services.fstrim.enable = true;
    services.k3s.enable = false;
    services.logind.lidSwitch = "ignore";
    services.logind.lidSwitchDocked = "ignore";
    services.picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };
    services.prometheus = {
      enable = true;
      configText =
        if builtins.pathExists ./prometheus.yml.secret then
          (builtins.readFile ./prometheus.yml.secret)
        else
          "";
      exporters.node = {
        enable = true;
        enabledCollectors = [ "logind" "systemd" "hwmon" ];
        disabledCollectors = [ "rapl" "textfile" ];
        openFirewall = true;
        firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
      };
    };
    services.vsftpd = {
      enable = true;
      localUsers = true;
      userlist = [ "jdb" ];
      userlistDeny = false;
      writeEnable = true;
      extraConfig = ''
        log_ftp_protocol=Yes
        pasv_enable=Yes
        pasv_min_port=51000
        pasv_max_port=51005
      '';
    };

    services.xserver = {
      enable = true;
      displayManager = {
        defaultSession = "none+jdb-exwm";
        autoLogin.enable = true;
        autoLogin.user = "jdb";
      };
      layout = "gb";
      xkbOptions = "compose:caps";
      windowManager = {
        i3.enable = true;
        jdb-exwm.enable = true;
      };
    };

    time.timeZone = "Europe/London";

    users.users.jdb = {
      extraGroups =
        [ "disk" "wheel" "systemd-network" "docker" "networkmanager" ];
      isNormalUser = true;
    };

    virtualisation.docker.enable = true;

    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?
  }
  # Configure mobile as a nix builder.
  {
    nix.buildMachines = [{
      hostName = "mobile";
      system = "aarch64-linux";
      maxJobs = 1;
    }];
    nix.distributedBuilds = true;
  }
  # Configure sound.
  {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
  }
  # Configure printing.
  {
    services.printing = {
      enable = true;

      drivers = with pkgs; [ brlaser brgenml1lpr brgenml1cupswrapper ];
    };
  }
  # Auto clean the Nix store.
  {
    nix = {
      settings.auto-optimise-store = true;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
    };
  }
  {
    # Increase ulimit.
    security.pam.loginLimits = [
      {
        domain = "*";
        type = "soft";
        item = "nofile";
        value = "8192";
      }
      {
        domain = "*";
        type = "hard";
        item = "nofile";
        value = "8192";
      }
    ];
  }
] //
{
  imports = [ ./hardware-configuration.nix ./exwm.nix ];
}
