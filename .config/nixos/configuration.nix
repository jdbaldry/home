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
    boot.kernel.sysctl = {
      "net.ipv6.conf.all.disable_ipv6" = 1;
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
        difftastic
        direnv
        file
        firefox
        fwupd
        fzf
        git
        git-crypt
        gnumake
        gnupg
        go-jsonnet
        gopass
        gron
        grafana-agent-flow
        iftop
        imagemagick
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
        poppler_utils
        python3
        quakespasm
        retroarch
        ripgrep
        ripgrep-all
        rnix-lsp
        rofi
        runelite
        screenkey
        scrot
        scsh
        shellcheck
        shfmt
        signal-desktop
        slack
        snowball
        tanka
        tcpdump
        tmux
        unar
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
        watchexec
        wireshark
        xclip
        xinput_exporter
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
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      gc.automatic = true;
      package = pkgs.nixUnstable;
      settings = {
        allowed-users = [ "root" "jdb" ];
        trusted-users = [ "root" "jdb" ];
      };
    };
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
      inputs.jdb.overlay
      inputs.kooky.overlay
      inputs.jsonnet-language-server.overlay
      inputs.snowball.overlay
      inputs.xinput_exporter.overlay
      (final: prev: { sudo = prev.sudo.override { withInsults = true; }; })
      (final: prev:
        let
          version = "0.28.0";
          src = prev.pkgs.fetchFromGitHub {
            owner = "grafana";
            repo = "agent";
            rev = "v${version}";
            sha256 = "sha256-UuDRnpb9JpghGDFsrlU7+iMboqiWVyT7qFSSPlLSFGs=";
          };
        in
        {
          grafana-agent-flow-ui =
            let
              modules =
                prev.stdenv.mkDerivation {
                  inherit src version;
                  name = "flow-ui";
                  phases = [ "unpackPhase" "configurePhase" "installPhase" ];

                  # A version is required for mkYarnModules.
                  configurePhase = ''
                    sed -i '3i  "version": "0.0.0",' "web/ui/package.json"
                  '';

                  installPhase = ''
                    mkdir -p $out
                    cp -r web/ui/* $out/
                  '';
                };
            in
            prev.pkgs.mkYarnModules rec {
              inherit version;
              name = "${pname}-${version}";
              pname = "grafana-agent-flow-ui";

              packageJSON = "${modules}/package.json";
              yarnLock = "${modules}/yarn.lock";

              pkgConfig = {
                "." = {
                  postInstall = "yarn --offline run build";
                };
              };
            };
          grafana-agent-flow =
            (prev.pkgs.grafana-agent.override rec {
              buildGoModule = args: prev.pkgs.buildGo118Module (args // {
                inherit src version;
                doCheck = false;
                ldflags = [
                  "-X github.com/grafana/agent/pkg/build.Branch=main"
                  "-X github.com/grafana/agent/pkg/build.Version=${version}"
                  "-X github.com/grafana/agent/pkg/build.Revision=v${version}"
                  "-X github.com/grafana/agent/pkg/build.BuildUser=jdb"
                ];
                nativeBuildInputs = [ prev.pkgs.yarn ];
                preBuild = ''
                  ln -sf ${prev.pkgs.grafana-agent-flow-ui}/node_modules web/ui/node_modules
                '';
                # tags = [ "builtinassets" ];
                vendorSha256 = "sha256-UEQYZbP3dzi7wZwX+InJrgHrFB1wfSUNmUMkit+Y1Lo=";
              });
            }).overrideAttrs
              (old: rec {
                buildInputs = (old.buildInputs or [ ]) ++ [ prev.pkgs.bcc ];
              });
        })
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
    programs.systemtap.enable = true;

    fonts = {
      fonts = with pkgs; [ fira-code fira-code-symbols powerline-fonts ];
      fontconfig.defaultFonts.monospace = [ "Fira Code" ];
    };

    hardware.bluetooth.enable = true;

    services.fstrim.enable = true;
    services.k3s.enable = false;
    services.logind.lidSwitch = "hibernate";
    services.logind.lidSwitchDocked = "ignore";
    services.picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };
    services.grafana-agent-flow = {
      enable = true;
      configFile = ./agent.river;
    };
    services.prometheus = {
      enable = false;
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

    time.timeZone = "America/Barbados";

    users = {
      users = lib.fold (a: b: a // b)
        {
          jdb = {
            extraGroups =
              [ "disk" "wheel" "systemd-network" "docker" "networkmanager" ];
            isNormalUser = true;
          };
        }
        (builtins.map
          (i: {
            "guixbuilder${i}" = {
              group = "guixbuild";
              extraGroups = [ "guixbuild" ];
              home = "/var/empty";
              shell = pkgs.shadow;
              description = "Guix build user ${i}";
              isSystemUser = true;
            };
          }) [ "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" ]);
      groups.guixbuild.name = "guixbuild";
    };

    systemd.services.guix-daemon = {
      enable = true;
      description = "Build daemon for GNU Guix";
      serviceConfig = {
        ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
        Environment = "GUIX_LOCPATH=/root/.guix-profile/lib/locale";
        RemainAfterExit = "yes";
        StandardOutput = "syslog";
        StandardError = "syslog";
        TaskMax = 8192;
      };
      wantedBy = [ "multi-user.target" ];
    };

    systemd.mounts = [{
      description = "Read-only /gnu/store for GNU Guix";

      unitConfig = {
        DefaultDependencies = "no";
        ConditionPathExists = "/gnu/store";
        Before = "guix-daemon.service";
      };
      what = "/gnu/store";
      where = "/gnu/store";
      type = "none";
      options = "bind,ro";
      wantedBy = [ "guix-daemon.service" ];

    }];

    virtualisation.docker.enable = true;

    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?
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
        value = "524288";
      }
      {
        domain = "*";
        type = "hard";
        item = "nofile";
        value = "524288";
      }
    ];
  }
  {
    # Allow users to control backlight brightness.
    services.udev.extraRules = with pkgs;''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${coreutils}/bin/chown root:users /sys/class/backlight/%k/brightness", RUN+="${coreutils}/bin/chmod 0660 /sys/class/backlight/%k/brightness"
    '';
  }
  {
    # Configure DNS over HTTPS
    networking = {
      nameservers = [ "127.0.0.1" "1.1.1.1" ];
      resolvconf.enable = pkgs.lib.mkOverride 0 false;
    };

    services.dnscrypt-proxy2 = {
      enable = true;
      settings = {
        ipv6_servers = false;
        require_dnssec = true;

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
  }
  {
    # Configure Cachix nixos-community binary cache.
    nix = {
      binaryCaches = [ "https://nix-community.cachix.org" ];
      binaryCachePublicKeys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
    };
  }
  {
    # Configure ClamAV for Grafana endpoint antivirus.
    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
  }
  {
    # Run xinput_exporter.
    services.xinput_exporter = {
      enable = true;
      user = "jdb";
    };
  }
] //
{
  imports = [ ./hardware-configuration.nix ./exwm.nix ];
}
