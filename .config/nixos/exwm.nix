{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.xserver.windowManager.jdb-exwm;
  jdb-emacs = ((
    pkgs.emacsPackagesFor
      (pkgs.emacsNativeComp.overrideAttrs (old: rec {
        buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.imagemagick ];
        configureFlags = (old.configureFlags or [ ]) ++ [ "--with-imagemagick" ];
      }))
  ).emacsWithPackages
    (epkgs:
      with epkgs; [
        ace-link
        agda2-mode
        atomic-chrome
        bazel
        bug-hunter
        code-review
        company
        company-native-complete
        counsel
        counsel-projectile
        dap-mode
        deadgrep
        direnv
        dockerfile-mode
        eglot
        emacsql-sqlite-builtin
        envrc
        evil
        expand-region
        exwm
        esup
        fira-code-mode
        flycheck
        flycheck-aspell
        flycheck-golangci-lint
        flymake-shellcheck
        forge
        format-all
        geiser-guile
        go-autocomplete
        go-mode
        graphviz-dot-mode
        gruber-darker-theme
        guix
        haskell-mode
        hcl-mode
        ivy
        js2-mode
        jsonnet-mode
        keycast
        keychain-environment
        kubernetes
        lsp-haskell
        lsp-mode
        lsp-ui
        magit
        markdown-mode
        multiple-cursors
        nix-mode
        nixpkgs-fmt
        ob-async
        org-gcal
        org-pomodoro
        org-roam
        origami
        parinfer-rust-mode
        pass
        pinentry
        projectile
        quack
        rainbow-delimiters
        request
        ripgrep
        slack
        smartparens
        smex
        swiper
        terraform-mode
        web-mode
        w3m
        which-key
        yaml-mode
        yasnippet
        xterm-color
      ]));
in
{
  options.services.xserver.windowManager.jdb-exwm = {
    enable = mkEnableOption "jdb-exwm";
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.session = singleton {
      name = "jdb-exwm";
      start = ''
        ${jdb-emacs}/bin/emacs --daemon --eval "(require 'exwm)" -f exwm-enable
        exec ${jdb-emacs}/bin/emacsclient -c
      '';
    };
    environment.systemPackages = [
      jdb-emacs
      pkgs.sqlite # Used by Emacs org-roam.
    ];
  };
}
