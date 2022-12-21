{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.programs.emacs;
  emacs = ((
    pkgs.emacsPackagesFor
      (pkgs.emacsPgtk.overrideAttrs (old: rec {
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
        shackle
        slack
        smartparens
        smex
        sway
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
  options.programs.emacs = {
    enable = mkEnableOption "emacs";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      emacs
      pkgs.sqlite # Used by Emacs org-roam.
    ];
  };
}
