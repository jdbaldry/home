{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.xserver.windowManager.jdb-exwm;
  jdb-emacs = (pkgs.emacsWithPackages (epkgs:
    with epkgs; [
      bug-hunter
      company
      counsel
      dap-mode
      direnv
      dockerfile-mode
      eglot
      envrc
      evil
      expand-region
      exwm
      fira-code-mode
      format-all
      flycheck
      flycheck-golangci-lint
      flymake-shellcheck
      forge
      go-autocomplete
      go-mode
      graphviz-dot-mode
      gruber-darker-theme
      haskell-mode
      ivy
      js2-mode
      jsonnet-mode
      lsp-mode
      lsp-ui
      keychain-environment
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
      pass
      pinentry
      projectile
      quack
      rainbow-delimiters
      ripgrep
      slack
      smartparens
      smex
      swiper
      terraform-mode
      w3m
      yaml-mode
      yasnippet
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
        ${jdb-emacs}/bin/emacs
      '';
    };
    environment.systemPackages = [
      jdb-emacs
      pkgs.sqlite # Used by Emacs org-roam.
    ];
  };
}
