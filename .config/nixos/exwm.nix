{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.services.xserver.windowManager.jdb-exwm;
  jdb-emacs = ((pkgs.emacsPackagesFor pkgs.emacsGcc).emacsWithPackages (epkgs:
    with epkgs; [
      ace-link
      agda2-mode
      atomic-chrome
      bug-hunter
      code-review
      company
      counsel
      dap-mode
      deadgrep
      direnv
      dockerfile-mode
      eglot
      envrc
      evil
      expand-region
      exwm
      fira-code-mode
      flycheck
      flycheck-aspell
      flycheck-golangci-lint
      flymake-shellcheck
      forge
      format-all
      go-autocomplete
      go-mode
      graphviz-dot-mode
      gruber-darker-theme
      haskell-mode
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
