# From: https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
# To initialize the declarative management of userPackages:
# $ nix-env -f '<nixpkgs>' -r -iA userPackages
# Subsequently, to update userPackages:
# $ nix-rebuild
self: super:
let
  inherit (super) callPackage fetchFromGitHub;
  jsonnet-mode = callPackage ../jsonnet-mode.nix { };
in {
  userPackages = super.userPackages or { } // {
    # Packages to install
    complete-alias = callPackage ../complete-alias.nix { };
    direnv = self.direnv;
    emacs = self.emacsWithPackages (epkgs:
      with epkgs; [
        company
        dockerfile-mode
        eglot
        envrc
        expand-region
        fira-code-mode
        format-all
        go-autocomplete
        go-mode
        graphviz-dot-mode
        gruber-darker-theme
        js2-mode
        jsonnet-mode
        haskell-mode
        hindent
        magit
        markdown-mode
        multiple-cursors
        nix-mode
        nixpkgs-fmt
        org-pomodoro
        perspective
        pinentry
        projectile
        smartparens
        smex
        terraform-mode
        yaml-mode
      ]);
    diff-so-fancy = self.gitAndTools.diff-so-fancy;
    firefox = self.firefox;
    grafana-loki = super.grafana-loki.overrideAttrs (old: rec {
      version = "1.6.1";
      src = fetchFromGitHub {
        owner = "grafana";
        repo = "loki";
        rev = "v${version}";
        sha256 = "0bakskzizazc5cd6km3n6facc5val5567zinnxg3yjy29xdi64ww";
      };
    });
    gron = self.gron;
    lorri = self.lorri;
    niv = self.niv;
    powerline-go = super.powerline-go.overrideAttrs (old: rec {
      version = "ignore-namespace";

      src = fetchFromGitHub {
        owner = "jdbaldry";
        repo = "powerline-go";
        rev = "21d2b69a10d29e09f0c836ac086b4cd689bd7fb3";
        sha256 = "18cdkii3kx80hrk4l25blmpxl66nh27f5d6gy6pb8k15fcm76d75";
      };
    });
    vlc = self.vlc;
    zoom-us = self.zoom-us;

    nix = self.nix;

    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi
      exec nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
    '';
  };
}
