{ pkgs ? import <nixpkgs>, ... }:

with pkgs;

{
  accounts.email.accounts = {
    fastmail = {
      primary = true;

      address = "jdbaldry@fastmail.com";
      imap.host = "imap.fastmail.com";
      smtp.host = "smtp.fastmail.com";
      passwordCommand = "${pass}/bin/pass show smtp.fastmail.com:465/jdbaldry@fastmail.com";
      userName = "jdbaldry@fastmail.com";

      mbsync = {
        enable = true;

        create = "both";
      };
      msmtp.enable = true;
      mu.enable = true;

      realName = "jdb";
      signature = {
        text = ''
          Cheers,

          jdb
        '';
        showSignature = "append";
      };
    };
    gmail = {
      address = "jdbaldry@gmail.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      passwordCommand = "${pass}/bin/pass show smtp.gmail.com:465/jdbaldry@gmail.com";
      userName = "jdbaldry@gmail.com";

      mbsync = {
        enable = true;

        create = "both";
        extraConfig.account = {
          PipelineDepth = 1;
        };
      };
      msmtp.enable = true;
      mu.enable = true;

      realName = "jdb";
      signature = {
        text = ''
          Cheers,

          jdb
        '';
        showSignature = "append";
      };
    };
    grafana = {
      address = "jack.baldry@grafana.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      passwordCommand = "${pass}/bin/pass show smtp.gmail.com:465/jack.baldry@grafana.com";
      userName = "jack.baldry@grafana.com";

      mbsync = {
        enable = true;

        create = "both";
        extraConfig.account = {
          PipelineDepth = 1;
        };
      };
      msmtp.enable = true;
      mu.enable = true;

      realName = "jdb";
      signature = {
        text = ''
          Cheers,

          jdb
        '';
        showSignature = "append";
      };
    };
  };

  programs.bash = {
    enable = true;

    bashrcExtra = ''
      # Add SSH keys to keychain.
      eval "$(${keychain}/bin/keychain --quiet --eval github_rsa openwrt_git_rsa)"
      # Enable direnv.
      eval "$(${direnv}/bin/direnv hook bash)"

      # Add ~/bin to PATH.
      PATH="$HOME/bin:$PATH";

      # Configure emacsclient as editor.
      ALTERNATE_EDITOR="";
      VISUAL="emacsclient -c -a emacs";
      EDITOR="emacsclient -c -a emacs";

      # Open all browser windows in a new window.
      BROWSER="chromium --no-window";

      # Set environment file location for ssh-agent configuration on login.
      SSH_ENV="$HOME/.ssh/environment";

      # Alacritty terminal compatibility.
      TERM="tmux-256color";

      # GPG TTY.
      GPG_TTY="$(tty)";
    '';
    initExtra = ''
    '';
    shellAliases = {
      ap = "ansible-playbook";
      am = "alsamixer";
      kc = "kubectl";
      tf = "terraform";
      # Ensure the next command is checked as an alias when using watch.
      watch = "watch ";
      xc = "xclip - i - sel clipboard";
    };
  };
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.mu.enable = true;

  services.mbsync = {
    enable = true;

    postExec = "${mu}/bin/mu index";
    verbose = true;
  };
}
