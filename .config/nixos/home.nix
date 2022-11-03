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

  home.stateVersion = "21.05";

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

      # Guix
     GUIX_PROFILE="/home/jdb/.guix-profile"
     . "$GUIX_PROFILE/etc/profile"
     PATH="/usr/local/bin:$PATH"

     # xcc copies the command and output to the clipboard for sharing.
     # For example, to re-run the previous command and capture its output:
     #   do_thing
     #   xcc !!
     #
     # Clipboard will have:
     # $ do thing
     #
     # <output>
     function xcc() {
       local cmd=""
       for word in "''${@}"; do
         if [[ "''${BASH_ALIASES[''${word}]+_}" ]]; then
           cmd+="''${BASH_ALIASES[''${word}]}"
         else
           cmd+="''${word}"
         fi
         cmd+=" "
       done
       readonly cmd
       {
         printf "$ %s\n" "''${cmd}"
         eval "''${cmd}" | sed 's/\x1b\[[0-9;]*m//g'
       } 2>&1 | tee /dev/tty | xclip -i -sel clipboard
     }

     export HISTCONTROL=ignoreboth
    '';
    initExtra = ''
    '';
    shellAliases = {
      ap = "ansible-playbook";
      am = "alsamixer";
      kc = "kubectl";
      nix-stray-roots = "nix-store --gc --print-roots | egrep -v '^(/nix/var|/run/\w+-system|\{memory)'";
      tf = "terraform";
      # Ensure the next command is checked as an alias when using watch.
      watch = "watch ";
      xc = "xclip -i -sel clipboard";
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
