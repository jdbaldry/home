{
  accounts.email.accounts = {
    fastmail = {
      primary = true;

      address = "jdbaldry@fastmail.com";
      imap.host = "imap.fastmail.com";
      smtp.host = "smtp.fastmail.com";
      passwordCommand = "pass show smtp.fastmail.com:465/jdbaldry@fastmail.com";
      userName = "jdbaldry@fastmail.com";

      mbsync = {
        enable = true;
        create = "maildir";
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
      passwordCommand = "pass show smtp.gmail.com:465/jdbaldry@gmail.com";
      userName = "jdbaldry@gmail.com";

      mbsync = {
        enable = true;
        create = "maildir";
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
      passwordCommand = "pass show smtp.gmail.com:465/jack.baldry@grafana.com";
      userName = "jack.baldry@grafana.com";

      mbsync = {
        enable = true;
        create = "maildir";
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

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.mu.enable = true;
}
