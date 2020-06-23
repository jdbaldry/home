{
  allowUnfree = true;
  packageOverrides = pkgs:
    with pkgs; rec {
      powerline-go = pkgs.powerline-go.overrideAttrs (oldAttrs: rec {
        version = "ignore-namespace";

        src = fetchFromGitHub {
          owner = "jdbaldry";
          repo = "powerline-go";
          rev = "21d2b69a10d29e09f0c836ac086b4cd689bd7fb3";
          sha256 = "18cdkii3kx80hrk4l25blmpxl66nh27f5d6gy6pb8k15fcm76d75";
        };
      });

      vscode-with-extensions = pkgs.vscode-with-extensions.override {
        vscodeExtensions = (with pkgs.vscode-extensions; [
          ms-python.python
          bbenoist.Nix
        ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "jsonnet-formatter";
            publisher = "xrc-inc";
            version = "0.3.0";
            sha256 = "019av9j3xy3rlp7f5xi7708g8b0j1d42g6azpx4agnv665g6gi47";
          }
          {
            name = "jsonnet";
            publisher = "heptio";
            version = "0.1.0";
            sha256 = "1m0iwk7qn3dhg3qwafm2xzyrcms421xd24ivrz138abj8f8ra203";
          }
          {
            name = "gitlens";
            publisher = "eamodio";
            version = "10.2.1";
            sha256 = "1bh6ws20yi757b4im5aa6zcjmsgdqxvr1rg86kfa638cd5ad1f97";
          }
          {
            name = "nixfmt-vscode";
            publisher = "brettm12345";
            version = "0.0.1";
            sha256 = "07w35c69vk1l6vipnq3qfack36qcszqxn8j3v332bl0w6m02aa7k";
          }
        ];
      };

      userPackages = pkgs.buildEnv {
        name = "user-packages";
        paths = [
          gitAndTools.diff-so-fancy
          powerline-go
          python
          python27Packages.virtualenv
          vscode-with-extensions
          zoom-us
        ];
        pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
        extraOutputsToInstall = [ "man" "doc" ];
      };
    };

}
