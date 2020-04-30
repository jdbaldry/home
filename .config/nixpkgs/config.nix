{ 
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    powerline-go = pkgs.powerline-go.overrideAttrs (oldAttrs: rec {
      version = "ignore-namespace";

      src = fetchFromGitHub {
        owner = "jdbaldry";
        repo = "powerline-go";
        rev = "21d2b69a10d29e09f0c836ac086b4cd689bd7fb3";
        sha256 = "18cdkii3kx80hrk4l25blmpxl66nh27f5d6gy6pb8k15fcm76d75";
      };
    });

    userProfile = writeText "user-profile" ''
      export PATH=/run/wrappers/bin:/home/$USER/.nix-profile/bin:/etc/profiles/per-user/$USER/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/sbin:/bin:/usr/sbin:/usr/bin
    '';
    userPackages = pkgs.buildEnv {
      name = "user-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${userProfile} $out/etc/profile.d/user-profile.sh
        '')
        gitAndTools.diff-so-fancy
        powerline-go
        python
        python27Packages.virtualenv
        vscode
        zoom-us
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}
