{ 
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
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
        python
        python2.7
        vscode
        zoom-us
        vscode
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}
