{ 
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    powerline-go = pkgs.powerline-go.overrideAttrs (oldAttrs: rec {
      version = "ignore-namespace";

      src = fetchFromGitHub {
        owner = "jdbaldry";
        repo = "powerline-go";
        rev = "731179742f730fc1b4ae5620ff8470d5ca11be70";
        sha256 = "1rcidm4wpw5011c9ay3xdqywhfy0f9746czql8cawwxga2b553w9";
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
