{
  description = "NixOS configuration";

  inputs = {
    # Match revisions from a successful build in Hydra.
    # https://hydra.nixos.org/job/mobile-nixos/unstable/device.pine64-pinephone.x86_64-linux/all
    # Specifically: https://hydra.nixos.org/build/148247638#tabs-buildinputs
    mobile-nixos = {
      url = "github:nixos/mobile-nixos/f0aee629bcd66694be70edcdb8fbc636091f3288";
      flake = false;
    };
    nixpkgs.url = "github:nixos/nixpkgs/967d40bec14be87262b21ab901dbace23b7365db";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    jdb.url = "/home/jdb/nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs: {
    nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        inputs.nixos-hardware.nixosModules.dell-xps-13-9380
        ./configuration.nix
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jdb = import ./home.nix;
        }
      ];
      specialArgs = {
        inputs = {
          inherit (inputs) emacs-overlay home-manager jdb;
          nixpkgs = inputs.nixpkgs-unstable;
        };
      };
    };
    # See Makefile for flashing.
    nixosConfigurations.mobile = inputs.nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        (import "${inputs.mobile-nixos}/lib/configuration.nix" {
          device = "pine64-pinephone";
        })
        ./mobile.nix
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
