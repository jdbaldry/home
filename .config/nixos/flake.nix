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
    mobile-nixos-nixpkgs.url = "github:nixos/nixpkgs/967d40bec14be87262b21ab901dbace23b7365db";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jsonnet-language-server.url = "github:grafana/jsonnet-language-server/main?dir=nix";
    kooky.url = "github:jdbaldry/kooky";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    snowball.url = "github:jdbaldry/nixpkgs-snowball";
    xinput_exporter.url = "/home/jdb/ext/jdbaldry/xinput_exporter";
  };

  outputs =
    inputs:
    let
      overlay = import ./overlay.nix;
    in
    {
      inherit overlay;

      nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
        modules = [
          inputs.nixos-hardware.nixosModules.dell-xps-13-9380
          ./configuration.nix
          inputs.home-manager.nixosModules.home-manager
          ./modules/android
          ./modules/brightness
          ./modules/dns
          ./modules/emacs
          ./modules/grafana-agent-flow
          ./modules/pipewire
          ./modules/pulseaudio
          ./modules/podman
          ./modules/sway
          ./modules/ulimit
          inputs.xinput_exporter.nixosModules.xinput_exporter
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jdb = import ./home.nix;
          }
        ];
      };
      # See Makefile for flashing.
      nixosConfigurations.mobile = inputs.mobile-nixos-nixpkgs.lib.nixosSystem {
        specialArgs = {
          inputs = with inputs; {
            nixpkgs = mobile-nixos-nixpkgs;
            mobile-nixos = mobile-nixos;
          };
        };
        system = "aarch64-linux";
        modules = [
          (import "${inputs.mobile-nixos}/lib/configuration.nix" {
            device = "pine64-pinephone";
          })
          ./mobile.nix
        ];
      };
    } //
    (inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs
        {
          inherit system;
          overlays = [ overlay ];
        };
    in
    {
      packages = {
        grafana-agent-flow = pkgs.grafana-agent-flow;
        kolide = pkgs.kolide;
      };
    }));
}
