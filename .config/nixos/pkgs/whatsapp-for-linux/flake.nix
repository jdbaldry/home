{
  description = "An unofficial WhatsApp desktop application for Linux.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = import ./default.nix {
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    };
  };
}
