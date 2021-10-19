{
  description = "A flake for personal Nix packages";

  outputs = { self }: {
    overlay = import ./overlay.nix;
  };
}
