{
  description = "A flake for personal Nix packages";

  outputs = { self }: {
    overlay = final: prev: import ./overlay.nix final prev;
  };
}
