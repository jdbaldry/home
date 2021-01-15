{
  description = "A flake for personal Nix packages";

  outputs = { self }: {
    overlay = final: prev: import ./default.nix final prev;
  };
}
