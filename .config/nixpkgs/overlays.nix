[(
  self: super:
  let
    callPackage = self.callPackage;
  in {
    complete-alias = callPackage ./complete-alias.nix { };
    cm = callPackage ./cm.nix { };
  }
)]
