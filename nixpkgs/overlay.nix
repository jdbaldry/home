final: prev:
with prev; {
  cue-lsp-server = callPackage ./cue-lsp-server { pkgs = prev; };
  complete-alias = callPackage ./complete-alias { pkgs = prev; };
  docsonnet = callPackage ./docsonnet { pkgs = prev; };
  grabpl = callPackage ./grabpl { pkgs = prev; };
  jsonnetmod = callPackage ./jsonnetmod { pkgs = prev; };
  jsonnet-bundler = callPackage ./jsonnet-bundler { pkgs = prev; };
  jsonnet-mode = callPackage ./jsonnet-mode { pkgs = prev; };
  misspell = callPackage ./misspell { pkgs = prev; };
  tanka = callPackage ./tanka { pkgs = prev; };
}
