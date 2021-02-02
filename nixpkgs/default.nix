final: prev: {
  cue-lsp-server = prev.callPackage ./cue-lsp-server { };
  complete-alias = prev.callPackage ./complete-alias { };
  grabpl = prev.callPackage ./grabpl { };
  jsonnet-bundler = prev.callPackage ./jsonnet-bundler { };
  jsonnet-mode = prev.callPackage ./jsonnet-mode { };
  misspell = prev.callPackage ./misspell { };
  tanka = prev.callPackage ./tanka { };
}
