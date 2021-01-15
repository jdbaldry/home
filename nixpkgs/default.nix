final: prev: {
  cue-lsp-server = prev.callPackage ./cue-lsp-server { };
  complete-alias = prev.callPackage ./complete-alias { };
  jsonnet-bundler = prev.callPackage ./jsonnet-bundler { };
  jsonnet-mode = prev.callPackage ./jsonnet-mode { };
  tanka = prev.callPackage ./tanka { };
}
