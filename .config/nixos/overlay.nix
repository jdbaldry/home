final: prev:

with prev;
{
  cue-lsp-server = callPackage ./pkgs/cue-lsp-server { pkgs = prev; };
  complete-alias = callPackage ./pkgs/complete-alias { pkgs = prev; };
  docsonnet = callPackage ./pkgs/docsonnet { pkgs = prev; };
  faillint = callPackage ./pkgs/faillint { pkgs = prev; };
  grabpl = callPackage ./pkgs/grabpl { pkgs = prev; };
  grafana-agent-flow = callPackage ./pkgs/grafana-agent-flow { pkgs = prev; };
  jsdoc-to-markdown = callPackage ./pkgs/jsdoc-to-markdown { pkgs = prev; };
  jsonnetmod = callPackage ./pkgs/jsonnetmod { pkgs = prev; };
  jsonnet-bundler = callPackage ./pkgs/jsonnet-bundler { pkgs = prev; };
  jsonnet-lint = callPackage ./pkgs/jsonnet-lint { pkgs = prev; };
  jsonnet-mode = callPackage ./pkgs/jsonnet-mode { pkgs = prev; };
  misspell = callPackage ./pkgs/misspell { pkgs = prev; };
  mixtool = callPackage ./pkgs/mixtool { pkgs = prev; };
}
