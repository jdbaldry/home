{ pkgs ? import <nixpkgs> {} }: 

let
  myEmacs = pkgs.emacs; 
  emacsWithPackages = (pkgs.emacsPackagesGen myEmacs).emacsWithPackages; 
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [ 
    gruber-darker-theme
    smex
  ]))
