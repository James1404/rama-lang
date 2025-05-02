let
  pkgs = import <nixpkgs-unstable> {};
in
{
  rama = pkgs.callPackage ./package.nix { };
}
