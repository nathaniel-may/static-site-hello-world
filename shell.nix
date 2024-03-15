# let
#   nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11";
#   pkgs = import nixpkgs { config = {}; overlays = []; };
# in

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/878ef7d9721bee9f81f8a80819f9211ad1f993da.tar.gz") {}
}:
pkgs.mkShell {
  packages = with pkgs; [
    stack
    ghc
    spago
    # npm
    # purs
    # cwebp
  ];
}