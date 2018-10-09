{ nixpkgs ? import <nixpkgs> {}
}:
nixpkgs.pkgs.haskell.packages.ghc843.callPackage ./very-serious-gopher-server.nix {}
