{ pkgs ? import <nixpkgs> {config.allowBroken=true;} }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          graphql =
            haskellPackagesNew.callPackage ./nix/graphql.nix { };

          may-haskell =
            haskellPackagesNew.callPackage ./may-haskell.nix { };
        };
      };
    };
  };

  mypkgs = import <nixpkgs> { inherit config; };
in
mypkgs.haskellPackages.may-haskell
