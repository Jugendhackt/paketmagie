{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, stdenv
      , vector
      }:
      mkDerivation {
        pname = "PaketmagieRouting";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ aeson base bytestring containers vector ];
        description = "Routing Packages magically";
        license = stdenv.lib.licenses.unfree;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
