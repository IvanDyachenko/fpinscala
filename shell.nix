{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.stdenv.mkDerivation rec {
  name = "development-env";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs =
      [
        # Development
        pkgs.metals-emacs
      ];
}

