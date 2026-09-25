{
  description = "Toolchains";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskellPackages;
        toolchain = with pkgs; [
          # Haskell
          haskell.ghc
          haskell.cabal-install
          haskell.haskell-language-server
          haskell.hlint
          haskell.cabal-fmt

          # Scala
          openjdk
          sbt
          coursier

          # Misc
          sqlite
        ];

      in
      {
        packages.default = pkgs.haskell.lib.justStaticExecutables (
          haskell.callCabal2nix "imageboard" ./imageboard { }
        );

        devShells = {
          default = pkgs.mkShell {
            packages = toolchain;
            buildInputs = [
              pkgs.zlib
            ];
          };

          opengl = pkgs.mkShell {
            packages = toolchain;
            buildInputs = [
              pkgs.zlib
              pkgs.freeglut
              pkgs.mesa
              pkgs.mesa_glu
            ];
          };
        };
      }
    );
}
