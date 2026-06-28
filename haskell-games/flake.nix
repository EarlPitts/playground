{
  description = "Haskell flake with OpenGL stuff";

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
        haskellPackages = pkgs.haskell.packages.ghc96;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            pkgs.mesa
            pkgs.freeglut
            pkgs.mesa_glu
            pkgs.zlib
            cabal-fmt
            # Add any system dependencies here
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.freeglut
            pkgs.mesa
            pkgs.mesa_glu
            pkgs.zlib
          ];
        };
      }
    );
}
