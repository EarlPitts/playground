{
  description = "Haskell flake with OpenGL stuff";

  inputs = {
    nixpkgs.url = "nixpkgs";
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
        haskellPackages = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            with haskellPackages;
            [
              ghc
              cabal-install
              haskell-language-server
              hlint
              zlib
              # Add any system dependencies here
            ]
            ++ [
              pkgs.mesa
              pkgs.freeglut
              pkgs.mesa_glu
              pkgs.zlib
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
