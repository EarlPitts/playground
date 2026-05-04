{
  description = "Dev stuff";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
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
        haskellPackages = pkgs.haskell.packages.ghc966;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with haskellPackages; [
            # Haskell
            ghc
            cabal-install
            haskell-language-server
            hlint
            cabal-fmt
            pkgs.zlib

            # Scala
            pkgs.sbt
            pkgs.coursier
          ];

          # Needed for some haskell packages
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.zlib
          ];

          # Set JAVA_HOME for tools that need it
          JAVA_HOME = "${pkgs.openjdk}";
        };
      }
    );
}
