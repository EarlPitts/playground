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
        haskellPackages = pkgs.haskell.packages.ghc910;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # Haskell
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.cabal-fmt

            # Scala
            openjdk
            sbt
            coursier

            # Misc
            sqlite
          ];

          buildInputs = [
            pkgs.zlib
          ];
        };
      }
    );
}
