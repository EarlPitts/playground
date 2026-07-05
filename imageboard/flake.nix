{
  description = "Haskell flake";

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
        packages.default = pkgs.haskell.lib.justStaticExecutables (
          haskellPackages.callCabal2nix "imageboard" ./. { }
        );

        devShells.default = pkgs.mkShell {
          packages = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            cabal-fmt
            pkgs.sqlite
            pkgs.pkg-config
          ];

          buildInputs = [
            pkgs.libpq
            pkgs.zlib
          ];
        };
      }
    );
}
