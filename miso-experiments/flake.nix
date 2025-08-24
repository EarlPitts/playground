{
  ## Config
  nixConfig = {

    # Miso's cachix cache
    extra-substituters = [
      "https://haskell-miso-cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];

  };

  inputs.miso.url = "github:dmjio/miso";
  inputs.nixpkgs.follows = "miso/nixpkgs";
  inputs.flake-utils.follows = "miso/flake-utils";

  outputs =
    {
      self,
      miso,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        haskellPackages = pkgs.haskell.packages.ghc9122;
        origShells = miso.devShells.${system};
        addPackages =
          shell:
          shell.overrideAttrs (drv: {
            buildInputs = drv.buildInputs ++ [
              haskellPackages.haskell-language-server
              # (pkgs.haskell-language-server.override { supportedGhcVersions = [ "912" ]; })
              haskellPackages.fourmolu
              haskellPackages.hlint
              pkgs.chromium
              pkgs.ghcid
            ];
          });
      in
      {
        devShells = {
          default = origShells.hls;
          typescript = origShells.typescript; # unchanged
          wasm = addPackages origShells.wasm;
          ghcjs = addPackages origShells.ghcjs;
          native = addPackages origShells.native;
        };
      }
    );
}
