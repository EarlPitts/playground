{
  description = "Scala development environment with sbt and Metals";

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
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            sbt
            coursier
            sqlite
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-language-server
            nodejs
          ];

          # Set JAVA_HOME for tools that need it
          JAVA_HOME = "${pkgs.openjdk}";
        };
      }
    );
}
