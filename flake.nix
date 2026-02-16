{
  description = "align dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell.compiler.ghc9103
            pkgs.cabal-install
            pkgs.haskell.packages.ghc9103.haskell-language-server
            pkgs.hlint
            pkgs.zlib.dev
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.zlib.out}/lib:$LD_LIBRARY_PATH"
          '';
        };
      });
}
