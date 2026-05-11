{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "hs-json-parser";

          packages = [
            pkgs.cabal-install
            pkgs.haskell.compiler.ghc9103
            pkgs.hlint
          ];

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name)\n$PS1"

            alias b='cabal build'
            alias t='cabal test'
            alias l='hlint src tests'
          '';
        };
      }
    );
}
