{
  inputs = {
    json-test-suite = {
      url = "github:nst/JSONTestSuite";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, json-test-suite }:
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

          JSON_TEST_SUITE = "${json-test-suite}/test_parsing";

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name)\n$PS1"

            alias b='cabal build'
            alias t='cabal test'
            alias l='hlint lib src tests'

            alias d='cabal haddock'
            alias do='cabal haddock --open'
          '';
        };
      }
    );
}
