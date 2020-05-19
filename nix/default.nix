let
  hackGet = p: let thunk = p + /thunk.nix; in if builtins.pathExists thunk then import thunk else p;
in
{ nixpkgsPath ? hackGet dep/nixpkgs }:
let
  ghcjs = import nixpkgsPath {
    crossSystem = (import (nixpkgsPath + "/lib")).systems.examples.ghcjs;
  };

  ghcjsSource = builtins.filterSource (path: type:
    !(builtins.elem path [
      ./.
      ../release.nix
    ])
    &&
    !(builtins.elem (builtins.baseNameOf path) [
      ".git"
    ])
  ) ../.;
in {
  ghcjs8_6 = ghcjs.haskell.packages.ghcjs86.override (old: {
    ghc = old.ghc.override {
      ghcjsSrc = ghcjsSource;
    };
  });
}
