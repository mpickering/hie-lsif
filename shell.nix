let
  np = import <nixpkgs> {};
in
  np.mkShell { buildInputs = [ np.haskell.packages.ghc863.ghc
                               np.haskell.packages.ghc863.cabal-install
                               np.icdiff
                               np.gist ]; }
