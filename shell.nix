let
  np = import <nixpkgs> { overlays = [(import /home/matt/config/overlay.nix)];};
  ghc = np.nur.repos.mpickering.ghc.mkGhc
        { version = "8.8.0.20190424";
          url = "https://downloads.haskell.org/~ghc/8.8.1-alpha1/ghc-8.8.0.20190424-x86_64-deb8-linux.tar.xz";
          hash = "0lizyz7prlq99f0l3mcl01ks6241v87l5mpr0dqzg7rxxl23i1mh"; };
in
  np.mkShell { buildInputs = [ ghc
                               np.haskell.packages.ghc865.cabal-install
                               np.ncurses
                               np.icdiff
                               np.gist ]; }
