with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "zbc";

  buildInputs = [
    # GHC:
    haskell.packages.lts-6_7.ghc

    # Non-Haskell Dependencies:
    zlib      # For the header files.
    zlib.out  # For the .so file.
  ];

  # Work around a bug in GHC:
  # https://ghc.haskell.org/trac/ghc/ticket/11042
  shellHook = ''
    export LD_LIBRARY_PATH=${zlib.out}/lib
  '';
}
