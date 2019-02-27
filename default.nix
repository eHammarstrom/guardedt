{ mkDerivation, base, ghc-prim, mtl, stdenv, transformers }:
mkDerivation {
  pname = "guardedt";
  version = "0.1.0.0";
  src = /home/estrom/dev/haskell/guardedt;
  libraryHaskellDepends = [ base ghc-prim mtl transformers ];
  testHaskellDepends = [ base transformers ];
  homepage = "github.com/ehammarstrom/guardedt";
  license = stdenv.lib.licenses.bsd3;
}
