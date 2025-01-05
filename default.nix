{ mkDerivation, base, boxes, containers, gloss, hspec
, hspec-discover, lib, MonadRandom, mtl, optparse-applicative
, random, transformers
}:
mkDerivation {
  pname = "maze";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base boxes containers hspec MonadRandom mtl random transformers
  ];
  executableHaskellDepends = [
    base containers gloss MonadRandom optparse-applicative random
    transformers
  ];
  testHaskellDepends = [ base containers hspec transformers ];
  testToolDepends = [ hspec-discover ];
  description = "Generates mazes randomly";
  license = lib.licenses.bsd2;
  mainProgram = "maze";
}
