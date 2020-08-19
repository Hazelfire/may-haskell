{ mkDerivation, aeson, base, graphql, hpack, hspec, lens
, serverless-haskell, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "may-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base graphql lens serverless-haskell text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base graphql lens serverless-haskell text
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base graphql hspec lens serverless-haskell text
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/may-haskell#readme";
  license = stdenv.lib.licenses.bsd3;
}
