{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-dynamodb
, base, exceptions, graphql, hpack, hspec, jwt, lens, mtl
, serverless-haskell, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "may-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base exceptions
    graphql hspec jwt lens mtl serverless-haskell text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base exceptions
    graphql hspec jwt lens mtl serverless-haskell text
    unordered-containers
  ];
  testHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base exceptions
    graphql hspec jwt lens mtl serverless-haskell text
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/may-haskell#readme";
  license = stdenv.lib.licenses.bsd3;
}
