{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-dynamodb
, base, bytestring, exceptions, graphql, hpack, hspec, http-types
, iso8601-time, jwt, lens, mtl, serverless-haskell, stdenv, text
, time, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "may-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base bytestring
    exceptions graphql hspec http-types iso8601-time jwt lens mtl
    serverless-haskell text time unordered-containers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base bytestring
    exceptions graphql hspec http-types iso8601-time jwt lens mtl
    serverless-haskell text time unordered-containers wai warp
  ];
  testHaskellDepends = [
    aeson amazonka amazonka-core amazonka-dynamodb base bytestring
    exceptions graphql hspec http-types iso8601-time jwt lens mtl
    serverless-haskell text time unordered-containers wai warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/may-haskell#readme";
  license = stdenv.lib.licenses.bsd3;
}
