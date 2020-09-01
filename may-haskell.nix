{ mkDerivation, aeson, aeson-qq, amazonka, amazonka-cognito-idp
, amazonka-core, amazonka-dynamodb, base, bytestring, containers
, exceptions, graphql, hpack, hspec, http-types, iso8601-time, jwt
, lens, mtl, serverless-haskell, stdenv, text, time
, unordered-containers, wai, warp, wreq
}:
mkDerivation {
  pname = "may-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq amazonka amazonka-cognito-idp amazonka-core
    amazonka-dynamodb base bytestring containers exceptions graphql
    hspec http-types iso8601-time jwt lens mtl serverless-haskell text
    time unordered-containers wai warp wreq
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq amazonka amazonka-cognito-idp amazonka-core
    amazonka-dynamodb base bytestring containers exceptions graphql
    hspec http-types iso8601-time jwt lens mtl serverless-haskell text
    time unordered-containers wai warp wreq
  ];
  testHaskellDepends = [
    aeson aeson-qq amazonka amazonka-cognito-idp amazonka-core
    amazonka-dynamodb base bytestring containers exceptions graphql
    hspec http-types iso8601-time jwt lens mtl serverless-haskell text
    time unordered-containers wai warp wreq
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/may-haskell#readme";
  license = stdenv.lib.licenses.bsd3;
}
