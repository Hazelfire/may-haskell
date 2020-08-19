{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import May (handler, noGraphQLError, invalidGraphQLError)
import qualified May.Query as Query
import qualified Control.Lens as Lens
import Data.Text (Text)
import qualified AWSLambda.Events.APIGateway as APIGateway
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
-- These two imports are actually from serverless-haskell. Don't really like how sneaky that is
import qualified Data.Aeson.TextValue as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Embedded as Aeson
import Data.Aeson ((.=), (.:))

requestIdentity :: APIGateway.RequestIdentity
requestIdentity = APIGateway.RequestIdentity Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

requestAuthorizer :: APIGateway.Authorizer
requestAuthorizer = APIGateway.Authorizer Nothing (HashMap.fromList [("sub", Aeson.String "e1b774eb-a9d5-4add-b09c-ce4d45909381")]) HashMap.empty

requestContext :: APIGateway.ProxyRequestContext
requestContext = APIGateway.ProxyRequestContext  Nothing  "" "" "" "" requestIdentity "" "" "" "" (Just requestAuthorizer)

testRequest :: Maybe a -> APIGateway.APIGatewayProxyRequest a
testRequest body = APIGateway.APIGatewayProxyRequest "" "" "" [] [] HashMap.empty HashMap.empty requestContext (Aeson.TextValue <$> body)

main :: IO ()
main = hspec $ do
  describe "May" $ do
    it "returns an error if we pass it a request with an empty body" $ do
      result <- handler (testRequest Nothing)
      let body = Lens.view APIGateway.agprsBody result
          value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> body
      value `shouldBe`  Just (Aeson.object ["error" .= noGraphQLError])

    it "returns an error if we pass it a request with invalid GraphQL" $ do
      result <- handler (testRequest (Just (Aeson.Embedded (Query.Query "" Nothing Nothing))))
      let body = Lens.view APIGateway.agprsBody result
          value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> body
          value_data = Aeson.parseMaybe (Aeson.withObject "response" (\v -> v .: "data")) <$> value
      value_data `shouldBe`  Just (Just Aeson.Null)
    it "Correctly returns my nodes when queried" $ do
      result <- handler (testRequest (Just (Aeson.Embedded (Query.Query "{ me { nodes { __typename ... on Folder { name parent } } } }" Nothing Nothing))))
      let body = Lens.view APIGateway.agprsBody result
          value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> body
          value_data = Aeson.parseMaybe (Aeson.withObject "response" (\v -> v .: "data")) <$> value
      value_data `shouldBe`  Just (Just (Aeson.listValue id []))
