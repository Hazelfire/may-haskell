{-# LANGUAGE OverloadedStrings #-}

module Spec.MockCalls (runQuery, runWebhook) where

import May (handler)
import qualified May.Mock as Mock
import qualified AWSLambda.Events.APIGateway as APIGateway
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.TextValue as Aeson
import qualified Data.Aeson.Embedded as Aeson
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified May.Query as Query
import qualified Control.Lens as Lens
import Data.ByteString (ByteString)

requestIdentity :: APIGateway.RequestIdentity
requestIdentity = APIGateway.RequestIdentity Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

requestAuthorizer :: APIGateway.Authorizer
requestAuthorizer = APIGateway.Authorizer Nothing (HashMap.fromList []) HashMap.empty

requestContext :: APIGateway.ProxyRequestContext
requestContext = APIGateway.ProxyRequestContext  Nothing  "" "" "" "" requestIdentity "" "" "" "" (Just requestAuthorizer)

testRequest :: ByteString -> Maybe a -> APIGateway.APIGatewayProxyRequest a
testRequest path body = APIGateway.APIGatewayProxyRequest "" path "" [] [] HashMap.empty HashMap.empty requestContext (Aeson.TextValue <$> body)

callEndpoint :: Mock.MayState -> ByteString -> Aeson.Value -> (Maybe Aeson.Value, Mock.MayState)
callEndpoint state path body = 
  let 
     (response, finalState) = Mock.runMockMonad (handler (testRequest path (Just (Aeson.Embedded body )))) state
  in
    case response of
      Right successResponse -> 
        let responseBody = Lens.view APIGateway.agprsBody successResponse
            value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> responseBody
        in
         (value, finalState)
      Left _ -> (Nothing, finalState)

runQuery :: Mock.MayState -> Text -> (Maybe Aeson.Value, Mock.MayState)
runQuery state query = 
  callEndpoint state "/" (Aeson.toJSON (Query.Query query Nothing Nothing))


runWebhook :: Mock.MayState -> Aeson.Value -> (Maybe Aeson.Value, Mock.MayState)
runWebhook state payload =
  callEndpoint state "/stripehook" payload
