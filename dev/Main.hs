{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main
  )
where

import           Network.Wai.Handler.Warp       ( run )
import           Control.Monad                  ( void )
import qualified Network.Wai                    as Wai
import           Network.HTTP.Types             as HTTP
import           May                            
import           Data.ByteString                ( ByteString )
import           Data.Aeson                     ( encode
                                                , decodeStrict
                                                )
import qualified AWSLambda.Events.APIGateway as APIGateway
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TextValue as Aeson
import qualified Data.Aeson.Embedded as Aeson
import qualified Web.JWT as JWT
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified May.Mock as Mock
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Aeson.QQ(aesonQQ)
import qualified Data.IORef as IORef
import qualified May.ServerlessMayMonad as ServerlessMayMonad
import qualified Network.AWS                   as AWS


initialMayState :: Mock.MayState
initialMayState = Mock.MayState (HashMap.fromList []) False
                  
main :: IO ()
main = do
  putStrLn "Initialising database"
  putStrLn "Running backend server on port 3000"

  stateRef <- IORef.newIORef initialMayState

  void $  run 3000 ( devServer stateRef )

  putStrLn "Running frontend server on part 8080"


requestIdentity :: APIGateway.RequestIdentity
requestIdentity = APIGateway.RequestIdentity Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

requestAuthorizer :: Maybe Text -> APIGateway.Authorizer
requestAuthorizer sub = 
   let claims = 
         case sub of 
           Just s -> [("sub", Aeson.String s)] 
           Nothing -> []

   in APIGateway.Authorizer Nothing (HashMap.fromList claims) HashMap.empty

requestContext :: Maybe Text -> APIGateway.ProxyRequestContext
requestContext sub = APIGateway.ProxyRequestContext  Nothing  "" "" "" "" requestIdentity "" "" "" "" (Just (requestAuthorizer sub))

testRequest :: Maybe Text -> Maybe a -> APIGateway.APIGatewayProxyRequest (Aeson.Embedded a)
testRequest sub body = APIGateway.APIGatewayProxyRequest "" "" "" [] [] HashMap.empty HashMap.empty (requestContext sub) (Aeson.TextValue . Aeson.Embedded <$> body)

corsHeaders :: [Header]
corsHeaders = [("Access-Control-Allow-Origin", "*")]

graphqlEndpoint :: Mock.MayState -> Maybe Text -> ByteString -> IO Wai.Response
graphqlEndpoint _ sub contents = case decodeStrict contents of
  Nothing ->
    return $ Wai.responseLBS status400 corsHeaders "Could not decode graphql query JSON"
  Just body -> do
    env  <- AWS.newEnv AWS.Discover
    response <- ServerlessMayMonad.runServerlessMayMonad (ServerlessMayMonad.Context sub) env (handler (testRequest sub body)) 
    let responseBody = Lens.view APIGateway.agprsBody response
        value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> responseBody
    return $ Wai.responseLBS HTTP.status200 corsHeaders (encode value)


-- | Devserver, "Application" type for Wai
devServer :: IORef.IORef Mock.MayState -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
devServer state request respond = do
  body     <- Wai.getRequestBodyChunk request
  let method = Wai.requestMethod request
  case method of
    "OPTIONS" -> 
      respond $ Wai.responseLBS status200 [("Access-Control-Allow-Origin", "*"), ("Access-Control-Allow-Headers", "authorization,content-type"),("Access-Control-Allow-Methods", "POST,OPTIONS,GET")] ""
    _ ->
      case Wai.pathInfo request of
        ["oauth2", "authorize"] ->
          respond $ Wai.responseFile status200 corsHeaders "static/dev_authorize.html" Nothing

        ["oauth2", "token"] ->
          let 
            tokens = [aesonQQ| { id_token: #{devIdToken}, access_token: #{devAccessToken}, refresh_token: "refresh", expires_in: 999999} |]
          in
            respond $ Wai.responseLBS status200 (("Content-Type", "application/json") : corsHeaders) (Aeson.encode tokens)

        ["stripe"] ->  do
          IORef.modifyIORef state (\s -> s { Mock.stateHasSubscription = True })
          respond $ Wai.responseFile status200 corsHeaders "static/dev_stripe.html" Nothing
        _ -> do
          s <- IORef.readIORef state
          let headers = Wai.requestHeaders request
              authHeader = fmap (\(_,value) -> value ) $ List.find ((\(name, _) -> name == "Authorization")) headers
              sub = JWT.sub=<<(JWT.claims <$> (JWT.decode =<< fmap Text.decodeUtf8 authHeader))
          respond =<< graphqlEndpoint s (fmap (Text.pack . show) sub) body

devAccessToken :: Text
devAccessToken = 
  let claims = mempty { -- mempty returns a default JWTClaimsSet
        JWT.sub=JWT.stringOrURI "sub"
        , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList
                [ ("cognito:groups", Aeson.Array [])]
       }
  in
   JWT.encodeSigned (JWT.hmacSecret "test-key") mempty claims

devIdToken :: Text
devIdToken = 
  let claims = mempty { -- mempty returns a default JWTClaimsSet
        JWT.sub=JWT.stringOrURI "sub"
        , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList 
                [("email", Aeson.String "email@email.com")
                ,("name", Aeson.String "test mc test")
                ]
       }
  in
   JWT.encodeSigned (JWT.hmacSecret "test-key") mempty claims
