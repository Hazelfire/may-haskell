{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Network.Wai.Handler.Warp       ( run )
import           Control.Monad                  ( void )
import qualified Network.Wai                    as Wai
import           Network.HTTP.Types             ( status200
                                                , status400
                                                , Header
                                                )
import           May                            ( handler )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8  as BS
import           Data.Aeson                     ( encode
                                                , decodeStrict
                                                )
import qualified AWSLambda.Events.APIGateway as APIGateway
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TextValue as Aeson
import qualified Data.Aeson.Embedded as Aeson
import qualified Web.JWT as JWT
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Control.Lens as Lens

main :: IO ()
main = do
  putStrLn "Initialising database"
  putStrLn "Running backend server on port 3000"


  void $ run 3000 devServer

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

graphqlEndpoint :: Maybe Text -> ByteString -> IO Wai.Response
graphqlEndpoint sub contents = case decodeStrict contents of
  Nothing ->
    return $ Wai.responseLBS status400 corsHeaders "Could not decode graphql query JSON"
  Just body -> do
    response <- handler (testRequest sub body)
    let responseBody = Lens.view APIGateway.agprsBody response
        value = Lens.view (Aeson.unTextValue . Aeson.unEmbed) <$> responseBody
    return $ Wai.responseLBS status200 corsHeaders (encode value)

-- | Devserver, "Application" type for Wai
devServer :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
devServer request respond = do
  body     <- Wai.getRequestBodyChunk request
  let method = Wai.requestMethod request
  case method of
    "OPTIONS" -> 
      respond $ Wai.responseLBS status200 [("Access-Control-Allow-Origin", "*"), ("Access-Control-Allow-Headers", "authorization,content-type"),("Access-Control-Allow-Methods", "POST,OPTIONS")] ""
    _ ->  do
      let headers = Wai.requestHeaders request
          authHeader = fmap (\(_,value) -> value ) $ List.find ((\(name, _) -> name == "Authorization")) headers
          sub = JWT.sub=<<(JWT.claims <$> (JWT.decode =<< fmap Text.decodeUtf8 authHeader))
      print request
      response <- graphqlEndpoint (fmap (Text.pack . show) sub) body
      respond response
