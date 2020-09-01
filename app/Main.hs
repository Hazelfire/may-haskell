{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main  (main) where

import           AWSLambda.Events.APIGateway    ( apiGatewayMain )
import           May
import qualified Network.AWS                   as AWS
import qualified AWSLambda.Events.APIGateway   as APIGateway
import           Data.Aeson.Embedded            ( Embedded )
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import           Control.Lens                   ( 
                                                 (^.)
                                                )
import qualified Control.Lens                  as Lens
import qualified May.ServerlessMayMonad as ServerlessMayMonad
import qualified Control.Exception
main :: IO ()
main = apiGatewayMain realHandler

catchAll :: IO a -> (Control.Exception.SomeException -> IO a) -> IO a
catchAll = Control.Exception.catch

realHandler :: APIGateway.APIGatewayProxyRequest (Embedded Aeson.Value) -> IO (APIGateway.APIGatewayProxyResponse (Embedded Aeson.Value))
realHandler evt = do
  catchAll ( do
    env  <- AWS.newEnv AWS.Discover
    let authorizer = (evt^.(APIGateway.agprqRequestContext . APIGateway.prcAuthorizer))
        subV = (HashMap.lookup "sub" . Lens.view APIGateway.aClaims) =<< authorizer
        sub = 
          case subV of 
            (Just (Aeson.String a)) -> Just a
            _  -> Nothing 
    ServerlessMayMonad.runServerlessMayMonad (ServerlessMayMonad.Context sub) env $ 
        (handler evt) 
      ) $ \err -> do
           print err
           Control.Exception.throwIO err
    

