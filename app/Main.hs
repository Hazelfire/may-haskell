module Main  (main) where

import AWSLambda.Events.APIGateway (apiGatewayMain)
import May

main = apiGatewayMain handler
