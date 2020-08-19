{-# LANGUAGE OverloadedStrings #-}

module May.Query
    ( Query(..)
    ) where
import qualified Data.Aeson as Aeson 
import Data.Text (Text)
import Data.Aeson ((.:?),(.:), (.=))
import Language.GraphQL.Type (Schema)

data Query = Query
  { queryText :: Text
  , queryOperationName :: Maybe Text
  , queryVariables :: Maybe Aeson.Value
  } deriving (Show, Eq)

instance Aeson.FromJSON Query where
  parseJSON = Aeson.withObject "Query" $ \v ->
    Query <$> v .: "query" <*> v .:? "operationName" <*> v .:? "variables"

instance Aeson.ToJSON Query where
  toJSON (Query query operationName variables) = Aeson.object
    [ "query" .= query
    , "operationName" .= operationName
    , "variables" .= variables
    ]
