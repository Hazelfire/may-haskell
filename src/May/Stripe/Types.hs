{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module May.Stripe.Types (Subscription(..), Event(..), CustomerId(..), Customer(..)) where

import           Data.Text                      ( Text )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.:) )

data Event = CustomerSubscriptionCreated Subscription

instance Aeson.FromJSON Event where
  parseJSON = Aeson.withObject "Event" $ \v -> do
    data_ <- v .: "data"
    CustomerSubscriptionCreated <$> data_ .: "object"
    

data Subscription = Subscription { subscriptionCustomer :: CustomerId }

instance Aeson.FromJSON Subscription where
  parseJSON = Aeson.withObject "Subscription" $ \v -> do
    Subscription <$> v .: "customer"

newtype CustomerId = CustomerId Text
  deriving (Aeson.FromJSON, Eq)

data Customer = Customer { customerId :: Text, customerSub :: Text, customerEmail :: Text}
