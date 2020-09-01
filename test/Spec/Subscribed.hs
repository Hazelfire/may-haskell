{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Subscribed (spec) where


{- This module tests the subscribed endpoint. This endpoint is hit by stripe
 - when a user subscribes with the service. There are two things that happen
 - here. 
 -
 - 1. I get sent an email and
 - 2. I get added to the Subscribers group
 -
 - This endpoint also needs to test whether it actually came from stripe or not
 -}

import Test.Hspec
import qualified May.Mock as Mock
import qualified Spec.MockCalls as MockCalls
import Data.Aeson.QQ(aesonQQ)
import qualified Data.HashMap.Strict as HashMap
import qualified May.Stripe.Types              as Stripe
import qualified May.Types              as Types

unsubscribedState :: Mock.MayState
unsubscribedState = Mock.initial { Mock.stateStripeCustomers = HashMap.fromList [ ("id", Stripe.Customer "id" "sub" "to@test.com")] }


spec :: Spec
spec = 
  describe "Subscribers" $ do
    let (_, state) = MockCalls.runWebhook unsubscribedState
                      [aesonQQ| {type: "customer.subscription.created", data: {object: {customer: "id"}}} |]

    it "correctly gives user subscription" $ 
      Mock.stateHasSubscription state `shouldBe` True

    it "correctly sent email to user" $ 
      Mock.stateEmailsSent state `shouldBe` [Types.EmailRequest "to@test.com" "from@hazelfire.net" "Receipt for the service"]
       

