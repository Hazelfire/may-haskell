{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Test.Hspec
import qualified Data.HashMap.Strict as HashMap
-- These two imports are actually from serverless-haskell. Don't really like how sneaky that is
import qualified May.Types as Types
import qualified May.Mock as Mock
import qualified Spec.MockCalls as MockCalls
import Data.Aeson.QQ(aesonQQ)
import Spec.Subscribed

       
emptyMayState :: Mock.MayState
emptyMayState = Mock.initial { Mock.stateHasSubscription = True }

mayStateWithThreeNodes :: Mock.MayState
mayStateWithThreeNodes = Mock.initial
  { Mock.stateHasSubscription = True
  , Mock.stateNodes           =
    (HashMap.fromList
      [ ("node1", Types.FolderNode (Types.Folder "node1" "name" "node2"))
      , ("node2", Types.FolderNode (Types.Folder "node2" "name2" "root"))
      , ( "node3"
        , Types.TaskNode
          (Types.Task "node3" "taskname" "node1" 0 Nothing Nothing)
        )
      ]
    )
  }


main :: IO ()
main = hspec $ do
  Spec.Subscribed.spec
  
  describe "May" $ do
    it "Correctly returns my nodes when queried" $ 
      let
        (result, _)= MockCalls.runQuery mayStateWithThreeNodes " { me { nodes { __typename } } } "
      in
      result `shouldBe`  (Just [aesonQQ| {data: {me: {nodes: [{__typename: "Folder"},{__typename: "Folder"},{__typename: "Task"}]}}} |])

    describe "Subscriptions" $ do
      it "Returns false for subscription when does not have one" $ 
        let
          (result, _)= MockCalls.runQuery (emptyMayState {Mock.stateHasSubscription = False}) " { me { subscription } } "
        in
        result `shouldBe`  (Just [aesonQQ| {data: {me: {subscription: false}}} |])

      it "Returns true when user has subscription" $ 
         let
            (result, _) = MockCalls.runQuery (emptyMayState { Mock.stateHasSubscription = True }) " { me { subscription } } "
          in
          result `shouldBe`  (Just [aesonQQ| {data: {me: {subscription: true}}} |])
