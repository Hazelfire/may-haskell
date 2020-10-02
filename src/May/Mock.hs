{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module May.Mock
  (MockMayMonad, runMockMonad, MayState(..), initial)
   where

import qualified Control.Monad.State           as State
import qualified Control.Monad.Catch.Pure      as Catch
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                      ( Text )
import qualified May.Types                     as Types

data MayState = MayState 
  { stateNodes :: HashMap.HashMap Text Types.Node
  , stateHasSubscription :: Bool
  }

initial :: MayState
initial = MayState (HashMap.fromList []) False

newtype MockMayMonad a = MockMayMonad { runMockMonadInternal :: Catch.CatchT (State.State MayState) a }
  deriving (Monad, Applicative, Functor, Catch.MonadThrow, Catch.MonadCatch, State.MonadState MayState)

instance Types.MonadMay MockMayMonad where
  getNodes = HashMap.elems . stateNodes <$> State.get
  batchPatch commands = State.forM_ commands applyCommand
  getSubscription = stateHasSubscription <$> State.get
  deleteUser = pure True
  getSubscriptionSession = pure "session"

applyCommand :: Types.PatchCommand -> MockMayMonad ()
applyCommand (Types.DeleteCommand nodeId) = 
  State.state $ \state -> ((), state { stateNodes = HashMap.delete nodeId (stateNodes state)})
applyCommand (Types.UpdateCommand update) =
  let (nid, node) =  
       case update of
        Types.UpdateFolder (Types.FolderUpdate{Types.folderUpdateId=fid,Types.folderUpdateName=name,Types.folderUpdateParent=parent}) -> 
          (fid, Types.FolderNode $ Types.Folder fid name parent)
        Types.UpdateTask (Types.TaskUpdate{Types.taskUpdateId=tid,Types.taskUpdateName=name,Types.taskUpdateParent=parent,Types.taskUpdateDuration=duration,Types.taskUpdateDue=due,Types.taskUpdateDoneOn=doneOn})-> 
          (tid, Types.TaskNode $ Types.Task tid name parent duration due doneOn)
 in
  State.state $ \state-> ((),state { stateNodes = HashMap.insert nid node (stateNodes state)})

runMockMonad :: MockMayMonad a -> MayState -> (Either Catch.SomeException a, MayState)
runMockMonad  = 
   State.runState . Catch.runCatchT . runMockMonadInternal 
