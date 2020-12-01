module May.Types
  ( Node(..)
  , Task(..)
  , Folder(..)
  , User(..)
  , MonadMay(..)
  , PatchCommand(..)
  , UpdateNode(..)
  , FolderUpdate(..)
  , Sub(..)
  , TaskUpdate(..)
  , EmailRequest(..)
  )
where

import           Data.Text                      ( Text )
import qualified Data.Time                     as Time
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Reader           ( ReaderT
                                                , lift
                                                )

data Node = TaskNode Task
          | FolderNode Folder

data Task = Task
  { taskId :: Text
  , taskName :: Text
  , taskParent :: Text
  , taskDuration :: Double
  , taskDue :: Maybe Time.UTCTime
  , taskDoneOn :: Maybe Time.UTCTime
  }

data User = User
  { userId :: Text
  , userName :: Text
  }

data Folder = Folder
  { folderId :: Text
  , folderName :: Text
  , folderParent :: Text
  , folderSharedWith :: Maybe [Text]
  }

class Catch.MonadCatch m => MonadMay m where
  getNodes :: m [Node]
  batchPatch :: [PatchCommand] -> m ()
  getSubscription :: m Bool
  getSubscriptionSession :: m Text
  deleteUser :: m Bool
  getUsers :: m [User]

data EmailRequest = EmailRequest
  { emailRequestTo :: Text
  , emailRequestFrom :: Text
  , emailRequestBody :: Text
  }
  deriving (Show, Eq)

newtype Sub = Sub Text

data PatchCommand = UpdateCommand UpdateNode | DeleteCommand Text

data UpdateNode = UpdateFolder FolderUpdate | UpdateTask TaskUpdate

data FolderUpdate = FolderUpdate
    { folderUpdateId :: Text
    , folderUpdateName :: Text
    , folderUpdateParent :: Text
    }



data TaskUpdate = TaskUpdate
    { taskUpdateId :: Text
    , taskUpdateName :: Text
    , taskUpdateParent :: Text
    , taskUpdateDuration :: Double
    , taskUpdateDue :: Maybe Time.UTCTime
    , taskUpdateDoneOn :: Maybe Time.UTCTime
    }

instance (MonadMay m) => MonadMay (ReaderT e m) where
  getNodes        = lift getNodes
  batchPatch      = lift . batchPatch
  getSubscription = lift getSubscription
  deleteUser             = lift deleteUser
  getSubscriptionSession = lift getSubscriptionSession
  getUsers = lift getUsers

