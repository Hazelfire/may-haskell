{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module May
    ( handler
    , noGraphQLError
    , invalidGraphQLError
    ) where
    
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified AWSLambda.Events.APIGateway as APIGateway
import Data.Aeson.Embedded (Embedded)
import Control.Lens ((&), (.~), (^.))
import qualified Control.Lens as Lens
import qualified Language.GraphQL as GraphQL
import qualified Language.GraphQL.Type.Schema as GraphQL hiding (EnumType, ScalarType)
import qualified Language.GraphQL.Type as GraphQL
import qualified Language.GraphQL.Error as GraphQL
import qualified Language.GraphQL.Type.Out as GraphQLOut
import qualified Language.GraphQL.Type.In as GraphQLIn
import qualified May.Query as Query
import May.Query (Query)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Reader (ReaderT, runReaderT, asks,lift)
import qualified Network.AWS.DynamoDB.Query as DynamoDB
import qualified Network.AWS.DynamoDB.BatchWriteItem as DynamoDB
import qualified Network.AWS.DynamoDB.Types as DynamoDB
import qualified Network.AWS as AWS
import qualified Control.Monad.Trans.AWS as AWS hiding (send)
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Control.Exception.Base as Exception
import qualified Control.Monad.Catch as Catch
import Data.Int (Int32)
import Network.AWS.Prelude (NonEmpty((:|)))
import qualified Data.Time.ISO8601 as ISO8601
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time

handler :: APIGateway.APIGatewayProxyRequest (Embedded Query) -> IO (APIGateway.APIGatewayProxyResponse (Embedded Aeson.Value))
handler evt = do
  let body =  evt^.APIGateway.requestBodyEmbedded
  print evt
  case body of
    Just query -> do
      env  <- AWS.newEnv AWS.Discover
      let subV = (HashMap.lookup "sub" . Lens.view APIGateway.aClaims) =<< (evt^.(APIGateway.agprqRequestContext . APIGateway.prcAuthorizer))
          sub = 
            case subV of 
              (Just (Aeson.String a)) -> Just a
              _  -> Nothing 
      response <- (AWS.runResourceT $ AWS.runAWST env $ (runReaderT 
          ((GraphQL.graphql schema (Query.queryText query))) (Context sub)))
          
      case response of
        Right json ->
          pure $ APIGateway.responseOK & APIGateway.responseBodyEmbedded .~ Just (Aeson.Object json)
        Left _ -> 
          pure $ APIGateway.responseBadRequest & APIGateway.responseBodyEmbedded .~ Just (Aeson.object ["error" .= invalidGraphQLError])
    Nothing ->
      pure $ APIGateway.responseBadRequest & APIGateway.responseBodyEmbedded .~ Just (Aeson.object ["error" .= noGraphQLError ])

data Context = Context { contextSub :: Maybe Text }

type MyIO = ReaderT Context AWS.AWS


meType :: GraphQLOut.Type MyIO
meType = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType "Me" Nothing [] (HashMap.fromList [("nodes", nodesResolver)])

data Node = TaskNode Task
          | FolderNode Folder

instance ToGraphQL Node where
  toGraphQL (TaskNode task) = toGraphQL task
  toGraphQL (FolderNode folder) = toGraphQL folder
  

data Task = Task
  { taskId :: Text
  , taskName :: Text
  , taskParent :: Text
  , taskDuration :: Double
  , taskDue :: Maybe Time.UTCTime
  }

data Folder = Folder 
  { folderId :: Text
  , folderName :: Text
  , folderParent :: Text
  }

instance FromDynamoDBRecord Folder where
  fromDynamoDB record = do
    fid <- stringField record "node_id"
    name <- stringField record "name"
    parent <- stringField record "pid"
    pure $ Folder fid name parent

instance ToGraphQL Folder where
  toGraphQL (Folder{folderId=fid,folderName=name,folderParent=parent}) = GraphQL.Object (HashMap.fromList 
                [ ("id", GraphQL.String fid)
                , ("name", GraphQL.String name)
                , ("parent", GraphQL.String parent)
                , ("__typename", GraphQL.String "Folder")
                ]
                )
    
    
type Parser a = Either Text a

class FromDynamoDBRecord a where
   fromDynamoDB :: HashMap.HashMap Text DynamoDB.AttributeValue -> Parser a

instance FromDynamoDBRecord Node where
   fromDynamoDB record = do
     gqlType <- maybeToRight "should have type field" (Lens.view DynamoDB.avS =<<HashMap.lookup "type" record)
     case gqlType of
        "task" -> TaskNode <$> fromDynamoDB record
        "folder" -> FolderNode <$> fromDynamoDB record
        _ -> Left "expecting node type to be task or folder"

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a _ = Left a

numberField :: HashMap.HashMap Text DynamoDB.AttributeValue -> Text -> Parser Double
numberField record name =  do 
  attributeValue <- maybeToRight (Text.concat ["should have ", name, " field"]) (HashMap.lookup name record)
  value <- maybeToRight (Text.concat [name, "should be a number"]) (attributeValue^.DynamoDB.avN)
  number <- maybeToRight (Text.concat [name, "should parse as a number (should never ever fail)"]) (readMaybe (Text.unpack value))
  pure number


stringField :: HashMap.HashMap Text DynamoDB.AttributeValue -> Text -> Parser Text
stringField record name = do
  attributeValue <- maybeToRight (Text.concat ["should have ", name, " field"]) (HashMap.lookup name record)
  value <- maybeToRight (Text.concat [name, "should be a string"]) (attributeValue^.DynamoDB.avS)
  pure value

maybeField :: Parser a -> Parser (Maybe a)
maybeField (Right a) = Right (Just a)
maybeField (Left _) = Right Nothing

instance FromDynamoDBRecord Task where
   fromDynamoDB record = do
     tid <-  stringField record "node_id"
     name <-  stringField record "name"
     duration <- numberField record "duration"
     pid <- stringField record "pid"
     dueMilis <- maybeField (numberField record "due")
     let due = Time.posixSecondsToUTCTime . fromInteger . floor .(/1000) <$> dueMilis
     pure $ Task tid name pid duration due

class ToGraphQL a where
   toGraphQL ::  a -> GraphQL.Value

instance ToGraphQL Task where
  toGraphQL (Task{taskId=tid,taskName=name,taskParent=pid,taskDuration=duration,taskDue=dueM}) = GraphQL.Object (HashMap.fromList (
    case dueM of 
      Just due -> 
        [("__typename", GraphQL.String "Task")
        , ("name", GraphQL.String name)
        , ("duration", GraphQL.Float duration)
        , ("id", GraphQL.String tid)
        , ("due", GraphQL.String . Text.pack $ ISO8601.formatISO8601Millis due)
        , ("parent", GraphQL.String pid)
        ]
      Nothing ->
        [("__typename", GraphQL.String "Task")
        , ("name", GraphQL.String name)
        , ("duration", GraphQL.Float duration)
        , ("parent", GraphQL.String pid)
        , ("id", GraphQL.String tid)
        ]
      ))

instance ToGraphQL a => ToGraphQL [a] where
  toGraphQL list = GraphQL.List (map toGraphQL list)


data UnauthenticatedException = UnauthenticatedException


instance Exception.Exception UnauthenticatedException where
  displayException _ = "Endpoint requires an access token" 

instance Show UnauthenticatedException where
  show _ = "Endpoint requires an access token" 

data ServerError = ServerError Text

data InvalidArgsError = InvalidArgsError Text

instance Show InvalidArgsError where
  show (InvalidArgsError err) = "Invalid Arguments error: " ++ Text.unpack err

instance Exception.Exception ServerError where
  displayException _ = "This should never occur" 

instance Show ServerError where
  show (ServerError err) = "Server Error: "  ++ Text.unpack err

instance Exception.Exception InvalidArgsError where
  displayException _ = "Invalid arguments"


nodesResolver :: GraphQLOut.Resolver MyIO
nodesResolver = GraphQLOut.ValueResolver nodesField $ do
  sub <- lift getSub
  let query = DynamoDB.query "MayNodeTable" & DynamoDB.qKeyConditionExpression .~ Just "user_id = :user_id" & DynamoDB.qExpressionAttributeValues .~ (HashMap.fromList [(":user_id", DynamoDB.attributeValue &  DynamoDB.avS .~ Just sub)])
  response <- AWS.send query
  let items = Either.rights $ List.map fromDynamoDB (response^.DynamoDB.qrsItems) :: [Node]
  pure $ toGraphQL items

getSub :: MyIO Text
getSub = do
  subM <- asks contextSub
  case subM of 
    Just sub -> pure sub
    Nothing -> 
      Catch.throwM (GraphQL.ResolverException UnauthenticatedException)
  

nodesField :: GraphQLOut.Field MyIO
nodesField = GraphQLOut.Field (Just "All the nodes of a user" ) nodesType HashMap.empty

nodesType :: GraphQLOut.Type MyIO
nodesType = GraphQLOut.NonNullListType nodeType

nodeType :: GraphQLOut.Type MyIO
nodeType  = GraphQLOut.NonNullUnionType (GraphQLOut.UnionType "Node" (Just "a node (folder or task)") [taskType, folderType])

folderType :: GraphQLOut.ObjectType MyIO
folderType = GraphQLOut.ObjectType "Folder" (Just "a folder") [] (HashMap.fromList 
   [ ("name", fieldResolver "name" "the name of the folder" (GraphQLOut.NonNullScalarType GraphQL.string) )
   , ("parent", fieldResolver "parent" "The id of the folder that contains this folder" (GraphQLOut.NonNullScalarType GraphQL.id) )
   , ("id", fieldResolver "id" "The id of this folder" (GraphQLOut.NonNullScalarType GraphQL.id) )
   , ("__typename", fieldResolver "__typename" "type" (GraphQLOut.NonNullScalarType GraphQL.string))
   ])

taskType :: GraphQLOut.ObjectType MyIO
taskType = GraphQLOut.ObjectType "Task" (Just "a task") [] (HashMap.fromList 
   [ ("name", fieldResolver "name" "the name of the task" (GraphQLOut.NonNullScalarType GraphQL.string) )
   , ("id", fieldResolver "id" "the unique id of the task" (GraphQLOut.NonNullScalarType GraphQL.string) )
   , ("due", fieldResolver "due" "the timestamp of when the task is due" (GraphQLOut.NamedScalarType GraphQL.string) )
   , ("parent", fieldResolver "parent" "The id of the folder that contains that task" (GraphQLOut.NonNullScalarType GraphQL.id) )
   , ("duration", fieldResolver "duration" "the duration of the task in hours" (GraphQLOut.NonNullScalarType GraphQL.float) )
   , ("__typename", fieldResolver "__typename" "type" (GraphQLOut.NonNullScalarType GraphQL.string))
   ])



fieldResolver :: Text -> Text -> GraphQLOut.Type MyIO -> GraphQLOut.Resolver MyIO
fieldResolver name description graphqlType = 
  GraphQLOut.ValueResolver (GraphQLOut.Field (Just description) graphqlType HashMap.empty) $ do
  value <- asks GraphQL.values
  let object =
        case value of
          GraphQL.Object obj -> Just obj
          _ -> Nothing
  case HashMap.lookup name =<< object of
    Just val-> pure val
    _ -> 
      case graphqlType of
        GraphQLOut.NamedScalarType _ -> pure GraphQL.Null
        GraphQLOut.NamedObjectType _ -> pure GraphQL.Null
        GraphQLOut.NamedInterfaceType _ -> pure GraphQL.Null
        GraphQLOut.NamedUnionType _ -> pure GraphQL.Null
        _ -> 
          Catch.throwM (GraphQL.ResolverException (ServerError $ Text.concat ["could not resolve field ", name]))


meField :: GraphQLOut.Field MyIO
meField = GraphQLOut.Field (Just "Find information about the current user") meType HashMap.empty

meResolver :: GraphQLOut.Resolver MyIO
meResolver = GraphQLOut.ValueResolver meField $
   pure $ GraphQL.Object HashMap.empty


noGraphQLError :: Text
noGraphQLError = "This is a graphql endpoint. It requires sending graphql!"

invalidGraphQLError :: Text
invalidGraphQLError = "Invalid GraphQL"

schema :: GraphQL.Schema MyIO
schema = GraphQL.Schema (GraphQLOut.ObjectType "Query" Nothing [] resolvers ) (Just $ GraphQLOut.ObjectType "Mutation" Nothing [] mutations) Nothing

resolvers :: HashMap.HashMap Text (GraphQLOut.Resolver MyIO)
resolvers = HashMap.fromList [("me", meResolver)]

mutations :: HashMap.HashMap Text (GraphQLOut.Resolver MyIO)
mutations = HashMap.fromList [("patchNodes", patchNodeResolver)]

patchNodeResolver :: GraphQLOut.Resolver MyIO
patchNodeResolver  = GraphQLOut.ValueResolver (GraphQLOut.Field (Just "applies the given edits to the node") patchNodeResultType (HashMap.fromList [("args", patchNodeArg)])) $ do
  (GraphQL.Arguments arguments) <- asks GraphQL.arguments
  sub <- lift getSub
  let argsM = HashMap.lookup "args" arguments
  case argsM of
    Just args -> 
      case (fromGraphQL args :: Either Text [PatchCommand])of
        Right (first : rest) -> do
          let writeRequests = fmap (patchCommandToWriteRequest sub) (first :| rest)
          _ <- AWS.send (DynamoDB.batchWriteItem & DynamoDB.bwiRequestItems .~ (HashMap.fromList [("MayNodeTable", writeRequests)]))
          pure $ GraphQL.Object (HashMap.fromList [("ok", GraphQL.Boolean True)])
        Right [] -> do
          pure $ GraphQL.Object (HashMap.fromList [("ok", GraphQL.Boolean True)])
        Left err ->
          Catch.throwM (GraphQL.ResolverException (InvalidArgsError $ Text.concat ["could not decode patch command, ", err]))
    Nothing ->
          Catch.throwM (GraphQL.ResolverException (InvalidArgsError "no args as argument"))

dynoString :: Text -> DynamoDB.AttributeValue
dynoString string = DynamoDB.attributeValue & DynamoDB.avS .~ (Just string)

patchCommandToWriteRequest :: Text -> PatchCommand -> DynamoDB.WriteRequest
patchCommandToWriteRequest sub (UpdateCommand (UpdateFolder x)) =
   let 
      putRequest = DynamoDB.putRequest & DynamoDB.prItem .~ (HashMap.insert "user_id" (dynoString sub) (toDynamoDb x))
   in
  DynamoDB.writeRequest & DynamoDB.wrPutRequest .~ 
     (Just putRequest)
patchCommandToWriteRequest sub (UpdateCommand (UpdateTask x)) =
   let 
      putRequest = DynamoDB.putRequest & DynamoDB.prItem .~ (HashMap.insert "user_id" (dynoString sub) (toDynamoDb x))
   in
  DynamoDB.writeRequest & DynamoDB.wrPutRequest .~ 
     (Just putRequest)
patchCommandToWriteRequest sub (DeleteCommand nid) =
   let 
      deleteRequest = DynamoDB.deleteRequest & DynamoDB.drKey .~ (HashMap.fromList [("node_id", dynoString nid), ("user_id", dynoString sub)])
   in
  DynamoDB.writeRequest & DynamoDB.wrDeleteRequest .~ 
     (Just deleteRequest)
      

class FromGraphQL a where
   fromGraphQL :: GraphQL.Value -> Parser a

data PatchCommand = UpdateCommand UpdateNode | DeleteCommand Text



instance FromGraphQL Text where
   fromGraphQL (GraphQL.String obj) = pure obj
   fromGraphQL _ = Left "Expecting a text object"

instance FromGraphQL Double where
   fromGraphQL (GraphQL.Float obj) = pure obj
   fromGraphQL _ = Left "Expecting a float object"

instance FromGraphQL Int32 where
   fromGraphQL (GraphQL.Int obj) = pure obj
   fromGraphQL _ = Left "Expecting a int object"

gqlField :: FromGraphQL a => GraphQL.Value -> Text -> Parser a
gqlField record key = do
   object <- maybeToRight (Text.concat ["Expecting field to be object: ", key]) (asGqlObject record)
   field <- maybeToRight (Text.concat ["Expecting field ", key]) (HashMap.lookup key object)
   prefixError key $  fromGraphQL field

asGqlObject :: GraphQL.Value -> Maybe (HashMap.HashMap Text GraphQL.Value)
asGqlObject (GraphQL.Object obj) = Just obj
asGqlObject _ = Nothing

prefixError :: Text -> Parser a -> Parser a
prefixError prefix (Left err) = Left $ Text.concat [prefix,": ",err]
prefixError _ a = a

data PatchCommandType = UpdateCommandType | DeleteCommandType

instance FromGraphQL PatchCommandType where
  fromGraphQL (GraphQL.Enum "DELETE") = pure $ DeleteCommandType
  fromGraphQL (GraphQL.Enum "UPDATE") = pure $ UpdateCommandType
  fromGraphQL _ = Left $ "PatchCommandType must be DELETE or UPDATE"

instance FromGraphQL PatchCommand where
  fromGraphQL value = prefixError "PatchCommand" $ do
      patchType <- gqlField value "type" :: Parser PatchCommandType
      case patchType of
         DeleteCommandType-> 
            DeleteCommand <$> gqlField value "id"
         UpdateCommandType -> do
            folderM <- (maybeField $ gqlField value "folder" ) :: Parser (Maybe FolderUpdate)
            case folderM of
              Just folder ->
                pure $ UpdateCommand $ UpdateFolder folder
              Nothing ->
                UpdateCommand . UpdateTask <$> gqlField value "task"
                

instance FromGraphQL a => FromGraphQL [a] where
   fromGraphQL (GraphQL.List a) = 
      let 
         children = (List.map fromGraphQL a)
      in
        case Either.lefts children of
          [] -> Right $ Either.rights children
          b -> Left $ Text.intercalate ", " b
   fromGraphQL _ = Left "Should be list"

data UpdateNode = UpdateFolder FolderUpdate | UpdateTask TaskUpdate

class ToDynamoDb a where
   toDynamoDb :: a -> HashMap.HashMap Text DynamoDB.AttributeValue

data FolderUpdate = FolderUpdate
    { folderUpdateId :: Text
    , folderUpdateName :: Text
    , folderUpdateParent :: Text
    }

instance ToDynamoDb FolderUpdate where
   toDynamoDb (FolderUpdate{folderUpdateId=fid,folderUpdateName=name,folderUpdateParent=parent}) =
     HashMap.fromList [ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just fid))
                    , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                    , ("parent", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                    ]
      

instance FromGraphQL FolderUpdate where
  fromGraphQL value = prefixError "FolderUpdate" $ 
    FolderUpdate <$> gqlField value "id"
                 <*> gqlField value "name"
                 <*> gqlField value "parent"
     

data TaskUpdate = TaskUpdate 
    { taskUpdateId :: Text
    , taskUpdateName :: Text
    , taskUpdateParent :: Text
    , taskUpdateDuration :: Double
    , taskUpdateDue :: Maybe Time.UTCTime
    }

instance ToDynamoDb TaskUpdate where
   toDynamoDb (TaskUpdate{taskUpdateId=tid,taskUpdateName=name,taskUpdateParent=parent,taskUpdateDuration=duration,taskUpdateDue=dueM}) =
    case dueM of
     Just due -> 
       HashMap.fromList [ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just tid))
                      , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                      , ("parent", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                      , ("duration", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack (show duration))))
                      , ("due", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack $ show (fromRational (toRational (Time.utcTimeToPOSIXSeconds due) * 1000):: Double) )))
                      ]
     Nothing -> 
         HashMap.fromList [ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just tid))
                      , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                      , ("parent", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                      , ("duration", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack (show duration))))
                      ]
instance FromGraphQL TaskUpdate where
  fromGraphQL value = prefixError "TaskUpdate" $
    TaskUpdate <$> gqlField value "id"
                 <*> gqlField value "name"
                 <*> gqlField value "parent"
                 <*> gqlField value "duration"
                 <*> (maybeField (gqlField value "due"))

instance FromGraphQL Time.UTCTime where
  fromGraphQL (GraphQL.String iso8601) =
                 case ISO8601.parseISO8601 (Text.unpack iso8601) of
                     Just result -> pure $ result
                     Nothing -> Left $ "UTC time must be valid ISO8601"
  fromGraphQL _ = Left $ "UTC time must be string"

  
patchNodeArg :: GraphQLIn.Argument
patchNodeArg = GraphQLIn.Argument (Just "arguments to give to patch nodes") (GraphQLIn.NonNullListType patchCommandType) Nothing

patchCommandType :: GraphQLIn.Type
patchCommandType = GraphQLIn.NonNullInputObjectType ( GraphQLIn.InputObjectType  "PatchCommand" (Just "a command to change something about the nodes") (HashMap.fromList 
   [ ("type", GraphQLIn.InputField (Just "delete or update action") patchCommandTypeType Nothing)
   , ("id", GraphQLIn.InputField (Just "the id if the action is to delete") (GraphQLIn.NamedScalarType GraphQL.id) Nothing)
   , ("folder", GraphQLIn.InputField (Just "the folder if you wish to update a folder") (GraphQLIn.NamedInputObjectType folderInputType ) Nothing)
   , ("task", GraphQLIn.InputField (Just "the task if you wish to update a task") (GraphQLIn.NamedInputObjectType taskInputType ) Nothing)
   ]
   ))

folderInputType :: GraphQLIn.InputObjectType
folderInputType = GraphQLIn.InputObjectType "PatchFolder" (Just "an update to a folder") (HashMap.fromList
    [ ("parent", GraphQLIn.InputField (Just "change the parent (containing folder)") (GraphQLIn.NonNullScalarType GraphQL.string) Nothing)
    , ("name", GraphQLIn.InputField (Just "change the name of the folder") (GraphQLIn.NonNullScalarType GraphQL.string) Nothing)
    , ("id", GraphQLIn.InputField (Just "the id of the folder you want to change") (GraphQLIn.NonNullScalarType GraphQL.id) Nothing)
    ]
    )

taskInputType :: GraphQLIn.InputObjectType
taskInputType = GraphQLIn.InputObjectType "PatchTask" (Just "an update to a task") (HashMap.fromList
    [ ("parent", GraphQLIn.InputField (Just "change the task (containing folder)") (GraphQLIn.NonNullScalarType GraphQL.id) Nothing)
    , ("name", GraphQLIn.InputField (Just "change the name of the task") (GraphQLIn.NonNullScalarType GraphQL.string) Nothing)
    , ("id", GraphQLIn.InputField (Just "the id of the task you want to change") (GraphQLIn.NonNullScalarType GraphQL.id) Nothing)
    , ("duration", GraphQLIn.InputField (Just "change the duration of the task (in hours)") (GraphQLIn.NonNullScalarType GraphQL.float) Nothing)
    , ("due", GraphQLIn.InputField (Just "change the due date of the task (posix time in milliseconds)") (GraphQLIn.NamedScalarType GraphQL.string) Nothing)
    ]
    )

patchCommandTypeType :: GraphQLIn.Type
patchCommandTypeType = GraphQLIn.NonNullEnumType (GraphQL.EnumType "PatchCommandType" (Just "The type of the command, either update or delete") (HashMap.fromList [("DELETE", GraphQL.EnumValue (Just "DELETE")),("UPDATE", GraphQL.EnumValue (Just "UPDATE"))]))

patchNodeResultType :: GraphQLOut.Type MyIO
patchNodeResultType = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType "PathNodeResponse" Nothing [] (HashMap.fromList [("ok", fieldResolver "ok" "Whether the patch suceeded" (GraphQLOut.NonNullScalarType GraphQL.boolean))])
