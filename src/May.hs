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


collectErrors :: [Parser a] -> Parser [a]
collectErrors lst = 
  case Either.lefts lst of
    [] -> pure $ Either.rights lst
    lefts -> Left $ Text.intercalate ", " lefts

nodesResolver :: GraphQLOut.Resolver MyIO
nodesResolver = GraphQLOut.ValueResolver nodesField $ do
  sub <- lift getSub
  let query = DynamoDB.query "MayNodeTable" & DynamoDB.qKeyConditionExpression .~ Just "user_id = :user_id" & DynamoDB.qExpressionAttributeValues .~ (HashMap.fromList [(":user_id", DynamoDB.attributeValue &  DynamoDB.avS .~ Just sub)])
  response <- AWS.send query
  case collectErrors $ List.map fromDynamoDB (response^.DynamoDB.qrsItems) :: Parser [Node] of
    Right values -> pure $ toGraphQL values
    Left err -> 
      Catch.throwM (GraphQL.ResolverException (ServerError err))

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
patchNodeResolver  = 
  let 
    (GraphQLInputType patchNodeInput decodeGraphql) = graphqlSchema
  in

  GraphQLOut.ValueResolver (GraphQLOut.Field (Just "applies the given edits to the node") patchNodeResultType (HashMap.fromList [("args", GraphQLIn.Argument (Just "arguments to patch node") patchNodeInput Nothing)])) $ do
  (GraphQL.Arguments arguments) <- asks GraphQL.arguments
  sub <- lift getSub
  let argsM = HashMap.lookup "args" arguments
  case argsM of
    Just args -> 
      case (decodeGraphql args :: Either Text [PatchCommand])of
        Right (first : rest) -> do
          let writeRequests = fmap (patchCommandToWriteRequest sub) (first :| rest)
          _ <- AWS.send (DynamoDB.batchWriteItem & DynamoDB.bwiRequestItems .~ (HashMap.fromList [("MayNodeTable", writeRequests)]))
          pure $ GraphQL.Object (HashMap.fromList [("ok", GraphQL.Boolean True)])
        Right [] ->
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
      

data PatchCommandInput = PatchCommandInput {
      patchCommandInputType :: PatchCommandType,
      patchCommandInputFolder :: Maybe FolderUpdate,
      patchCommandInputTask :: Maybe TaskUpdate,
      patchCommandInputId :: Maybe Text
      }

data PatchCommand = UpdateCommand UpdateNode | DeleteCommand Text
instance FromGraphQL PatchCommand where
  graphqlSchema =
    let (GraphQLInputType type_ decode) = graphqlSchema
    in 
     GraphQLInputType type_ $ \v -> do
        patchCommandInput <- decode v
        case patchCommandInputType patchCommandInput of
          DeleteCommandType ->  do
            id_ <- maybeToRight "delete command should have id field" (patchCommandInputId patchCommandInput)
            pure $ DeleteCommand id_
          UpdateCommandType ->
            case patchCommandInputFolder patchCommandInput of
              Just folderInput -> pure $ UpdateCommand $ UpdateFolder folderInput
              Nothing -> do
                task <- maybeToRight "update command needs either task or folder update" (patchCommandInputTask patchCommandInput)
                pure $ UpdateCommand $ UpdateTask task
              




asGqlObject :: GraphQL.Value -> Maybe (HashMap.HashMap Text GraphQL.Value)
asGqlObject (GraphQL.Object obj) = Just obj
asGqlObject _ = Nothing

prefixError :: Text -> Parser a -> Parser a
prefixError prefix (Left err) = Left $ Text.concat [prefix,": ",err]
prefixError _ a = a

data PatchCommandType = UpdateCommandType | DeleteCommandType

instance FromGraphQL PatchCommandType where
  graphqlSchema = 
     let type_ = GraphQLIn.NonNullEnumType (GraphQL.EnumType "PatchCommandType" (Just "The type of the command, either update or delete") (HashMap.fromList [("DELETE", GraphQL.EnumValue (Just "DELETE")),("UPDATE", GraphQL.EnumValue (Just "UPDATE"))]))
      in
        GraphQLInputType type_ $ \v -> 
          case v of
            (GraphQL.Enum "DELETE") -> pure DeleteCommandType
            (GraphQL.Enum "UPDATE") -> pure UpdateCommandType
            _ -> Left "Expected patch command type to be DELETE or UPDATE"

            
instance FromGraphQL PatchCommandInput where
  graphqlSchema = inputObject "PatchCommand" "a command to change or add something about the nodes" $
   PatchCommandInput <$> gqlField "type" "delete or update action"
                <*> gqlMaybeField "folder" "the folder if you wish to update a folder"
                <*> gqlMaybeField "task" "the task if you wish to update a task"
                <*> gqlMaybeField "id" "the id if you want to delete a node"
                

instance FromGraphQL a => FromGraphQL [a] where
   graphqlSchema  = 
     let 
       (GraphQLInputType childType decodeChild ) = graphqlSchema
       type_ = GraphQLIn.NonNullListType childType
      in
        GraphQLInputType type_ $ \v -> 
          case v of
            (GraphQL.List list) -> sequenceA $ List.map decodeChild list
            _ -> Left "Expected to be list"

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
                    , ("pid", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                    , ("type", DynamoDB.attributeValue & DynamoDB.avS .~ (Just "folder"))
                    ]
      

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
                      , ("pid", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                      , ("duration", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack (show duration))))
                      , ("due", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack $ show (fromRational (toRational (Time.utcTimeToPOSIXSeconds due) * 1000):: Double) )))
                    , ("type", DynamoDB.attributeValue & DynamoDB.avS .~ (Just "task"))
                      ]
     Nothing -> 
         HashMap.fromList [ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just tid))
                      , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                      , ("parent", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                      , ("duration", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack (show duration))))
                      ]

instance FromGraphQL Time.UTCTime where
  graphqlSchema = 
     let type_ = GraphQLIn.NonNullScalarType GraphQL.string
     in
        GraphQLInputType type_ $ \v -> 
          case v of
            (GraphQL.String iso8601) -> 
                 case ISO8601.parseISO8601 (Text.unpack iso8601) of
                     Just result -> pure $ result
                     Nothing -> Left $ "Posixtime must be valid ISO8601"
            _ ->  Left "Posixtime must be string (IS08601)"
   

data GraphQLInputType a = GraphQLInputType GraphQLIn.Type (GraphQL.Value -> Parser a)

class FromGraphQL a where
  graphqlSchema :: GraphQLInputType a

instance FromGraphQL Text where
  graphqlSchema = 
     let type_ = GraphQLIn.NonNullScalarType GraphQL.string
      in
        GraphQLInputType type_ $ \v -> 
          case v of
            (GraphQL.String st) -> pure st
            _ -> Left "Expected to be string"

instance FromGraphQL Double where
   graphqlSchema =
     let type_ = GraphQLIn.NonNullScalarType GraphQL.float
     in
       GraphQLInputType type_ $ \v ->
          case v of
            (GraphQL.Float dbl) -> pure dbl
            _ -> Left "Expected to be a double"

instance FromGraphQL Int32 where
   graphqlSchema =
     let type_ = GraphQLIn.NonNullScalarType GraphQL.int
     in
       GraphQLInputType type_ $ \v ->
          case v of
            (GraphQL.Int int) -> pure int
            _ -> Left "Expected to be a int"
      
gqlField :: FromGraphQL a => Text -> Text -> GraphQLInputFields a
gqlField name description = 
   let 
     (GraphQLInputType childType childDecoder)= graphqlSchema
     type_ = (name, GraphQLIn.InputField (Just description) childType Nothing)
   in
     GraphQLInputFields [type_] $ \record -> do
       object <- maybeToRight (Text.concat ["Expecting field to be object: ", name]) (asGqlObject record)
       field <- maybeToRight (Text.concat ["Expecting field ", name]) (HashMap.lookup name object)
       prefixError name $  childDecoder field

gqlMaybeField :: FromGraphQL a => Text -> Text -> GraphQLInputFields (Maybe a)
gqlMaybeField name description = 
   let 
     (GraphQLInputType childType childDecoder)= graphqlSchema
     newType = 
       case childType of 
         GraphQLIn.NonNullScalarType a -> GraphQLIn.NamedScalarType a
         GraphQLIn.NonNullInputObjectType a -> GraphQLIn.NamedInputObjectType a
         GraphQLIn.NonNullEnumType a-> GraphQLIn.NamedEnumType a
         GraphQLIn.NonNullListType a-> GraphQLIn.ListType a 
         a -> a
     type_ = (name, GraphQLIn.InputField (Just description) newType Nothing)
   in
     GraphQLInputFields [type_] $ \record -> do
       object <- maybeToRight (Text.concat ["Expecting field to be object: ", name]) (asGqlObject record)
       case HashMap.lookup name object of
         Just a -> 
           prefixError name $ Just <$> (childDecoder a)
         Nothing -> 
           prefixError name $ pure $ Nothing
           
     

data GraphQLInputFields a = GraphQLInputFields [(Text, GraphQLIn.InputField)] (GraphQL.Value -> Parser a)

instance Functor GraphQLInputFields where
   fmap mapfunc (GraphQLInputFields type_ func) = GraphQLInputFields type_ (\v -> mapfunc <$> func v )

instance Applicative GraphQLInputFields where
   (GraphQLInputFields type1_ func) <*> (GraphQLInputFields type2_ val) = GraphQLInputFields (type1_ ++ type2_) $ (\v -> func v <*> val v )
   pure a = GraphQLInputFields [] (const . pure $ a)

instance FromGraphQL FolderUpdate where
  graphqlSchema = inputObject "FolderUpdate" "an update to a folder" $
    FolderUpdate <$> gqlField "id" "the id of the folder you want to change"
                 <*> gqlField "name" "change the name of the folder"
                 <*> gqlField "parent" "change the parent of the folder (move the folder)"

instance FromGraphQL TaskUpdate where
  graphqlSchema = inputObject "TaskUpdate" "an update to a task" $
    TaskUpdate <$> gqlField "id" "the id of the task you want to change"
               <*> gqlField "name" "change the name of the task"
               <*> gqlField "parent" "change the parent of the task (move the task)"
               <*> gqlField "duration" "change the duration of the task"
               <*> gqlMaybeField "due" "change the due date of the task"

inputObject :: Text -> Text -> GraphQLInputFields a -> GraphQLInputType a
inputObject name description (GraphQLInputFields fields decoder) =
  let 
    type_ = GraphQLIn.NonNullInputObjectType $ GraphQLIn.InputObjectType name (Just description) (HashMap.fromList fields)
  in GraphQLInputType type_ decoder


patchNodeResultType :: GraphQLOut.Type MyIO
patchNodeResultType = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType "PathNodeResponse" Nothing [] (HashMap.fromList [("ok", fieldResolver "ok" "Whether the patch suceeded" (GraphQLOut.NonNullScalarType GraphQL.boolean))])
