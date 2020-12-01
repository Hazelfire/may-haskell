{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module May
  ( handler
  , noGraphQLError
  , invalidGraphQLError
  , GraphQLOutType(..)
  , Parser
  , graphqlOutSchema
  , ServerError(..)
  )
where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import qualified AWSLambda.Events.APIGateway   as APIGateway
import           Data.Aeson.Embedded            ( Embedded )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import qualified Language.GraphQL              as GraphQL
import qualified Language.GraphQL.Type.Schema  as GraphQL
                                         hiding ( EnumType
                                                , ScalarType
                                                )
import qualified Language.GraphQL.Type         as GraphQL
import qualified Language.GraphQL.Error        as GraphQL
import qualified Language.GraphQL.Type.Out     as GraphQLOut
import qualified Language.GraphQL.Type.In      as GraphQLIn
import qualified May.Query                     as Query
import           May.Query                      ( Query )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.List                     as List
import qualified Control.Exception.Base        as Exception
import qualified Control.Monad.Catch           as Catch
import           Data.Int                       ( Int32 )
import qualified Data.Time.ISO8601             as ISO8601
import qualified Data.Time                     as Time
import           Control.Monad.Reader           ( asks
                                                , ReaderT
                                                )
import qualified May.Types                     as Types
import           May.Errors (ServerError(..))

handler
  :: Types.MonadMay m
  => APIGateway.APIGatewayProxyRequest (Embedded Aeson.Value)
  -> m (APIGateway.APIGatewayProxyResponse (Embedded Aeson.Value))
handler evt = do
  let body = evt ^. APIGateway.requestBodyEmbedded
  case evt ^. APIGateway.agprqPath of
    "/" -> 
      case fmap (Aeson.fromJSON) body of
        Just (Aeson.Success query) -> 
          graphqlHandler query
        _ ->
          pure
            $  APIGateway.responseBadRequest
            &  APIGateway.responseBodyEmbedded
            .~ Just (Aeson.object ["error" .= noGraphQLError])
    _ ->
        pure
          $  APIGateway.responseBadRequest
          &  APIGateway.responseBodyEmbedded
          .~ Just (Aeson.object ["error" .= ("Could not find url"::Text)])
      
             
           


graphqlHandler :: Types.MonadMay m
  => Query
  -> m (APIGateway.APIGatewayProxyResponse (Embedded Aeson.Value))
graphqlHandler query = do
      response <- GraphQL.graphql schema (Query.queryText query)
      case response of
        Right json ->
          pure $ APIGateway.responseOK & APIGateway.responseBodyEmbedded .~ Just
            (Aeson.Object json)
        Left _ ->
          pure
            $  APIGateway.responseBadRequest
            &  APIGateway.responseBodyEmbedded
            .~ Just (Aeson.object ["error" .= invalidGraphQLError])

meType :: Types.MonadMay m => GraphQLOut.Type m
meType = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType "Me" Nothing []
  (HashMap.fromList
    [("nodes", nodesResolver), ("subscription", subscriptionResolver)]
  )

instance ToGraphQL Types.Task where
  graphqlOutSchema =
    outputObject "Task" "a task node"
      $ [ gqlOutField "id"   "the id of the task"   Types.taskId
        , gqlOutField "name" "the name of the task" Types.taskName
        , gqlOutField "parent"
                      "the parent of the task (containing folder)"
                      Types.taskParent
        , gqlOutField "duration"
                      "the duration of the task in hours"
                      Types.taskDuration
        , gqlOutField "due" "the duration of the task in hours" Types.taskDue
        , gqlOutField "doneOn" "when the task was completed" Types.taskDoneOn
        ]

instance ToGraphQL Types.User where
  graphqlOutSchema =
    outputObject "User" "a user node"
      $ [ gqlOutField "id"   "the id of the user"   Types.userId
        , gqlOutField "name" "the name of the user" Types.userName
        ]

type Parser a = Either Text a

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a _        = Left a






data GraphQLOutType m a = GraphQLOutType (GraphQLOut.Type m) (a -> GraphQL.Value)

data GraphQLOutField m a = GraphQLOutField (Text, GraphQL.Resolver m) (a -> GraphQL.Value)


class ToGraphQL a where
  graphqlOutSchema :: Catch.MonadThrow m => GraphQLOutType m a

instance ToGraphQL Text where
  graphqlOutSchema =
    GraphQLOutType (GraphQLOut.NonNullScalarType GraphQL.string) GraphQL.String

instance ToGraphQL Bool where
  graphqlOutSchema = GraphQLOutType
    (GraphQLOut.NonNullScalarType GraphQL.boolean)
    GraphQL.Boolean

instance ToGraphQL Double where
  graphqlOutSchema =
    GraphQLOutType (GraphQLOut.NonNullScalarType GraphQL.float) GraphQL.Float

instance ToGraphQL Time.UTCTime where
  graphqlOutSchema = GraphQLOutType
    (GraphQLOut.NonNullScalarType GraphQL.string)
    (GraphQL.String . Text.pack . ISO8601.formatISO8601)

instance ToGraphQL a => ToGraphQL [a] where
  graphqlOutSchema =
    let (GraphQLOutType childType encode) = graphqlOutSchema
    in  GraphQLOutType (GraphQLOut.NonNullListType childType)
                       (GraphQL.List . fmap encode)

instance ToGraphQL a => ToGraphQL (Maybe a) where
  graphqlOutSchema =
    let (GraphQLOutType childType encode) = graphqlOutSchema
        newEncode                         = \case
          Just val -> encode val
          Nothing  -> GraphQL.Null
        newType = case childType of
          GraphQLOut.NonNullScalarType a -> GraphQLOut.NamedScalarType a
          GraphQLOut.NonNullObjectType a -> GraphQLOut.NamedObjectType a
          GraphQLOut.NonNullListType   a -> GraphQLOut.ListType a
          GraphQLOut.NamedUnionType    a -> GraphQLOut.NamedUnionType a
          a                              -> a
    in  GraphQLOutType newType newEncode

instance ToGraphQL Types.Folder where
  graphqlOutSchema =
    outputObject "Folder" "a folder node"
      $ [ gqlOutField "id"   "the id of the folder"   Types.folderId
        , gqlOutField "name" "the name of the folder" Types.folderName
        , gqlOutField "parent"
                      "the parent of the folder (containing folder)"
                      Types.folderParent
        , gqlOutField "shareId"
                      "the shareId of the folder"
                      Types.folderShareId
        ]

instance ToGraphQL Types.Node where
  graphqlOutSchema =
    let
      -- This is not total, but if this fails, I want the system to crash because it never should
        (GraphQLOutType (GraphQLOut.NonNullObjectType folderType_) encodeFolder)
          = graphqlOutSchema
        (GraphQLOutType (GraphQLOut.NonNullObjectType taskType_) encodeTask)
          = graphqlOutSchema
        type_ = GraphQLOut.NonNullUnionType
          (GraphQLOut.UnionType "Node"
                                (Just "a node (folder or task)")
                                [taskType_, folderType_]
          )
    in  GraphQLOutType type_ $ \case
          Types.FolderNode folder -> encodeFolder folder
          Types.TaskNode   task   -> encodeTask task

gqlOutField
  :: (Catch.MonadThrow m, ToGraphQL b)
  => Text
  -> Text
  -> (a -> b)
  -> GraphQLOutField m a
gqlOutField name description getter =
  let (GraphQLOutType childType encode) = graphqlOutSchema
      resolve                           = do
        value <- asks GraphQL.values
        let object = case value of
              GraphQL.Object obj -> Just obj
              _                  -> Nothing
        case HashMap.lookup name =<< object of
          Just val -> pure val
          _        -> case childType of
            GraphQLOut.NamedScalarType    _ -> pure GraphQL.Null
            GraphQLOut.NamedObjectType    _ -> pure GraphQL.Null
            GraphQLOut.NamedInterfaceType _ -> pure GraphQL.Null
            GraphQLOut.NamedUnionType     _ -> pure GraphQL.Null
            _                               -> Catch.throwM
              (GraphQL.ResolverException
                (ServerError $ Text.concat ["could not resolve field ", name])
              )
  in  GraphQLOutField
        ( name
        , GraphQLOut.ValueResolver
          (GraphQLOut.Field (Just description) childType HashMap.empty)
          resolve
        )
        (encode . getter)

graphOutFieldConvert :: a -> GraphQLOutField m a -> (Text, GraphQL.Value)
graphOutFieldConvert val (GraphQLOutField (name, _) encode) =
  (name, encode val)

outputObject
  :: Catch.MonadThrow m
  => Text
  -> Text
  -> [GraphQLOutField m a]
  -> GraphQLOutType m a
outputObject name description fields =
  let newFields = gqlOutField "__typename" "type" (const name) : fields
      newEncode = \obj -> GraphQL.Object
        (HashMap.fromList (List.map (graphOutFieldConvert obj) newFields))
      type_ = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType
        name
        (Just description)
        []
        (HashMap.fromList
          (List.map (\(GraphQLOutField childType _) -> childType) newFields)
        )
  in  GraphQLOutType type_ newEncode







data InvalidArgsError = InvalidArgsError Text

instance Show InvalidArgsError where
  show (InvalidArgsError err) = "Invalid Arguments error: " ++ Text.unpack err

instance Exception.Exception InvalidArgsError where
  displayException _ = "Invalid arguments"


wrapResolver
  :: (Catch.MonadCatch m, ToGraphQL a)
  => Text
  -> (ReaderT GraphQLOut.Context m) a
  -> GraphQLOut.Resolver m
wrapResolver description monad =
  let (GraphQLOutType type_ encoder) = graphqlOutSchema
  in  GraphQLOut.ValueResolver
          (GraphQLOut.Field (Just description) type_ HashMap.empty)
        $   encoder
        <$> monad

nodesResolver :: Types.MonadMay m => GraphQLOut.Resolver m
nodesResolver = wrapResolver "all the nodes of a user" Types.getNodes

subscriptionResolver :: Types.MonadMay m => GraphQLOut.Resolver m
subscriptionResolver =
  wrapResolver "whether the user has a subscription" $
  Types.getSubscription



fieldResolver
  :: Types.MonadMay m
  => Text
  -> Text
  -> GraphQLOut.Type m
  -> GraphQLOut.Resolver m
fieldResolver name description graphqlType =
  GraphQLOut.ValueResolver
      (GraphQLOut.Field (Just description) graphqlType HashMap.empty)
    $ do
        value <- asks GraphQL.values
        let object = case value of
              GraphQL.Object obj -> Just obj
              _                  -> Nothing
        case HashMap.lookup name =<< object of
          Just val -> pure val
          _        -> case graphqlType of
            GraphQLOut.NamedScalarType    _ -> pure GraphQL.Null
            GraphQLOut.NamedObjectType    _ -> pure GraphQL.Null
            GraphQLOut.NamedInterfaceType _ -> pure GraphQL.Null
            GraphQLOut.NamedUnionType     _ -> pure GraphQL.Null
            _                               -> Catch.throwM
              (GraphQL.ResolverException
                (ServerError $ Text.concat ["could not resolve field ", name])
              )


meField :: Types.MonadMay m => GraphQLOut.Field m
meField = GraphQLOut.Field (Just "Find information about the current user")
                           meType
                           HashMap.empty

meResolver :: Types.MonadMay m => GraphQLOut.Resolver m
meResolver =
  GraphQLOut.ValueResolver meField $ pure $ GraphQL.Object HashMap.empty


noGraphQLError :: Text
noGraphQLError = "This is a graphql endpoint. It requires sending graphql!"

invalidGraphQLError :: Text
invalidGraphQLError = "Invalid GraphQL"

schema :: Types.MonadMay m => GraphQL.Schema m
schema = GraphQL.Schema
  (GraphQLOut.ObjectType "Query" Nothing [] resolvers)
  (Just $ GraphQLOut.ObjectType "Mutation" Nothing [] mutations)
  Nothing

resolvers :: Types.MonadMay m => HashMap.HashMap Text (GraphQLOut.Resolver m)
resolvers = HashMap.fromList [("me", meResolver), ("users", usersResolver)]

usersResolver :: Types.MonadMay m => GraphQLOut.Resolver m
usersResolver = wrapResolver "gets all users" Types.getUsers

mutations :: Types.MonadMay m => HashMap.HashMap Text (GraphQLOut.Resolver m)
mutations = HashMap.fromList
  [ ("patchNodes"                , patchNodeResolver)
  , ("deleteUser"                , deleteUserResolver)
  , ("requestSubscriptionSession", requestSubscriptionSessionResolver)
  ]

deleteUserResolver :: Types.MonadMay m => GraphQLOut.Resolver m
deleteUserResolver =
  wrapResolver "deletes the requesting user" $ do
    nodes <- Types.getNodes
    Types.batchPatch (take 25 (map nodeToDelete nodes))
    OkResult <$> Types.deleteUser

requestSubscriptionSessionResolver :: Types.MonadMay m => GraphQLOut.Resolver m
requestSubscriptionSessionResolver =
  wrapResolver "creates a subscription session id" $
    Types.getSubscriptionSession
    
data OkResult = OkResult { okResult :: Bool }

instance ToGraphQL OkResult where
  graphqlOutSchema = outputObject "OkResult" "a result with only the ok field" $
    [ gqlOutField "ok" "whether the operation succeeded or not" okResult ]

nodeToDelete :: Types.Node -> Types.PatchCommand
nodeToDelete (Types.FolderNode Types.Folder{Types.folderId=fid}) = Types.DeleteCommand fid
nodeToDelete (Types.TaskNode Types.Task{Types.taskId=tid}) = Types.DeleteCommand tid


patchNodeResolver :: Types.MonadMay m => GraphQLOut.Resolver m
patchNodeResolver =
  let (GraphQLInputType patchNodeInput decodeGraphql) = graphqlSchema
  in

    GraphQLOut.ValueResolver
        (GraphQLOut.Field
          (Just "applies the given edits to the node")
          patchNodeResultType
          (HashMap.fromList
            [ ( "args"
              , GraphQLIn.Argument (Just "arguments to patch node")
                                   patchNodeInput
                                   Nothing
              )
            ]
          )
        )
      $ do
          (GraphQL.Arguments arguments) <- asks GraphQL.arguments
          let argsM = HashMap.lookup "args" arguments
          case argsM of
            Just args ->
              case (decodeGraphql args :: Either Text [Types.PatchCommand]) of
                Right commands -> do
                  Types.batchPatch commands
                  pure $ GraphQL.Object
                    (HashMap.fromList [("ok", GraphQL.Boolean True)])
                Left err -> Catch.throwM
                  (GraphQL.ResolverException
                    ( InvalidArgsError
                    $ Text.concat ["could not decode patch command, ", err]
                    )
                  )
            Nothing -> Catch.throwM
              (GraphQL.ResolverException
                (InvalidArgsError "no args as argument")
              )



data PatchCommandInput = PatchCommandInput {
      patchCommandInputType :: PatchCommandType,
      patchCommandInputFolder :: Maybe Types.FolderUpdate,
      patchCommandInputTask :: Maybe Types.TaskUpdate,
      patchCommandInputId :: Maybe Text
      }

instance FromGraphQL Types.PatchCommand where
  graphqlSchema =
    let (GraphQLInputType type_ decode) = graphqlSchema
    in
      GraphQLInputType type_ $ \v -> do
        patchCommandInput <- decode v
        case patchCommandInputType patchCommandInput of
          DeleteCommandType -> do
            id_ <- maybeToRight "delete command should have id field"
                                (patchCommandInputId patchCommandInput)
            pure $ Types.DeleteCommand id_
          UpdateCommandType ->
            case patchCommandInputFolder patchCommandInput of
              Just folderInput ->
                pure $ Types.UpdateCommand $ Types.UpdateFolder folderInput
              Nothing -> do
                task <- maybeToRight
                  "update command needs either task or folder update"
                  (patchCommandInputTask patchCommandInput)
                pure $ Types.UpdateCommand $ Types.UpdateTask task





asGqlObject :: GraphQL.Value -> Maybe (HashMap.HashMap Text GraphQL.Value)
asGqlObject (GraphQL.Object obj) = Just obj
asGqlObject _                    = Nothing

prefixError :: Text -> Parser a -> Parser a
prefixError prefix (Left err) = Left $ Text.concat [prefix, ": ", err]
prefixError _      a          = a

data PatchCommandType = UpdateCommandType | DeleteCommandType

instance FromGraphQL PatchCommandType where
  graphqlSchema =
    let type_ = GraphQLIn.NonNullEnumType
          (GraphQL.EnumType
            "PatchCommandType"
            (Just "The type of the command, either update or delete")
            (HashMap.fromList
              [ ("DELETE", GraphQL.EnumValue (Just "DELETE"))
              , ("UPDATE", GraphQL.EnumValue (Just "UPDATE"))
              ]
            )
          )
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.Enum "DELETE") -> pure DeleteCommandType
          (GraphQL.Enum "UPDATE") -> pure UpdateCommandType
          _ -> Left "Expected patch command type to be DELETE or UPDATE"


instance FromGraphQL PatchCommandInput where
  graphqlSchema =
    inputObject "PatchCommand"
                "a command to change or add something about the nodes"
      $   PatchCommandInput
      <$> gqlField "type" "delete or update action"
      <*> gqlMaybeField "folder" "the folder if you wish to update a folder"
      <*> gqlMaybeField "task"   "the task if you wish to update a task"
      <*> gqlMaybeField "id"     "the id if you want to delete a node"


instance FromGraphQL a => FromGraphQL [a] where
  graphqlSchema =
    let (GraphQLInputType childType decodeChild) = graphqlSchema
        type_ = GraphQLIn.NonNullListType childType
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.List list) -> sequenceA $ List.map decodeChild list
          _                   -> Left "Expected to be list"


instance FromGraphQL Time.UTCTime where
  graphqlSchema =
    let type_ = GraphQLIn.NonNullScalarType GraphQL.string
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.String iso8601) ->
            case ISO8601.parseISO8601 (Text.unpack iso8601) of
              Just result -> pure $ result
              Nothing     -> Left $ "Posixtime must be valid ISO8601"
          _ -> Left "Posixtime must be string (IS08601)"


data GraphQLInputType a = GraphQLInputType GraphQLIn.Type (GraphQL.Value -> Parser a)

class FromGraphQL a where
  graphqlSchema :: GraphQLInputType a

instance FromGraphQL Text where
  graphqlSchema =
    let type_ = GraphQLIn.NonNullScalarType GraphQL.string
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.String st) -> pure st
          _                   -> Left "Expected to be string"

instance FromGraphQL Double where
  graphqlSchema =
    let type_ = GraphQLIn.NonNullScalarType GraphQL.float
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.Float dbl) -> pure dbl
          _                   -> Left "Expected to be a double"

instance FromGraphQL Int32 where
  graphqlSchema =
    let type_ = GraphQLIn.NonNullScalarType GraphQL.int
    in  GraphQLInputType type_ $ \v -> case v of
          (GraphQL.Int int) -> pure int
          _                 -> Left "Expected to be a int"

gqlField :: FromGraphQL a => Text -> Text -> GraphQLInputFields a
gqlField name description =
  let (GraphQLInputType childType childDecoder) = graphqlSchema
      type_ = (name, GraphQLIn.InputField (Just description) childType Nothing)
  in  GraphQLInputFields [type_] $ \record -> do
        object <- maybeToRight
          (Text.concat ["Expecting field to be object: ", name])
          (asGqlObject record)
        field <- maybeToRight (Text.concat ["Expecting field ", name])
                              (HashMap.lookup name object)
        prefixError name $ childDecoder field

gqlMaybeField :: FromGraphQL a => Text -> Text -> GraphQLInputFields (Maybe a)
gqlMaybeField name description =
  let (GraphQLInputType childType childDecoder) = graphqlSchema
      newType = case childType of
        GraphQLIn.NonNullScalarType a -> GraphQLIn.NamedScalarType a
        GraphQLIn.NonNullInputObjectType a -> GraphQLIn.NamedInputObjectType a
        GraphQLIn.NonNullEnumType a -> GraphQLIn.NamedEnumType a
        GraphQLIn.NonNullListType a -> GraphQLIn.ListType a
        a -> a
      type_ = (name, GraphQLIn.InputField (Just description) newType Nothing)
  in  GraphQLInputFields [type_] $ \record -> do
        object <- maybeToRight
          (Text.concat ["Expecting field to be object: ", name])
          (asGqlObject record)
        case HashMap.lookup name object of
          Just a  -> prefixError name $ Just <$> (childDecoder a)
          Nothing -> prefixError name $ pure $ Nothing



data GraphQLInputFields a = GraphQLInputFields [(Text, GraphQLIn.InputField)] (GraphQL.Value -> Parser a)

instance Functor GraphQLInputFields where
  fmap mapfunc (GraphQLInputFields type_ func) =
    GraphQLInputFields type_ (\v -> mapfunc <$> func v)

instance Applicative GraphQLInputFields where
  (GraphQLInputFields type1_ func) <*> (GraphQLInputFields type2_ val) =
    GraphQLInputFields (type1_ ++ type2_) $ (\v -> func v <*> val v)
  pure a = GraphQLInputFields [] (const . pure $ a)

instance FromGraphQL Types.FolderUpdate where
  graphqlSchema =
    inputObject "FolderUpdate" "an update to a folder"
      $   Types.FolderUpdate
      <$> gqlField "id"     "the id of the folder you want to change"
      <*> gqlField "name"   "change the name of the folder"
      <*> gqlField "parent" "change the parent of the folder (move the folder)"

instance FromGraphQL Types.TaskUpdate where
  graphqlSchema =
    inputObject "TaskUpdate" "an update to a task"
      $   Types.TaskUpdate
      <$> gqlField "id"       "the id of the task you want to change"
      <*> gqlField "name"     "change the name of the task"
      <*> gqlField "parent"   "change the parent of the task (move the task)"
      <*> gqlField "duration" "change the duration of the task"
      <*> gqlMaybeField "due"    "change the due date of the task"
      <*> gqlMaybeField "doneOn" "when the task was completed"


inputObject :: Text -> Text -> GraphQLInputFields a -> GraphQLInputType a
inputObject name description (GraphQLInputFields fields decoder) =
  let type_ = GraphQLIn.NonNullInputObjectType $ GraphQLIn.InputObjectType
        name
        (Just description)
        (HashMap.fromList fields)
  in  GraphQLInputType type_ decoder


patchNodeResultType :: Types.MonadMay m => GraphQLOut.Type m
patchNodeResultType = GraphQLOut.NonNullObjectType $ GraphQLOut.ObjectType
  "PathNodeResponse"
  Nothing
  []
  (HashMap.fromList
    [ ( "ok"
      , fieldResolver "ok"
                      "Whether the patch suceeded"
                      (GraphQLOut.NonNullScalarType GraphQL.boolean)
      )
    ]
  )
