{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module May.ServerlessMayMonad (ServerlessMayMonad, runServerlessMayMonad, Context(..)) where

import           May
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , void
                                                , runReaderT
                                                , MonadReader
                                                , MonadIO
                                                , liftIO
                                                )
import qualified Network.AWS                   as AWS
import qualified Network.AWS.DynamoDB          as DynamoDB
import qualified Network.AWS.CognitoIdentityProvider
                                               as Cognito
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ((.:))
import qualified Data.HashMap.Strict           as HashMap
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import qualified Control.Lens                  as Lens
import           Data.Text                      ( Text )
import qualified Control.Monad.Catch           as Catch
import qualified Language.GraphQL.Error        as GraphQL
import qualified Data.List                     as List
import qualified Data.Time                     as Time
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Maybe                    as Maybe
import qualified Control.Exception.Base        as Exception
import qualified Data.Either                   as Either
import           Network.AWS.Prelude            ( NonEmpty((:|)) )
import           Text.Read                      ( readMaybe )
import           System.Environment             ( getEnv )
import           May.Types                     as Types
import qualified Network.Wreq                  as Wreq
import qualified Control.Monad.Trans.AWS       as AWS
                                         hiding ( send )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Control.Exception.Lens        as Lens

newtype ServerlessMayMonad a = ServerlessMayMonad { runServerlessMayMonadInternal :: ReaderT Context AWS.AWS a }
  deriving (Functor, Applicative, Monad, Catch.MonadThrow, Catch.MonadCatch, MonadReader Context, MonadIO, AWS.MonadAWS )

data Context = Context { contextSub :: Maybe Text}

runServerlessMayMonad :: Context -> AWS.Env -> ServerlessMayMonad a -> IO a
runServerlessMayMonad context env monad = 
  AWS.runResourceT $ AWS.runAWST env $ runReaderT ( runServerlessMayMonadInternal monad) context

getSub :: ServerlessMayMonad Text 
getSub  = do
  subM <- asks contextSub
  case subM of 
    Just sub -> pure sub
    Nothing -> 
      Catch.throwM (GraphQL.ResolverException UnauthenticatedException)


instance MonadMay ServerlessMayMonad where
  getNodes = do
    sub <- getSub
    nodeTableName <- getNodeTableName
    let query = DynamoDB.query nodeTableName & DynamoDB.qKeyConditionExpression .~ Just "user_id = :user_id" & DynamoDB.qExpressionAttributeValues .~ (HashMap.fromList [(":user_id", dynoString sub)])
    response <- AWS.send query
    case collectErrors $ List.map fromDynamoDB (response^.DynamoDB.qrsItems) :: Parser [Node] of
      Right values -> pure values
      Left err -> 
        Catch.throwM (GraphQL.ResolverException (ServerError err))
  batchPatch commands =
     case commands of
       (first : rest) -> do
          sub <- getSub
          nodeTableName <- getNodeTableName
          let writeRequests = fmap (patchCommandToWriteRequest sub) (first :| rest)
          () <$ AWS.send (DynamoDB.batchWriteItem & DynamoDB.bwiRequestItems .~ (HashMap.fromList [(nodeTableName, writeRequests)]))
       _ ->
          return ()
  getSubscription = do
    user <- getStripeUser
    if stripeCustomerIsSubscriber user
       then 
         True <$ setUserGroupAsSubscriber
       else
         return False
  getSubscriptionSession = do
    user <- getStripeUser
    createStripeSubscriptionSession user
  deleteUser = do
    sub <- getSub
    userPool <- getCognitoPoolName
    let query = Cognito.adminDeleteUser userPool sub
    _ <- cognitoDeleteUser query
    sid <- dynoDeleteUser sub
    True <$ stripeDeleteCustomer sid
  getUsers = do
    userPool <- getCognitoPoolName
    let query = Cognito.listUsers userPool
    result <- AWS.send query
    let users = result ^. Cognito.lursUsers
    
    return . Maybe.catMaybes $ map (\user -> do
        let attributes = user ^. Cognito.utAttributes
            nameL = filter (\attribute -> attribute ^. Cognito.atName == "name") attributes
            subL = filter (\attribute -> attribute ^. Cognito.atName == "sub") attributes
        case (nameL, subL) of
          ([nameA], [subA]) -> do
            sub <- subA ^. Cognito.atValue
            name <- nameA ^. Cognito.atValue
            return $ Types.User sub name
          _ -> Nothing
        ) users

  

dynoDeleteUser :: Text -> ServerlessMayMonad Text
dynoDeleteUser sub = do
    userTableName <- getUserTableName
    let query = DynamoDB.deleteItem userTableName & DynamoDB.diReturnValues .~ Just DynamoDB.AllOld
                                                  & DynamoDB.diKey .~ (HashMap.fromList [("user_id", dynoString sub)])

    response <- AWS.send query
    case fromDynamoDB (response^.DynamoDB.dirsAttributes) of
      Right user -> pure $ mayUserStripeId user
      Left err -> Catch.throwM (ServerError err)
  
   

cognitoDeleteUser :: Cognito.AdminDeleteUser -> ServerlessMayMonad ()
cognitoDeleteUser request = 
  Lens.catching AWS._ServiceError (void $ AWS.send request) $ \serviceError -> 
    if serviceError^.AWS.serviceCode == "UserNotFound" then
      liftIO (putStrLn "Cognito user already deleted")
    else
      Catch.throwM (ServerError . Text.pack $ "error with deleting user: " ++ show serviceError )
     
  

      
setUserGroupAsSubscriber :: ServerlessMayMonad ()
setUserGroupAsSubscriber = do
  sub <- getSub 
  userPool <- getCognitoPoolName
  let query = Cognito.adminAddUserToGroup userPool sub "Subscribers"
  () <$ AWS.send query

serverlessGetEnv :: MonadIO m => String -> m Text
serverlessGetEnv name = liftIO $ Text.pack <$> getEnv name

getStripeKey :: MonadIO m => m Text
getStripeKey = serverlessGetEnv "STRIPE_API_KEY"

getUserTableName :: MonadIO m => m Text
getUserTableName = serverlessGetEnv "USER_TABLE_NAME"

getNodeTableName :: MonadIO m => m Text
getNodeTableName = serverlessGetEnv "NODE_TABLE_NAME"

getCognitoPoolName :: MonadIO m => m Text
getCognitoPoolName = serverlessGetEnv "COGNITO_POOL"

getStripePrice :: MonadIO m => m Text
getStripePrice = serverlessGetEnv "STRIPE_PRICE"

getFrontendURL :: MonadIO m => m Text
getFrontendURL = serverlessGetEnv "FRONTEND_URL"

createStripeSubscriptionSession :: StripeCustomer -> ServerlessMayMonad Text
createStripeSubscriptionSession su = do
  stripe_key <- getStripeKey
  stripe_price <- getStripePrice
  frontEndUrl <- getFrontendURL
  let wreqOptions = Wreq.defaults & Wreq.headers .~ [("authorization", Text.encodeUtf8 $ Text.concat [ "Bearer ", stripe_key] ), ("content-type", "application/x-www-form-urlencoded")]
      createStripeSubscriptionSessionBody = 
        Text.encodeUtf8 $ Text.intercalate "&"
                    [ Text.append "cancel_url=" frontEndUrl
                    , "mode=subscription"
                    , Text.append "success_url=" frontEndUrl
                    , Text.append "customer=" (stripeCustomerId su)
                    , "payment_method_types[0]=card"
                    , Text.append "line_items[0][price]=" stripe_price
                    , "line_items[0][quantity]=1"
                    ]
  response <- liftIO $ Wreq.postWith wreqOptions (Text.unpack $ Text.concat [stripeBaseUrl, "/v1/checkout/sessions"])  createStripeSubscriptionSessionBody
  case Aeson.decode (response^.Wreq.responseBody) of
    Just (Aeson.Object subscription_session) -> 
       case HashMap.lookup "id" subscription_session of
         Just (Aeson.String sub_id) -> pure sub_id
         _ -> 
            Catch.throwM (ServerError "Subscription response did not have key")
    _ -> 
      Catch.throwM (ServerError "Subscription response was not JSON")


    
getStripeUser :: ServerlessMayMonad StripeCustomer
getStripeUser = do
    sub <- getSub
    userTableName <- getUserTableName
    let query = DynamoDB.getItem userTableName & DynamoDB.giKey .~ (HashMap.fromList [("user_id", dynoString sub)])
    
    response <- AWS.send query
    let item = response ^. DynamoDB.girsItem
    case fromDynamoDB item of
      Right user -> do
        sim <- getStripeUserFromStripe (mayUserStripeId user)
        case sim of
          Just si -> pure si
          Nothing -> 
            Catch.throwM (ServerError "User should have been created")
      Left _ -> do
        customer <- stripeCreateCustomer
        let put_query = DynamoDB.putItem userTableName & DynamoDB.piItem .~ (toDynamoDb (MayUser sub (stripeCustomerId customer)))
        _ <- AWS.send put_query
        pure customer

getStripeUserFromStripe :: Text -> ServerlessMayMonad (Maybe StripeCustomer)
getStripeUserFromStripe stripe_id = do
  stripe_key <- getStripeKey
  let options = Wreq.defaults & Wreq.headers .~ [("Authorization", Text.encodeUtf8 $ Text.concat ["Bearer ", stripe_key])]
  response <- liftIO $ Wreq.getWith options (Text.unpack $ Text.concat [stripeBaseUrl, "/v1/customers/", stripe_id])
  let body = response ^.Wreq.responseBody
  case Aeson.decode body of
    Just cus -> pure cus
    Nothing ->  Catch.throwM (ServerError "Could not decode stripe response")

stripeBaseUrl :: Text
stripeBaseUrl = "https://api.stripe.com"

stripeCreateCustomer :: ServerlessMayMonad StripeCustomer
stripeCreateCustomer = do
  stripe_key <- getStripeKey
  let options = Wreq.defaults & Wreq.headers .~ [("Authorization", Text.encodeUtf8 $ Text.concat ["Bearer ", stripe_key])]
  response <- liftIO $ Wreq.postWith options (Text.unpack $ Text.concat [stripeBaseUrl,"/v1/customers"]) (""::ByteString)
  let body = response ^.Wreq.responseBody
  case Aeson.decode body of
    Just cus -> pure cus
    Nothing -> Catch.throwM (ServerError "Could not decode stripe response")

stripeDeleteCustomer :: MonadIO m => Text -> m ()
stripeDeleteCustomer sid = do
  stripe_key <- getStripeKey
  let options = Wreq.defaults & Wreq.headers .~ [("Authorization", Text.encodeUtf8 $ Text.concat ["Bearer ", stripe_key])]
  _ <- liftIO $ Wreq.deleteWith options (Text.unpack $ Text.concat [stripeBaseUrl,"/v1/customers/", sid])
  pure ()

data StripeCustomer = StripeCustomer { stripeCustomerId :: Text, stripeCustomerIsSubscriber :: Bool}

instance Aeson.FromJSON StripeCustomer where
  parseJSON = Aeson.withObject "StripeCustomer" $ \v -> do
     sid <- v .: "id"
     subscription <- v .: "subscriptions"
     subList <- subscription .: "data"
     return $ StripeCustomer sid (length (subList :: [Aeson.Value]) > 0)

data MayUser = MayUser {mayUserSub :: Text, mayUserStripeId :: Text}

instance FromDynamoDBRecord MayUser where
  fromDynamoDB record = do
    user_id <- stringField record "user_id"
    stripe_id <- stringField record "stripe_id"
    pure $ MayUser user_id stripe_id

instance ToDynamoDb MayUser where
  toDynamoDb (MayUser sub sid) = HashMap.fromList 
    [ ("user_id", dynoString sub)
    , ("stripe_id", dynoString sid)
    ]
{-| DynamoDB Serialisation -}
class FromDynamoDBRecord a where
   fromDynamoDB :: HashMap.HashMap Text DynamoDB.AttributeValue -> Parser a

instance FromDynamoDBRecord Node where
   fromDynamoDB record = do
     gqlType <- maybeToRight "should have type field" (Lens.view DynamoDB.avS =<<HashMap.lookup "type" record)
     case gqlType of
        "task" -> TaskNode <$> fromDynamoDB record
        "folder" -> FolderNode <$> fromDynamoDB record
        _ -> Left "expecting node type to be task or folder"

instance FromDynamoDBRecord Folder where
  fromDynamoDB record = do
    fid <- stringField record "node_id"
    name <- stringField record "name"
    parent <- stringField record "pid"
    sharedWith <- stringField record "sharedWith"
    pure $ Folder fid name parent sharedWith

timeField :: HashMap.HashMap Text DynamoDB.AttributeValue -> Text -> Parser Time.UTCTime
timeField record name =
     Time.posixSecondsToUTCTime . fromInteger . floor .(/1000) <$> numberField record name


stringField :: HashMap.HashMap Text DynamoDB.AttributeValue -> Text -> Parser Text
stringField record name = do
  attributeValue <- maybeToRight (Text.concat ["should have ", name, " field"]) (HashMap.lookup name record)
  value <- maybeToRight (Text.concat [name, "should be a string"]) (attributeValue^.DynamoDB.avS)
  pure value

numberField :: HashMap.HashMap Text DynamoDB.AttributeValue -> Text -> Parser Double
numberField record name =  do 
  attributeValue <- maybeToRight (Text.concat ["should have ", name, " field"]) (HashMap.lookup name record)
  value <- maybeToRight (Text.concat [name, "should be a number"]) (attributeValue^.DynamoDB.avN)
  number <- maybeToRight (Text.concat [name, "should parse as a number (should never ever fail)"]) (readMaybe (Text.unpack value))
  pure number

dynoString :: Text -> DynamoDB.AttributeValue
dynoString string = DynamoDB.attributeValue & DynamoDB.avS .~ (Just string)

dynoNumber :: Double -> DynamoDB.AttributeValue
dynoNumber double = DynamoDB.attributeValue & DynamoDB.avN .~ (Just . Text.pack $ (show double))

dynoDate :: Time.UTCTime -> DynamoDB.AttributeValue
dynoDate = dynoNumber . (*1000) . fromRational . toRational . Time.utcTimeToPOSIXSeconds

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

class ToDynamoDb a where
   toDynamoDb :: a -> HashMap.HashMap Text DynamoDB.AttributeValue

instance ToDynamoDb FolderUpdate where
   toDynamoDb (FolderUpdate{folderUpdateId=fid,folderUpdateName=name,folderUpdateParent=parent}) =
     HashMap.fromList [ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just fid))
                    , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                    , ("pid", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                    , ("type", DynamoDB.attributeValue & DynamoDB.avS .~ (Just "folder"))
                    ]
instance ToDynamoDb TaskUpdate where
   toDynamoDb (TaskUpdate{ taskUpdateId=tid
                         , taskUpdateName=name 
                         , taskUpdateParent=parent
                         , taskUpdateDuration=duration
                         , taskUpdateDue=dueM
                         , taskUpdateDoneOn=doneOnM
                         }) =
      let optionalFields = Maybe.catMaybes [
             (\due -> ("due", dynoDate due)) <$> dueM
             ,(\doneOn -> ("doneOn", dynoDate doneOn)) <$> doneOnM
             ]
      in
           HashMap.fromList ([ ("node_id", DynamoDB.attributeValue & DynamoDB.avS .~ (Just tid))
                          , ("name", DynamoDB.attributeValue & DynamoDB.avS .~ (Just name))
                          , ("pid", DynamoDB.attributeValue & DynamoDB.avS .~ (Just parent))
                          , ("duration", DynamoDB.attributeValue & DynamoDB.avN .~ (Just (Text.pack (show duration))))
                           , ("type", DynamoDB.attributeValue & DynamoDB.avS .~ (Just "task"))
                          ] ++ optionalFields)

maybeField :: Parser a -> Parser (Maybe a)
maybeField (Right a) = Right (Just a)
maybeField (Left _) = Right Nothing

data UnauthenticatedException = UnauthenticatedException

instance Exception.Exception UnauthenticatedException where
  displayException _ = "Endpoint requires an access token" 

instance Show UnauthenticatedException where
  show _ = "Endpoint requires an access token" 

collectErrors :: [Parser a] -> Parser [a]
collectErrors lst = 
  case Either.lefts lst of
    [] -> pure $ Either.rights lst
    lefts -> Left $ Text.intercalate ", " lefts

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a _ = Left a

instance FromDynamoDBRecord Task where
   fromDynamoDB record = do
     tid <-  stringField record "node_id"
     name <-  stringField record "name"
     duration <- numberField record "duration"
     pid <- stringField record "pid"
     due <- maybeField (timeField record "due")
     doneOn <- maybeField (timeField record "doneOn")
     pure $ Task tid name pid duration due doneOn


