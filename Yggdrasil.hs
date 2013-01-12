{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric, ScopedTypeVariables #-}

import Ygg.Event

import qualified Ygg.EventStore

import Ygg.EventBus (EventBus)
import qualified Ygg.EventBus

import qualified Ygg.TreeCache

import Ygg.TreeCacheServer
    
import System.Log.Logger

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Time.Clock (UTCTime, getCurrentTime)

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (static)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text.Lazy as T
import qualified Data.Text as TT

data YggState = YggState { yggEventBus :: EventBus
                         , yggSessionMap :: Map String UserId
                         , yggUserMap :: Map UserName UserId
                         , yggTreeCache :: TreeCacheServer }

newYggState es t = 
  YggState { yggEventBus = es
           , yggSessionMap = Map.empty 
           , yggUserMap = Map.empty 
           , yggTreeCache = t }

defaultPort = 3000

main = do
  updateGlobalLogger "Ygg" (setLevel DEBUG)
  
  infoM "Ygg" "Starting Event Bus"
  
  eventBus <- Ygg.EventBus.start
  eventStore <- Ygg.EventStore.start
  treeCache <- Ygg.TreeCacheServer.start =<<
                 (Ygg.EventStore.getAllEvents eventStore)
  
  yggState <- newMVar (newYggState eventBus treeCache)
  
  scotty defaultPort $ do
    
    middleware logStdoutDev
    middleware static
  
    post "/:parentId" $ \parentId -> do
      content <- readContent
      sessionId <- readSessionId
      Just userId <- liftIO $ getUserIdForSession sessionId yggState
      nodeId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      creationDate <- liftIO getCurrentTime
      liftIO $ pushNodeAddedEvent eventBus
                       (NodeId nodeId) (NodeId parentId)
                       (NodeContent $ T.pack $ content)
                       userId
                       creationDate

    get "/history" $ do
      liftIO (Ygg.EventStore.getAllEvents eventStore) >>= json
      
    get "/cache" $ do
      liftIO (Ygg.TreeCacheServer.getYggdrasil treeCache) >>= json

    post "/login/:username" $ \username -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      Just userId <- liftIO $ getIdForUser (UserName username) yggState
      liftIO $ addUserSession yggState sessionId userId
      
      liftIO $ debugM "Ygg.WebServer" $
        username ++ " logged in (" ++ show (userId, sessionId) ++ ")"

      json [sessionId, renderUserId userId]
      
    post "/register/:username" $ \userName -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      userId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      
      liftIO $ addUser yggState (UserName userName) (UserId userId)
      liftIO $ addUserSession yggState sessionId (UserId userId)
      
      liftIO $ pushUserRegisteredEvent eventBus (UserId userId)
      liftIO $ pushUserNameSetEvent eventBus
        (UserId userId) (UserName userName)
      
      liftIO $ debugM "Ygg.WebServer" $
        userName ++ " registered (" ++ show (userId, sessionId) ++ ")"
      
      json [sessionId, userId]
      
    post "/my-gravatar-hash" $ do
      content <- readContent
      sessionId <- readSessionId
      Just userId <- liftIO $ getUserIdForSession sessionId yggState
      
      liftIO $ pushUserGravatarHashSetEvent eventBus
        userId (GravatarHash content)
        
      liftIO $ debugM "Ygg.WebServer" $
        show userId ++ " set Gravatar hash to " ++ show content

    get "/" (redirect "/index.html")

readContent = param $ T.pack "content"
readSessionId = param $ T.pack "sessionId"

getUserIdForSession :: String -> MVar YggState -> IO (Maybe UserId)
getUserIdForSession sessionId yggState =
  fmap ((Map.lookup sessionId) . yggSessionMap) (readMVar yggState)

getIdForUser :: UserName -> MVar YggState -> IO (Maybe UserId)
getIdForUser userName yggState = do
  y <- atomically . readTVar . latestYggdrasil . yggTreeCache =<< readMVar yggState
  return $ Map.lookup userName (Ygg.TreeCache.yggUserNames y)

addUser :: MVar YggState -> UserName -> UserId -> IO ()
addUser y k v = modifyMVar_ y (return . f)
    where f y = y { yggUserMap = Map.insert k v (yggUserMap y) }

addUserSession :: MVar YggState -> String -> UserId -> IO ()
addUserSession y k v = modifyMVar_ y (return . f)
    where f y = y { yggSessionMap = Map.insert k v (yggSessionMap y) }

pushNodeAddedEvent :: EventBus -> NodeId -> NodeId -> NodeContent 
                      -> UserId -> UTCTime -> IO ()
pushNodeAddedEvent bus id id' c userId creationDate =
    liftIO $ Ygg.EventBus.pushEvent bus e
        where e = (NodeAdded id id' c userId creationDate)

pushUserRegisteredEvent :: EventBus -> UserId -> IO ()
pushUserRegisteredEvent bus userId =
    liftIO $ Ygg.EventBus.pushEvent bus e
        where e = UserRegistered userId

pushUserNameSetEvent :: EventBus -> UserId -> UserName -> IO ()
pushUserNameSetEvent bus userId userName =
    liftIO $ Ygg.EventBus.pushEvent bus e
        where e = UserNameSet userId userName

pushUserGravatarHashSetEvent :: EventBus -> UserId -> GravatarHash -> IO ()
pushUserGravatarHashSetEvent bus userId gravatarHash =
    liftIO $ Ygg.EventBus.pushEvent bus e
        where e = UserGravatarHashSet userId gravatarHash

instance Parsable UUID.UUID where
  parseParam = readEither