{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric, ScopedTypeVariables #-}

import Ygg.Event

import qualified Ygg.EventStore

import Ygg.EventBus (EventBus)
import qualified Ygg.EventBus

import qualified Ygg.TreeCache

import Ygg.TreeCacheServer
    
import Ygg.App

import System.Log.Logger

import Control.Applicative

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
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text.Lazy as T
import qualified Data.Text as TT

newYggState = 
  ServerState { yggSessionMap = Map.empty 
              , yggUserMap = Map.empty }

defaultPort = 3000

makeActFunction :: TreeCacheServer ->
                   EventBus ->
                   TVar ServerState ->
                   (forall a. Ygg.App.Action a -> ActionM a)
makeActFunction cache bus state m =
  do
    yggdrasil <- liftIO (Ygg.TreeCacheServer.getYggdrasil cache)
    sessionId <- readSessionId
    runAction yggdrasil sessionId state bus m

main = do
  updateGlobalLogger "Ygg" (setLevel DEBUG)
  
  eventBus <- Ygg.EventBus.start
  eventStore <- Ygg.EventStore.start
  allEvents <- Ygg.EventStore.getAllEvents eventStore
  treeCache <- Ygg.TreeCacheServer.start allEvents
  
  yggState <- atomically $ newTVar newYggState
    
  scotty defaultPort $ do
    
    let act = makeActFunction treeCache eventBus yggState
    
    middleware logStdoutDev
    middleware static
  
    post "/:parentId" $ \parentId -> do
      content <- readContent
      
      act $ postNode (NodeId parentId) content

    get "/history" $ do
      liftIO (Ygg.EventStore.getAllEvents eventStore) >>= json
      
    get "/cache" $ do
      liftIO (Ygg.TreeCacheServer.getYggdrasil treeCache) >>= json

    post "/login/:username" $ \username -> do
      (sessionId, userId) <- act $ loginAs username
      json [sessionId, renderUserId userId]
      
    post "/register/:username" $ \userName -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      userId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom

      act $ do
        addUser (UserName userName) (UserId userId)
        addUserSession sessionId (UserId userId)
      
        pushUserRegisteredEvent (UserId userId)
        pushUserNameSetEvent
          (UserId userId) (UserName userName)
      
      liftIO $ debugM "Ygg.WebServer" $
        userName ++ " registered (" ++ show (userId, sessionId) ++ ")"
      
      json [sessionId, userId]
      
    post "/my-gravatar-hash" $ do
      content <- readContent
      Just sessionId <- readSessionId
      Just userId <- act $ getUserIdForSession sessionId
      
      act $ pushUserGravatarHashSetEvent userId (GravatarHash content)
        
      liftIO $ debugM "Ygg.WebServer" $
        show userId ++ " set Gravatar hash to " ++ show content

    get "/" (redirect "/index.html")

readContent = param $ T.pack "content"

readSessionId = rescue (fmap Just $ param $ T.pack "sessionId") 
                       (\_ -> return Nothing)

postNode parentId content = do
  Just userId <- getLoggedInUserId
  nodeId <- liftIO $ UUID.toString <$> UUIDV4.nextRandom
  creationDate <- liftIO getCurrentTime
  pushNodeAddedEvent
    (NodeId nodeId) parentId
    (NodeContent $ T.pack $ content)
    userId creationDate
    
loginAs userName = do
  Just userId <- getIdForUser (UserName userName)
  sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      
  addUserSession sessionId userId
      
  liftIO $ debugM "Ygg.WebServer" $
    userName ++ " logged in (" ++ show (userId, sessionId) ++ ")"
    
  return (sessionId, userId)

pushNodeAddedEvent id id' c userId creationDate =
    writeEvent $ NodeAdded id id' c userId creationDate

pushUserRegisteredEvent userId =
    writeEvent $ UserRegistered userId

pushUserNameSetEvent userId userName =
    writeEvent $ UserNameSet userId userName

pushUserGravatarHashSetEvent userId gravatarHash =
    writeEvent $ UserGravatarHashSet userId gravatarHash

instance Parsable UUID.UUID where
  parseParam = readEither