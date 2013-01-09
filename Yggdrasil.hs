{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric, ScopedTypeVariables #-}

import Ygg.Event (UserId(..), NodeId(..), NodeContent(..), Event(NodeAdded))

import qualified Ygg.EventStore

import Ygg.EventBus (EventBus)
import qualified Ygg.EventBus
    
import System.Log.Logger

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

data YggState = YggState { yggNextNodeId :: Integer
                         , yggEventBus :: EventBus
                         , yggSessionMap :: Map String String }

newYggState es = YggState { yggNextNodeId = 1
                          , yggEventBus = es
                          , yggSessionMap = Map.empty }

defaultPort = 3000

main = do
  updateGlobalLogger "Ygg" (setLevel DEBUG)
  
  infoM "Ygg" "Starting Event Bus"
  
  eventBus <- Ygg.EventBus.start
  eventStore <- Ygg.EventStore.start
  
  yggState <- newMVar (newYggState eventBus)
  
  scotty defaultPort $ do
    
    middleware logStdoutDev
    middleware static
  
    post "/:parentId" $ \parentId -> do
      content <- readContent
      sessionId <- readSessionId
      username <- liftIO $ getUsernameForSession sessionId yggState
      id <- takeNextNodeId yggState
      creationDate <- liftIO getCurrentTime
      liftIO $ pushNodeAddedEvent eventBus
                       id (NodeId parentId)
                       (NodeContent $ T.pack $ content)
                       (UserId username)
                       creationDate

    get "/history" $ do
      liftIO (Ygg.EventStore.getAllEvents eventStore) >>= json

    post "/login/:username" $ \(username :: String) -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      liftIO $ addUserSession yggState sessionId username
      json sessionId

    get "/" (redirect "/index.html")

readContent = param $ T.pack "content"
readSessionId = param $ T.pack "sessionId"

takeNextNodeId = liftIO . consumeNodeId

consumeNodeId :: MVar YggState -> IO NodeId
consumeNodeId v = do
  y <- takeMVar v
  putMVar v (y { yggNextNodeId = yggNextNodeId y + 1 })
  return $ NodeId (yggNextNodeId y)

getUsernameForSession :: String -> MVar YggState -> IO String
getUsernameForSession sessionId yggState =
  readMVar yggState >>= return . (Map.! sessionId) . yggSessionMap

addUserSession :: MVar YggState -> String -> String -> IO ()
addUserSession y k v = modifyMVar_ y (return . f)
    where f y = y { yggSessionMap = Map.insert k v (yggSessionMap y) }

pushNodeAddedEvent :: EventBus -> NodeId -> NodeId -> NodeContent 
                      -> UserId -> UTCTime -> IO ()
pushNodeAddedEvent bus id id' c userId creationDate =
    liftIO $ Ygg.EventBus.pushEvent bus e
        where e = (NodeAdded id id' c userId creationDate)
