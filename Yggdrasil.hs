{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric, ScopedTypeVariables #-}

import Ygg.Event (UserId(..), NodeId(..), NodeContent(..), Event(NodeAdded))
import Ygg.EventStore
    (EventStore, pushEvent, initializeEventStore, getAllEvents)
    
import qualified Ygg.WebSocketServer
    
import Web.Scotty (put, post, get, redirect, body, scotty, middleware,
                   json, text, param)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (static)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Data.IORef (IORef, newIORef, atomicModifyIORef)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text.Lazy as T

data YggState = YggState { yggNextNodeId :: Integer
                         , yggEventStore :: EventStore
                         , yggSessionMap :: Map String String }

newYggState es = YggState { yggNextNodeId = 1
                          , yggEventStore = es
                          , yggSessionMap = Map.empty }

defaultPort = 3000

main = do
  eventStore <- initializeEventStore
  yggState <- newMVar (newYggState eventStore)
  
  forkIO $ Ygg.WebSocketServer.start eventStore
  
  scotty defaultPort $ do
    
    middleware logStdoutDev
    middleware static
  
    post "/:parentId" $ \parentId -> do
      content <- readContent
      sessionId <- readSessionId
      username <- liftIO $ getUsernameForSession sessionId yggState
      id <- takeNextNodeId yggState
      liftIO $ pushNodeAddedEvent eventStore id (NodeId parentId)
                 (NodeContent $ T.pack $ content)
                 (UserId username)

    get "/history" $ do
      liftIO (getAllEvents eventStore) >>= json

    post "/login/:username" $ \(username :: String) -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      liftIO $ addUserSession yggState sessionId username
      json sessionId

    get "/" (redirect "/index.html")

readContent = param $ T.pack "content"
readSessionId = param $ T.pack "sessionId"

takeNextNodeId = liftIO . consumeNodeId

getUsernameForSession sessionId yggState =
  readMVar yggState >>= return . (Map.! sessionId) . yggSessionMap

addUserSession :: MVar YggState -> String -> String -> IO ()
addUserSession y k v = modifyMVar_ y (return . f)
    where f y = y { yggSessionMap = Map.insert k v (yggSessionMap y) }

pushNodeAddedEvent :: EventStore -> NodeId -> NodeId -> NodeContent 
                      -> UserId -> IO ()
pushNodeAddedEvent es id id' c userId = 
  liftIO $ pushEvent es (NodeAdded id id' c userId)

consumeNodeId v = do
  y <- takeMVar v
  putMVar v (y { yggNextNodeId = yggNextNodeId y + 1 })
  return $ NodeId (yggNextNodeId y)
