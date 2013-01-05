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

defaultPort = 3000

addUserSession :: MVar (Map String String) -> String -> String -> IO ()
addUserSession map k v =
  modifyMVar_ map (return . Map.insert k v)
  
main = do
  nodeIdSupply <- newIORef 1
  eventStore <- initializeEventStore
  sessionMap <- newMVar Map.empty
  
  forkIO $ Ygg.WebSocketServer.start eventStore
  
  scotty defaultPort $ do
    
    middleware logStdoutDev
    middleware static
  
    post "/:parentId" $ \parentId -> do
      content <- readContent
      sessionId <- readSessionId
      username <- liftIO $ liftM (Map.! sessionId) (readMVar sessionMap)
      id <- takeNextNodeId nodeIdSupply
      liftIO $ pushNodeAddedEvent eventStore id (NodeId parentId)
                 (NodeContent $ T.pack $ content)
                 (UserId username)

    get "/history" $ do
      liftIO (getAllEvents eventStore) >>= json

    post "/login/:username" $ \(username :: String) -> do
      sessionId <- liftIO $ fmap UUID.toString UUIDV4.nextRandom
      liftIO $ addUserSession sessionMap sessionId username
      json sessionId

    get "/" (redirect "/index.html")

readContent = param $ T.pack "content"
readSessionId = param $ T.pack "sessionId"

takeNextNodeId = liftIO . consumeNodeId

pushNodeAddedEvent :: EventStore -> NodeId -> NodeId -> NodeContent 
                      -> UserId -> IO ()
pushNodeAddedEvent es id id' c userId = 
  liftIO $ pushEvent es (NodeAdded id id' c userId)

consumeNodeId :: IORef Integer -> IO NodeId
consumeNodeId r = fmap NodeId $ atomicModifyIORef r (\n -> (n + 1, n))
