{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric #-}

import Ygg.Event (NodeId(..), NodeContent(..), Event(NodeAdded))
import Ygg.EventStore
    (EventStore, pushEvent, initializeEventStore)
    
import qualified Ygg.WebSocketServer (start)

import Web.Scotty (put, get, redirect, body, scotty, middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (static)

import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Data.IORef (IORef, newIORef, atomicModifyIORef)

import Data.Text.Lazy.Encoding (decodeUtf8)

defaultPort = 3000

main = do
  nodeIdSupply <- newIORef 1
  eventStore <- initializeEventStore
  
  forkIO $ Ygg.WebSocketServer.start eventStore
    
  scotty defaultPort $ do
    
    middleware logStdoutDev
    middleware static
  
    put "/:parentId" $ \parentId -> do
      content <- readContentFromRequestBody
      id <- takeNextNodeId nodeIdSupply
      liftIO $ pushNodeAddedEvent eventStore id (NodeId parentId) content

    get "/" (redirect "/index.html")

readContentFromRequestBody = liftM (NodeContent . decodeUtf8) body

takeNextNodeId = liftIO . consumeNodeId

pushNodeAddedEvent :: EventStore -> NodeId -> NodeId -> NodeContent -> IO ()
pushNodeAddedEvent es id id' c = 
  liftIO $ pushEvent es (NodeAdded id id' c)

consumeNodeId :: IORef Integer -> IO NodeId
consumeNodeId r = fmap NodeId $ atomicModifyIORef r (\n -> (n + 1, n))
