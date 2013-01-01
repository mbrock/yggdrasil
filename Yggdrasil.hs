{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric #-}

import Ygg.Event
import Ygg.EventStore

import Web.Scotty (scotty, get, put, middleware, body,
                   text, json, status, redirect)

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Network.HTTP.Types (status404)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception

import Data.Monoid (mconcat)
import Data.Foldable (foldl', msum)
import Data.List (delete)
import Data.Tree
import Data.IORef
import Data.Maybe

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=))
import qualified Data.Aeson as JSON

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy as BS

import qualified Network.WebSockets as WS

import Control.Monad (liftM, forM_, forever)
import Control.Monad.IO.Class (liftIO)

import GHC.Generics

type Yggdrasil = Tree (NodeId, NodeContent)

instance ToJSON Yggdrasil where
  toJSON (Node (id, content) xs) =
    object ["id" .= id, "content" .= content,
            "branches" .= map toJSON xs]
    
main = do
  nodeIdSupply <- newIORef 1
  eventStore <- initializeEventStore
  
  forkIO $ webSocketServer eventStore
    
  scotty 3000 $ do
    
    middleware logStdoutDev
    middleware static
  
    put "/:parentId" $ \parentId -> do
      content <- readContentFromRequestBody
      nextNodeId <- takeNextNodeId nodeIdSupply
      pushNodeAddedEvent eventStore nextNodeId parentId content

    get "/" (redirect "/index.html")

readContentFromRequestBody = liftM (NodeContent . decodeUtf8) body

takeNextNodeId = liftIO . consumeNodeId

pushNodeAddedEvent eventStore nodeId parentId content = 
  liftIO $ pushEvent eventStore event
  where event = NodeAdded (NodeId nodeId) (NodeId parentId) content

data Client = Client { clientId :: Integer
                     , clientSink :: WS.Sink WS.Hybi00 }
              
instance Eq Client where
  a == b = clientId a == clientId b
              
data ServerState = ServerState { ssClients :: [Client]
                               , ssNextClientId :: Integer }

broadcastEvent :: ServerState -> Event -> IO ()
broadcastEvent clients e = do
  print (length $ ssClients clients, e)
  forM_ (map clientSink $ ssClients clients)
    (flip WS.sendSink $ WS.textData $ decodeUtf8 $ JSON.encode e)
  
webSocketServer :: EventStore -> IO ()
webSocketServer eventStore = do
  state <- newMVar (ServerState [] 0)
  
  forkIO $ do
    myEventChan <- chanForNewEvents eventStore
    forever $ do
      e <- atomically $ readTChan myEventChan
      print e
      clients <- readMVar state
      broadcastEvent clients e
  
  WS.runServer "0.0.0.0" 9160 $ handleWebSocket eventStore state
                                       
handleWebSocket :: EventStore -> MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
handleWebSocket eventStore state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    newClientId <- fmap ssNextClientId (liftIO $ readMVar state)
    sink <- WS.getSink
    let client = Client newClientId sink
    liftIO $ modifyMVar_ state
      (\ss -> return $ ss { ssClients = client : ssClients ss 
                          , ssNextClientId = newClientId + 1 })
    es <- liftIO $ getAllEvents eventStore
    
    catchDisconnect <- return (\e -> case fromException e of
      Just WS.ConnectionClosed -> liftIO $ removeClient state client
      _ -> return () :: WS.WebSockets WS.Hybi00 ())
    
    flip WS.catchWsError catchDisconnect $ do
      liftIO $ forM_ es (broadcastEvent (ServerState [client] 0))
      forever $ do
        WS.receiveData :: WS.WebSockets WS.Hybi00 T.Text
        return ()

removeClient :: MVar ServerState -> Client -> IO ()
removeClient state sink = modifyMVar_ state f
  where f ss = return $ ss { ssClients = delete sink (ssClients ss) }

consumeNodeId :: IORef Integer -> IO Integer
consumeNodeId = flip atomicModifyIORef (\n -> (n + 1, n))
      
stringTree :: Yggdrasil -> Tree String
stringTree = fmap show
      
growTree :: [Event] -> Yggdrasil
growTree = foldl' processEvent e
  where e = Node (NodeId 0, NodeContent (T.pack "")) []

processEvent :: Yggdrasil -> Event -> Yggdrasil
processEvent y (NodeAdded nodeId parentId content) =
  addNode (nodeId, content) parentId y
  
addNode :: (NodeId, NodeContent) -> NodeId -> Yggdrasil -> Yggdrasil
addNode newNode parentId tree = addNodeWhere f tree
  where f (id, content) | id == parentId = Just (Node newNode [])
        f _ = Nothing

addNodeWhere :: (a -> Maybe (Tree a)) -> Tree a -> Tree a
addNodeWhere f (Node x ns) =
  case f x of
    Just t -> Node x (t:ns)
    Nothing -> Node x (map (addNodeWhere f) ns)
    
rootId :: Yggdrasil -> NodeId
rootId (Node (id, _) _) = id
    
findNode :: NodeId -> Yggdrasil -> Maybe Yggdrasil
findNode id y = if rootId y == id
                then Just y 
                else msum $ map (findNode id) (subForest y)