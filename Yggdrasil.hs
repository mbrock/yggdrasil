{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric #-}

import Web.Scotty

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Network.HTTP.Types (status404)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Monoid (mconcat)
import Data.Foldable (foldl', msum)
import Data.Tree
import Data.IORef
import Data.Maybe

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as JSON

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import qualified Network.WebSockets as WS

import Control.Monad (liftM, forM_)
import Control.Monad.IO.Class (liftIO)

import GHC.Generics

newtype NodeId = NodeId Integer
               deriving (Eq, Show, Generic)
                        
newtype NodeContent = NodeContent T.Text
                    deriving (Eq, Show, Generic)

data Event = NodeAdded NodeId NodeId NodeContent
           deriving (Eq, Show, Generic)

type Yggdrasil = Tree (NodeId, NodeContent)

instance ToJSON Yggdrasil where
  toJSON (Node (id, content) xs) =
    object ["id" .= id, "content" .= content,
            "branches" .= map toJSON xs]
    
instance ToJSON NodeId
instance ToJSON NodeContent
instance ToJSON Event

main = do
  nextNodeIdRef <- newIORef 1
  eventsRef <- newTVarIO []
  
  forkIO $ webSocketServer eventsRef
  
  scotty 3000 $ do
    
    middleware logStdoutDev
    middleware static
  
    put "/:parentId" $ \parentId -> do
      content <- liftM (NodeContent . decodeUtf8) body
      nextNodeId <- liftIO $ (consumeNodeId nextNodeIdRef)
      let event = NodeAdded (NodeId nextNodeId) (NodeId parentId) content
      liftIO $ pushEvent eventsRef event

    get "/:nodeId" $ \nodeId -> do
      events <- liftIO . atomically $ readTVar eventsRef
      maybe (status status404 >> text "not found") json
        (findNode (NodeId nodeId) (growTree events))
        
type Client = WS.Sink WS.Hybi00
type ServerState = [Client]

broadcastEvent :: ServerState -> Event -> IO ()
broadcastEvent clients e =
  forM_ clients (flip WS.sendSink $ WS.textData $ decodeUtf8 $ JSON.encode e)

webSocketServer :: TVar [Event] -> IO ()
webSocketServer events = do
  state <- newMVar []
  WS.runServer "0.0.0.0" 9160 $ handleWebSocket events state
                                       
handleWebSocket :: TVar [Event] -> MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
handleWebSocket events state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    es <- liftIO . atomically $ readTVar events
    liftIO $ forM_ es (broadcastEvent [sink])

consumeNodeId :: IORef Integer -> IO Integer
consumeNodeId = flip atomicModifyIORef (\n -> (n + 1, n))

pushEvent :: TVar [a] -> a -> IO ()
pushEvent var e = atomically $ 
  readTVar var >>= \es -> writeTVar var (es ++ [e])
      
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