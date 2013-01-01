{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric #-}

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Foldable
import Data.Tree
import Data.IORef
import Data.Maybe

import Data.Aeson as JSON

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import Control.Monad (liftM)
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

main = do
  nextNodeIdRef <- newIORef 1
  eventsRef <- newIORef []

  scotty 3000 $ do
  
    put "/:parentId" $ \parentId -> do
      content <- liftM (NodeContent . decodeUtf8) body
      nextNodeId <- liftIO $ (consumeNodeId nextNodeIdRef)
      let event = NodeAdded (NodeId nextNodeId) (NodeId parentId) content
      liftIO $ pushEvent eventsRef event

    get "/:nodeId" $ \nodeId -> do
      events <- liftIO $ readIORef eventsRef
      text . decodeUtf8 $
        maybe (encode $ toJSON ("not found" :: String)) encode
          (findNode (NodeId nodeId) (growTree events))
      
consumeNodeId :: IORef Integer -> IO Integer
consumeNodeId = flip atomicModifyIORef (\n -> (n + 1, n))

pushEvent :: IORef [Event] -> Event -> IO ()
pushEvent es e = modifyIORef es (++ [e])
      
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