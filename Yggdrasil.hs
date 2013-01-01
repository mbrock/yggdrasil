{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Foldable
import Data.Tree
import Data.IORef
import Data.Maybe

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

newtype NodeId = NodeId Integer
               deriving (Eq, Show)
                        
newtype NodeContent = NodeContent T.Text
                    deriving (Eq, Show)

data Event = NodeAdded NodeId NodeId NodeContent
           deriving (Eq, Show)

type Yggdrasil = Tree (NodeId, NodeContent)

main = do
  nextNodeIdRef <- newIORef 1
  eventsRef <- newIORef []

  scotty 3000 $ do
  
    put "/:parentId" $ do
      parentId <- liftM NodeId (param "parentId")
      content <- liftM (NodeContent . decodeUtf8) body
      nextNodeId <- liftIO $ atomicModifyIORef nextNodeIdRef (\n -> (n + 1, n))
      let event = NodeAdded (NodeId nextNodeId) parentId content
      liftIO $ modifyIORef eventsRef (++ [event])

    get "/:nodeId" $ do
      nodeId <- liftM NodeId (param "nodeId")
      events <- liftIO $ readIORef eventsRef
      text . T.pack $
        show events ++ "\n" ++
        maybe "not found" (drawTree . stringTree)
          (findNode nodeId (growTree events))
      
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