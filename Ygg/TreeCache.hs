{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Ygg.TreeCache (
) where

import Ygg.Event

import Data.Tree

import Data.Foldable (foldl', msum)

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=))
import qualified Data.Aeson as JSON

import qualified Data.Text.Lazy as T

type Yggdrasil = Tree (NodeId, NodeContent)

instance ToJSON Yggdrasil where
  toJSON (Node (id, content) xs) =
    object ["id" .= id, "content" .= content,
            "branches" .= map toJSON xs]

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