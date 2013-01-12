{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Ygg.TreeCache (
  Yggdrasil (..), growTree, processEvent
) where

import Ygg.Event

import Data.Tree

import Data.Foldable (foldl', msum)

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=))
import qualified Data.Aeson as JSON

import qualified Data.Text.Lazy as T

import Data.Time.Clock
import Data.Time.Calendar

import Data.Map (Map)
import qualified Data.Map as Map

data YggUser =
  YggUser { yggUserId :: UserId
          , yggUserName :: UserName
          , yggUserGravatarHash :: GravatarHash }

type YggNode = (NodeId, NodeContent, UserId, UTCTime)

data Yggdrasil =
  Yggdrasil { yggTree :: Tree YggNode
            , yggUsers :: Map UserId YggUser 
            , yggUserNames :: Map UserName UserId }

instance ToJSON (Map UserId YggUser) where
  toJSON m = toJSON (Map.mapKeys renderUserId m)

instance ToJSON (Tree YggNode) where
  toJSON (Node (id, content, userId, time) xs) =
    object ["nodeId" .= id,
            "content" .= content,
            "branches" .= map toJSON xs,
            "userId" .= userId,
            "creationDate" .= time]
    
instance ToJSON Yggdrasil where
  toJSON y =
    object ["tree" .= yggTree y,
            "users" .= yggUsers y]

instance ToJSON YggUser where
  toJSON u =
    object ["userId" .= yggUserId u,
            "userName" .= yggUserName u,
            "userGravatarHash" .= yggUserGravatarHash u]

growTree :: [Event] -> Yggdrasil
growTree = foldl' processEvent e
  where e = Yggdrasil {
          yggTree = Node (NodeId "1cb24849-2565-40eb-9b41-ea65daa6b271",
                          NodeContent (T.pack ""),
                          UserId "1cb24849-2565-40eb-9b41-ea65daa6b271",
                          UTCTime (fromGregorian 2013 1 1) 0) [],
          yggUsers = Map.empty, 
          yggUserNames = Map.empty }

processEvent :: Yggdrasil -> Event -> Yggdrasil
processEvent y e = 
  case e of
    NodeAdded nodeId parentId content userId time ->
         modifyTree (addNode (nodeId, content, userId, time) parentId) y
    UserRegistered userId ->
         addUser userId y
    UserNameSet userId userName -> 
         setUserName userId userName y
    UserGravatarHashSet userId gravatarHash ->
         setUserGravatarHash userId gravatarHash y
  
modifyTree :: (Tree YggNode -> Tree YggNode) -> Yggdrasil -> Yggdrasil
modifyTree f y = y { yggTree = f (yggTree y) }
  
addNode :: YggNode -> NodeId -> Tree YggNode -> Tree YggNode
addNode newNode parentId y = addNodeWhere f y
  where f (id, _, _, _) | id == parentId = Just (Node newNode [])
        f _ = Nothing
        
addNodeWhere :: (a -> Maybe (Tree a)) -> Tree a -> Tree a
addNodeWhere f (Node x ns) =
  case f x of
    Just t -> Node x (t:ns)
    Nothing -> Node x (map (addNodeWhere f) ns)
    
rootId :: Tree YggNode -> NodeId
rootId (Node (id, _, _, _) _) = id
                     
addUser :: UserId -> Yggdrasil -> Yggdrasil
addUser userId y = y { yggUsers = Map.insert userId user (yggUsers y) }
    where user = YggUser { yggUserId = userId
                         , yggUserName = UserName "unnamed"
                         , yggUserGravatarHash = GravatarHash "" }
                 
setUserName :: UserId -> UserName -> Yggdrasil -> Yggdrasil
setUserName userId userName y =
    y { yggUsers = Map.adjust f userId (yggUsers y), 
        yggUserNames = Map.insert userName userId (yggUserNames y) }
    where f v = v { yggUserName = userName }

setUserGravatarHash :: UserId -> GravatarHash -> Yggdrasil -> Yggdrasil
setUserGravatarHash userId gravatarHash y =
    y { yggUsers = Map.adjust f userId (yggUsers y) }
    where f v = v { yggUserGravatarHash = gravatarHash }
