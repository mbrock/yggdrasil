{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Ygg.Event (Event (..),
                  NodeId (..),
                  UserId (..),
                  UserName (..),
                  GravatarHash (..),
                  NodeContent (..),
                  renderNodeId,
                  renderUserId,
                  renderUserName,
                  renderGravatarHash) where

import Data.Text.Lazy

import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Data.Maybe
import Data.String

import Data.Aeson
import GHC.Generics (Generic)
import Control.Applicative

import Data.Time.Clock
import Data.Time.Format

import Web.Scotty (Parsable (..), readEither)

newtype NodeId = NodeId String
    deriving (Eq, Show, Generic)
             
renderNodeId :: NodeId -> String
renderNodeId (NodeId x) = x
                        
newtype UserId = UserId String
    deriving (Eq, Show, Generic, Ord)
             
renderUserId :: UserId -> String
renderUserId (UserId x) = x
                        
newtype UserName = UserName String
    deriving (Eq, Show, Generic, Ord)
             
renderUserName :: UserName -> String
renderUserName (UserName x) = x
                            
newtype GravatarHash = GravatarHash String
    deriving (Eq, Show, Generic)
             
renderGravatarHash :: GravatarHash -> String
renderGravatarHash (GravatarHash x) = x
                        
newtype NodeContent = NodeContent Text
    deriving (Eq, Show, Generic)

data Event = NodeAdded NodeId NodeId NodeContent UserId UTCTime
           | UserRegistered UserId
           | UserNameSet UserId UserName
           | UserGravatarHashSet UserId GravatarHash
             deriving (Eq, Show, Generic)

instance ToJSON NodeId
instance ToJSON NodeContent
instance ToJSON UserId
instance ToJSON UserName
instance ToJSON GravatarHash

instance FromJSON NodeId
instance FromJSON NodeContent
instance FromJSON UserId
instance FromJSON UserName
instance FromJSON GravatarHash

instance ToJSON Event where
  toJSON (NodeAdded x y z userId creationDate) =
    object ["eventType" .= ("NodeAdded" :: String),
            "aggregateId" .= x,
            "parentId" .= y,
            "content" .= z,
            "userId" .= userId,
            "creationDate" .= creationDate]
    
  toJSON (UserRegistered userId) =
    object ["eventType" .= ("UserRegistered" :: String),
            "aggregateId" .= userId]
    
  toJSON (UserNameSet userId userName) =
    object ["eventType" .= ("UserNameSet" :: String),
            "aggregateId" .= userId,
            "userName" .= userName]

  toJSON (UserGravatarHashSet userId gravatarHash) =
    object ["eventType" .= ("UserGravatarHashSet" :: String),
            "aggregateId" .= userId,
            "gravatarHash" .= gravatarHash]

instance FromJSON Event where
  parseJSON (Object v) = 
    do eventType <- v .: "eventType"
       case eventType :: String of
         "NodeAdded" -> parseNodeAdded v
         "UserRegistered" -> parseUserRegistered v
         "UserNameSet" -> parseUserNameSet v
         "UserGravatarHashSet" -> parseUserGravatarHashSet v
         _ -> fail "unhandled event type"
    
parseNodeAdded v = 
  NodeAdded <$>
            (NodeId <$> v .: "aggregateId") <*>
            (NodeId <$> v .: "parentId") <*>
            v .: "content" <*>
            (UserId <$> v .: "userId") <*>
            v .: "creationDate"

parseUserRegistered v = 
  UserRegistered <$>
            (UserId <$> v .: "aggregateId")

parseUserNameSet v = 
  UserNameSet <$>
            (UserId <$> v .: "aggregateId") <*>
            (UserName <$> v .: "userName")

parseUserGravatarHashSet v = 
  UserGravatarHashSet <$>
            (UserId <$> v .: "aggregateId") <*>
            (GravatarHash <$> v .: "gravatarHash")
