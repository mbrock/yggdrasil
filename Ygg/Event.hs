{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Ygg.Event (Event (..),
                  NodeId (..),
                  UserId (..),
                  NodeContent (..)) where

import Data.Text.Lazy

import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Data.Maybe

import Data.Aeson
import GHC.Generics (Generic)
import Control.Applicative

import Data.Time.Clock
import Data.Time.Format

newtype NodeId = NodeId UUID
               deriving (Eq, Show, Generic)
                        
newtype UserId = UserId String
               deriving (Eq, Show, Generic)
                        
newtype NodeContent = NodeContent Text
                    deriving (Eq, Show, Generic)

data Event = NodeAdded NodeId NodeId NodeContent UserId UTCTime
           deriving (Eq, Show, Generic)

instance ToJSON NodeId
instance ToJSON NodeContent
instance ToJSON UserId

instance ToJSON Event where
  toJSON (NodeAdded x y z userId creationDate) =
    object ["nodeId" .= x,
            "parentId" .= y,
            "content" .= z,
            "userId" .= userId,
            "creationDate" .= creationDate]

instance FromJSON NodeId
instance FromJSON NodeContent
instance FromJSON UserId

instance FromJSON Event where
  parseJSON (Object v) = NodeAdded <$>
                         v .: "nodeId" <*>
                         v .: "parentId" <*>
                         v .: "content" <*>
                         v .: "userId" <*>
                         v .: "creationDate"

instance ToJSON UUID where
  toJSON = toJSON . UUID.toString
  
instance FromJSON UUID where
  parseJSON (String s) = 
    return $ fromJust (UUID.fromString $ unpack $ fromStrict s)