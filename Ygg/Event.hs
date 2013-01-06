{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Ygg.Event where

import Data.Text.Lazy

import Data.Aeson
import GHC.Generics (Generic)
import Control.Applicative

newtype NodeId = NodeId Integer
               deriving (Eq, Show, Generic)
                        
newtype UserId = UserId String
               deriving (Eq, Show, Generic)
                        
newtype NodeContent = NodeContent Text
                    deriving (Eq, Show, Generic)

data Event = NodeAdded NodeId NodeId NodeContent UserId
           deriving (Eq, Show, Generic)

instance ToJSON NodeId
instance ToJSON NodeContent
instance ToJSON UserId

instance ToJSON Event where
  toJSON (NodeAdded x y z userId) =
    object ["nodeId" .= x,
            "parentId" .= y,
            "content" .= z,
            "userId" .= userId]

instance FromJSON NodeId
instance FromJSON NodeContent
instance FromJSON UserId

instance FromJSON Event where
  parseJSON (Object v) = NodeAdded <$>
                         v .: "nodeId" <*>
                         v .: "parentId" <*>
                         v .: "content" <*>
                         v .: "userId"

                        
