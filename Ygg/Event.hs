{-# LANGUAGE DeriveGeneric #-}

module Ygg.Event where

import Data.Text.Lazy (Text)

import Data.Aeson (ToJSON (..), FromJSON (..))
import GHC.Generics (Generic)

newtype NodeId = NodeId Integer
               deriving (Eq, Show, Generic)
                        
newtype NodeContent = NodeContent Text
                    deriving (Eq, Show, Generic)

data Event = NodeAdded NodeId NodeId NodeContent
           deriving (Eq, Show, Generic)

instance ToJSON NodeId
instance ToJSON NodeContent
instance ToJSON Event
instance FromJSON NodeId
instance FromJSON NodeContent
instance FromJSON Event
