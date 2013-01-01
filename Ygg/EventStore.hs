module Ygg.EventStore (
  EventStore,
  EventChannel,
  
  initializeEventStore,
  pushEvent,
  chanForNewEvents,
  getAllEvents
  
  ) where

import Ygg.Event

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

type EventChannel = TChan Event

data EventStore = EventStore (TVar [Event]) EventChannel

eventLogPath = "events.json"

saveEvents :: EventStore -> IO ()
saveEvents (EventStore v _) = liftIO $
  atomically (readTVar v) >>= BS.writeFile eventLogPath . JSON.encode
  
pushEvent :: EventStore -> Event -> IO ()
pushEvent es@(EventStore v c) e = do 
  liftIO $ atomically $ do
    es <- readTVar v
    writeTVar v (es ++ [e])
    writeTChan c e
  saveEvents es

initializeEventStore :: IO EventStore
initializeEventStore =
  do file <- BS.readFile eventLogPath
     let (Just events) = JSON.decode file
     v <- newTVarIO events
     c <- newTChanIO
     return $ EventStore v c
     
chanForNewEvents :: EventStore -> IO (TChan Event)
chanForNewEvents (EventStore _ c) = atomically $ dupTChan c

getAllEvents :: EventStore -> IO [Event]
getAllEvents (EventStore v _) = atomically $ readTVar v
