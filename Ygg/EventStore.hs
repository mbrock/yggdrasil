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

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

type EventChannel = TChan Event

data EventStore = EventStore (TVar [Event]) EventChannel

eventLogPath = "events.json"

saveEvents :: EventStore -> IO ()
saveEvents (EventStore v _) =
  BS.writeFile eventLogPath . JSON.encode =<< atomically (readTVar v)

pushEvent :: EventStore -> Event -> IO ()
pushEvent es@(EventStore v c) e = do
  atomically $ do
    es <- readTVar v
    writeTVar v (es ++ [e])
    writeTChan c e
  saveEvents es

initializeEventStore :: IO EventStore
initializeEventStore = do
  Just events <- JSON.decode `fmap` BS.readFile eventLogPath
  v <- newTVarIO events
  c <- newTChanIO
  return $ EventStore v c

chanForNewEvents :: EventStore -> IO (TChan Event)
chanForNewEvents (EventStore _ c) = atomically $ dupTChan c

getAllEvents :: EventStore -> IO [Event]
getAllEvents (EventStore v _) = atomically $ readTVar v
