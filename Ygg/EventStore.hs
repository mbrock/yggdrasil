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

import qualified Network.AMQP as AMQP

type EventChannel = TChan Event

data EventStore = EventStore (TVar [Event]) EventChannel AMQP.Channel

eventLogPath = "events.json"

saveEvents :: EventStore -> IO ()
saveEvents (EventStore v _ _) =
  BS.writeFile eventLogPath . JSON.encode =<< atomically (readTVar v)

pushEvent :: EventStore -> Event -> IO ()
pushEvent es@(EventStore v c amqp) e = do
  atomically $ do
    es <- readTVar v
    writeTVar v (es ++ [e])
    writeTChan c e
  saveEvents es
  AMQP.publishMsg amqp "yggdrasil" "event"
      AMQP.newMsg { AMQP.msgBody = JSON.encode e
                  , AMQP.msgDeliveryMode = Just AMQP.Persistent }

initializeEventStore :: IO EventStore
initializeEventStore = do
  Just events <- JSON.decode `fmap` BS.readFile eventLogPath
  v <- newTVarIO events
  c <- newTChanIO

  amqpConnection <- AMQP.openConnection "127.0.0.1" "/" "guest" "guest"
  amqpChannel <- AMQP.openChannel amqpConnection

  AMQP.declareQueue amqpChannel AMQP.newQueue { AMQP.queueName = "events" }
  AMQP.declareExchange amqpChannel AMQP.newExchange { AMQP.exchangeName = "yggdrasil"
                                                    , AMQP.exchangeType = "direct" }
  AMQP.bindQueue amqpChannel "events" "yggdrasil" "event"

  return $ EventStore v c amqpChannel

chanForNewEvents :: EventStore -> IO (TChan Event)
chanForNewEvents (EventStore _ c _) = atomically $ dupTChan c

getAllEvents :: EventStore -> IO [Event]
getAllEvents (EventStore v _ _) = atomically $ readTVar v
