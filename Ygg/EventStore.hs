module Ygg.EventStore (start, getAllEvents) where

import Ygg.Event

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

import qualified Network.AMQP as AMQP

import System.Log.Logger
import System.Log.Handler.Syslog

newtype EventStore = EventStore { allEvents :: TVar [Event] }

eventLogPath = "events.json"

start :: IO EventStore
start = do
  
  events <- readPersistedEvents
  store <- fmap EventStore (newTVarIO events)

  amqpConnection <- AMQP.openConnection "127.0.0.1" "/" "guest" "guest"
  amqpChannel <- AMQP.openChannel amqpConnection

  AMQP.declareQueue amqpChannel $ 
    AMQP.newQueue { AMQP.queueName = "event-store" }
    
  AMQP.declareExchange amqpChannel $ 
    AMQP.newExchange { AMQP.exchangeName = "yggdrasil"
                     , AMQP.exchangeType = "direct" }
    
  AMQP.bindQueue amqpChannel "event-store" "yggdrasil" "event"
  
  debugM "Ygg.EventStore" $ "Listening for events"

  AMQP.consumeMsgs amqpChannel "event-store" AMQP.Ack $
    saveEventFromMessage store
    
  return store

storeEvents :: EventStore -> IO ()
storeEvents (EventStore v) = do
  debugM "Ygg.EventStore" $ "Persisting all events"
  BS.writeFile eventLogPath . JSON.encode =<< atomically (readTVar v)

saveEvent :: EventStore -> Event -> IO ()
saveEvent (EventStore v) e = do
  debugM "Ygg.EventStore" $ "Locally recording event " ++ show e
  atomically $ do
    es <- readTVar v
    writeTVar v (es ++ [e])

saveEventFromMessage :: EventStore -> (AMQP.Message, AMQP.Envelope) -> IO ()
saveEventFromMessage es (m, env) = do
  debugM "Ygg.EventStore" "Received event"
  let (Just e) = JSON.decode (AMQP.msgBody m)
  saveEvent es e
  storeEvents es
  AMQP.ackEnv env

getAllEvents :: EventStore -> IO [Event]
getAllEvents = atomically . readTVar . allEvents

readPersistedEvents :: IO [Event]
readPersistedEvents = do
  Just values <- JSON.decode `fmap` BS.readFile eventLogPath
  
  debugM "Ygg.EventStore" $
    "Read " ++ show (length values) ++ " persisted events"
    
  forM values (\v ->
         case JSON.fromJSON v of
           JSON.Error x    -> fail x
           JSON.Success e  -> return e)