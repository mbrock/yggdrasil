module Ygg.EventBus (
  EventBus,

  start,
  pushEvent

  ) where

import Ygg.Event

import System.Log.Logger

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

import qualified Network.AMQP as AMQP

data EventBus = EventBus { eventStoreAmqpChannel :: AMQP.Channel }

pushEvent :: EventBus -> Event -> IO ()
pushEvent es@(EventBus amqp) e = do
  debugM "Ygg.EventBus" $ "Pushing " ++ show e
  publishMessage amqp e
  
publishMessage :: AMQP.Channel -> Event -> IO ()
publishMessage amqp e =  
  AMQP.publishMsg amqp "yggdrasil" "event"
      AMQP.newMsg { AMQP.msgBody = JSON.encode e
                  , AMQP.msgDeliveryMode = Just AMQP.Persistent }

start :: IO EventBus 
start = do
  amqpConnection <- AMQP.openConnection "127.0.0.1" "/" "guest" "guest"
  amqpChannel <- AMQP.openChannel amqpConnection

  AMQP.declareQueue amqpChannel $
      AMQP.newQueue { AMQP.queueName = "events" }
    
  AMQP.declareExchange amqpChannel $ 
      AMQP.newExchange { AMQP.exchangeName = "yggdrasil"
                       , AMQP.exchangeType = "direct" }
      
  AMQP.bindQueue amqpChannel "events" "yggdrasil" "event"

  return $ EventBus amqpChannel
