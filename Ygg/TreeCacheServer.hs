module Ygg.TreeCacheServer where

import Ygg.Event
import Ygg.TreeCache

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

import qualified Network.AMQP as AMQP

import System.Log.Logger
import System.Log.Handler.Syslog

newtype TreeCacheServer = 
  TreeCacheServer { latestYggdrasil :: TVar Yggdrasil }

getYggdrasil :: TreeCacheServer -> IO Yggdrasil
getYggdrasil = atomically . readTVar . latestYggdrasil

start :: [Event] -> IO TreeCacheServer
start pastEvents = do
  
  server <- fmap TreeCacheServer $ newTVarIO (growTree pastEvents)
  
  amqpConnection <- AMQP.openConnection "127.0.0.1" "/" "guest" "guest"
  amqpChannel <- AMQP.openChannel amqpConnection

  AMQP.declareQueue amqpChannel $ 
    AMQP.newQueue { AMQP.queueName = "tree-cache" }
    
  AMQP.declareExchange amqpChannel $ 
    AMQP.newExchange { AMQP.exchangeName = "yggdrasil"
                     , AMQP.exchangeType = "direct" }
    
  AMQP.bindQueue amqpChannel "tree-cache" "yggdrasil" "event"
  
  debugM "Ygg.TreeCacheServer" $ "Listening for events"

  AMQP.consumeMsgs amqpChannel "tree-cache" AMQP.Ack $
    updateTreeCache server
    
  return server

updateTreeCache :: TreeCacheServer -> (AMQP.Message, AMQP.Envelope) -> IO ()
updateTreeCache t (m, env) = do
  debugM "Ygg.TreeCache" "Received event"
  let (Just e) = JSON.decode (AMQP.msgBody m)
  modifyTreeCache t (flip processEvent e)
  AMQP.ackEnv env
  
modifyTreeCache :: TreeCacheServer -> (Yggdrasil -> Yggdrasil) -> IO ()
modifyTreeCache t f = atomically $ modifyTVar' (latestYggdrasil t) f
