module Ygg.WebSocketServer (
  start
) where

import Ygg.Event
import Ygg.EventStore

import qualified Data.Aeson as JSON
import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception

import Control.Monad (join, liftM, liftM2, forM_, forever)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy.Encoding
import Data.List (delete)

import qualified Data.Text.Lazy as T

type WebSocketsProtocol = WS.Hybi00

data Client = Client { clientId :: Integer
                     , clientSink :: WS.Sink WebSocketsProtocol }
              
instance Eq Client where
  a == b = clientId a == clientId b
              
data ServerState = ServerState { ssClients :: [Client]
                               , ssNextClientId :: Integer }

start :: EventStore -> IO ()
start eventStore = do
  state <- newMVar (ServerState [] 0)
  
  forkIO $ do
    channel <- chanForNewEvents eventStore
    forever $ broadcastNextEvent state channel
  
  WS.runServer "0.0.0.0" 9160 $ handleWebSocket eventStore state

broadcastNextEvent :: MVar ServerState -> EventChannel -> IO ()
broadcastNextEvent s c =
  join $ liftM2 broadcastEvent (readMVar s) (atomically $ readTChan c)

broadcastEvent :: ServerState -> Event -> IO ()
broadcastEvent s e = forM_ (map clientSink clients) send
  where clients = ssClients s
        send c = WS.sendSink c (WS.textData $ decodeUtf8 $ JSON.encode e)
                                         
handleWebSocket :: EventStore -> MVar ServerState -> WS.Request 
                   -> WS.WebSockets WebSocketsProtocol ()
handleWebSocket eventStore state rq = do
    WS.acceptRequest rq
    
    client <- registerNewClient state
    
    withClientDisconnectionHandler state client $ do
      sendAllEvents eventStore client
      forever $ ignoreDataFrom client
        
withClientDisconnectionHandler state client =
  flip WS.catchWsError (const $ liftIO $ removeClient state client)
        
sendAllEvents eventStore client = liftIO $ do 
  es <- getAllEvents eventStore
  forM_ es (sendEventToClient client)
        
sendEventToClient client event =
  broadcastEvent (ServerState [client] 0) event
  
ignoreDataFrom client = do
  WS.receiveData :: WS.WebSockets WebSocketsProtocol T.Text
  return ()

registerNewClient state = do
  newClientId <- fmap ssNextClientId (liftIO $ readMVar state)
  sink <- WS.getSink
  saveClient state (Client newClientId sink)
  
saveClient state client = liftIO $ do
  modifyMVar_ state
    (\ss -> return $ ss { ssClients = client : ssClients ss 
                        , ssNextClientId = clientId client + 1 })
  return client

removeClient :: MVar ServerState -> Client -> IO ()
removeClient state sink = modifyMVar_ state f
  where f ss = return $ ss { ssClients = delete sink (ssClients ss) }
