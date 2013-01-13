{-# LANGUAGE NoMonomorphismRestriction #-}

module Ygg.App where

import Ygg.TreeCache
import Ygg.Event

import Ygg.EventBus (EventBus)
import qualified Ygg.EventBus

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

type SessionId = String

data ServerState = ServerState
    { yggSessionMap :: Map SessionId UserId
    , yggUserMap :: Map UserName UserId }

type YggAction =
    WriterT [Event]
    (ReaderT (Yggdrasil, Maybe SessionId)
     (StateT ServerState IO))

runAction :: MonadIO m => Yggdrasil -> Maybe SessionId 
             -> TVar ServerState -> EventBus -> YggAction a -> m a
runAction y sessionId stateVar bus action = liftIO $ do
  state <- atomically $ readTVar stateVar
  ((a, w), state') <- runAction' y sessionId state action
  pushEvents bus w
  atomically $ writeTVar stateVar state'
  return a
  
runAction' :: Yggdrasil -> Maybe SessionId -> ServerState -> YggAction a
              -> IO ((a, [Event]), ServerState)
runAction' y sessionId state m =
  runStateT (runReaderT (runWriterT m) (y, sessionId)) state
             
pushEvents :: EventBus -> [Event] -> IO ()             
pushEvents bus = mapM_ (Ygg.EventBus.pushEvent bus)
  
writeEvent :: Event -> YggAction ()
writeEvent e = tell [e]

getState :: YggAction ServerState
getState = lift $ lift get

getYggdrasil :: YggAction Yggdrasil
getYggdrasil = lift $ ask >>= return . fst

getSessionId :: YggAction (Maybe SessionId)
getSessionId = lift $ ask >>= return . snd

getUserIdForSession :: String -> YggAction (Maybe UserId)
getUserIdForSession sessionId =
  fmap ((Map.lookup sessionId) . yggSessionMap) getState

getLoggedInUserId :: YggAction (Maybe UserId)
getLoggedInUserId = do
  session <- getSessionId
  maybe (return Nothing) getUserIdForSession session

getIdForUser :: UserName -> YggAction (Maybe UserId)
getIdForUser userName = do
  y <- getYggdrasil
  return $ Map.lookup userName (Ygg.TreeCache.yggUserNames y)
  
addUser :: UserName -> UserId -> YggAction ()
addUser k v = lift . lift $ modify f
    where f y = y { yggUserMap = Map.insert k v (yggUserMap y) }

addUserSession :: String -> UserId -> YggAction ()
addUserSession k v = lift . lift $ modify f
    where f y = y { yggSessionMap = Map.insert k v (yggSessionMap y) }
