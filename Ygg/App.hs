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

type Action =
    WriterT [Event]
    (ReaderT (Yggdrasil, Maybe SessionId)
     (StateT ServerState IO))

runAction :: MonadIO m => Yggdrasil -> Maybe SessionId 
             -> TVar ServerState -> EventBus -> Action a -> m a
runAction y sessionId stateVar bus action = liftIO $ do
  state <- atomically $ readTVar stateVar
  ((a, w), state') <- runStateT (runReaderT (runWriterT action)
                                  (y, sessionId)) state
  mapM_ (Ygg.EventBus.pushEvent bus) w
  atomically $ writeTVar stateVar state'
  return a
  
writeEvent :: Event -> Action ()
writeEvent e = tell [e]

getUserIdForSession :: String -> Action (Maybe UserId)
getUserIdForSession sessionId =
  fmap ((Map.lookup sessionId) . yggSessionMap) getState
  
getState = lift $ lift get

getYggdrasil = lift $ ask >>= return . fst
getSessionId = lift $ ask >>= return . snd

getLoggedInUserId = do
  session <- getSessionId
  maybe (return Nothing) getUserIdForSession session

getIdForUser :: UserName -> Action (Maybe UserId)
getIdForUser userName = do
  y <- getYggdrasil
  return $ Map.lookup userName (Ygg.TreeCache.yggUserNames y)

addUser :: UserName -> UserId -> Action ()
addUser k v = lift . lift $ modify f
    where f y = y { yggUserMap = Map.insert k v (yggUserMap y) }

addUserSession :: String -> UserId -> Action ()
addUserSession k v = lift . lift $ modify f
    where f y = y { yggSessionMap = Map.insert k v (yggSessionMap y) }
