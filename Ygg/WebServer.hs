{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, 
             DeriveGeneric, ScopedTypeVariables #-}

module Ygg.WebServer where

import Ygg.Event

import qualified Ygg.EventStore

import Ygg.EventBus (EventBus)
import qualified Ygg.EventBus

import qualified Ygg.TreeCache

import Ygg.TreeCacheServer
    
import Ygg.App

import System.Log.Logger

import Control.Applicative

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Time.Clock (UTCTime, getCurrentTime)

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (static)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Control.Monad (liftM, mapM_)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text.Lazy as T
import qualified Data.Text as TT

newYggState = 
  ServerState { yggSessionMap = Map.empty 
              , yggUserMap = Map.empty }

makeActFunction :: TreeCacheServer ->
                   EventBus ->
                   TVar ServerState ->
                   (forall a. YggAction a -> ActionM a)
makeActFunction cache bus state m =
  do
    yggdrasil <- liftIO (Ygg.TreeCacheServer.getYggdrasil cache)
    sessionId <- readSessionId
    runAction yggdrasil sessionId state bus m

start :: Int -> IO ()
start port = do
  updateGlobalLogger "Ygg" (setLevel DEBUG)
  
  mapM_ putStrLn $ [""] ++
    map ("                " ++) [
      "                  v .   ._, |_  .,",
      "           `-._\\/  .  \\ /    |/_",
      "               \\\\  _\\, y | \\//",
      "         _\\_.___\\\\, \\\\/ -.\\||",
      "           `7-,--.`._||  / / ,",
      "           /'     `-. `./ / |/_.'",
      "                     |    |//",
      "                     |_    /",
      "                     |-   |",
      "                     |   =|",
      "                     |    |",
      "--------------------/ ,  . \\--------._"] ++
    map ("            " ++ ) [
      "",
      " _     __    __    ___   ___    __    __   _   _    ",
      "\\ \\_/ / /`_ / /`_ | | \\ | |_)  / /\\  ( (` | | | |   ",
      " |_|  \\_\\_/ \\_\\_/ |_|_/ |_| \\ /_/--\\ _)_) |_| |_|__ ",
      ""]
  
  eventBus <- Ygg.EventBus.start
  eventStore <- Ygg.EventStore.start
  allEvents <- Ygg.EventStore.getAllEvents eventStore
  treeCache <- Ygg.TreeCacheServer.start allEvents
  
  yggState <- atomically $ newTVar newYggState
    
  scotty port $ do
    middleware logStdoutDev
    middleware static
    
    let act = makeActFunction treeCache eventBus yggState
      
    post "/:parentId" $ \parentId ->
      act . postNode (NodeId parentId) =<< readContent

    post "/login/:username"     $ (json =<<) . act . loginAs . UserName
    post "/register/:username"  $ (json =<<) . act . registerUser . UserName
    post "/my-gravatar-hash"    $ readContent >>=  act . setMyGravatarHash

    get "/history" $
      json =<< liftIO (Ygg.EventStore.getAllEvents eventStore)
      
    get "/cache" $
      json =<< liftIO (Ygg.TreeCacheServer.getYggdrasil treeCache) 

    get "/" (redirect "/index.html")

postNode :: NodeId -> String -> YggAction ()
postNode parentId s = do
  let content = NodeContent $ T.pack s
  Just userId <- getLoggedInUserId
  nodeId <- NodeId <$> makeUUID
  creationDate <- liftIO getCurrentTime
      
  writeEvent $ NodeAdded nodeId parentId content userId creationDate
    
loginAs userName = do
  Just userId <- getIdForUser userName
  sessionId <- makeUUID
      
  addUserSession sessionId userId
    
  return (sessionId, userId)

registerUser userName = do
  sessionId <- makeUUID
  userId <- fmap UserId makeUUID
  
  addUser userName userId
  addUserSession sessionId userId
      
  writeEvent $ UserRegistered userId
  writeEvent $ UserNameSet userId userName
  
  return (sessionId, userId)

setMyGravatarHash content = do
  Just userId <- getLoggedInUserId
  writeEvent $ UserGravatarHashSet userId (GravatarHash content)

readContent = param $ T.pack "content"
readSessionId = rescue (fmap Just $ param $ T.pack "sessionId") 
                       (\_ -> return Nothing)

instance Parsable UUID.UUID where
  parseParam = readEither
  
makeUUID = liftIO $ fmap UUID.toString UUIDV4.nextRandom