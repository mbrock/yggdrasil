module Main where

import qualified Ygg.WebServer

port = 3000

main :: IO ()
main = Ygg.WebServer.start port
