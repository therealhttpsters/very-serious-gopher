{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad
import System.Environment
import Network.Socket

import qualified Data.Text          as T


appBuilder :: T.Text -> PortNumber -> GopherApp
appBuilder host port = let
      k =  Record { recName = "Very Serious Things are Coming"
      , recSelector = ""
      , recHost = host
      , recPort = port
      , recOther = []
      }
   in
      \_ -> pure $ Items [Listing PlainText k]

main :: IO ()
main = do
   dataPath <- getEnv "GOPHER_POST_DATA"
   serverHost <- T.pack <$> getEnv "GOPHER_HOST"
   serverPort <- (\port -> read port :: PortNumber) <$> getEnv "GOPHER_PORT"
   putStrLn dataPath
   putStrLn $ show serverHost
   putStrLn $ show serverPort
   runGopherApp serverPort $ appBuilder serverHost serverPort
   (forever (threadDelay 1))
