{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad
import System.Environment
import Network.Socket
import Data.List.Split

import qualified Data.Text          as T

linesToMOTD :: T.Text -> PortNumber  -> [[Char]] -> [Listing]
linesToMOTD host port messageLines = let
   toListing = \x -> Listing Info Record { recName = T.pack x
      , recSelector = ""
      , recHost = host
      , recPort = port
      , recOther = []
      }
   in
      map toListing messageLines

appBuilder :: T.Text -> PortNumber -> GopherApp
appBuilder host port = let
      topRow =  Record { recName = "Very Serious Things are Coming"
      , recSelector = ""
      , recHost = host
      , recPort = port
      , recOther = []
      }

      title = [Listing Info topRow]
      motd = splitOn "\n" <$> readFile "./motd.txt"
   in
      \_ -> Items <$> (\l -> title ++ (linesToMOTD host port l)) <$> motd

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
