{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad
import Control.Applicative

import System.Environment
import System.Directory

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
      topRow name =  Record { recName = name
      , recSelector = name
      , recHost = host
      , recPort = port
      , recOther = []
      }

      postNames = (filter (\x -> (T.isSuffixOf ".post") (T.pack x)) ) <$> (getDirectoryContents "./data")
      postContent = (mapM readFile) =<< (map (\x -> "./data/" ++ x)) <$> postNames
      lookupTable = zip <$> postNames <*> postContent

      motd = linesToMOTD host port <$> splitOn "\n" <$> readFile "./motd.txt"
      postIndex = map (\(name, _) -> Listing PlainText $ (topRow . T.pack) name) <$> lookupTable
   in
      \_ -> Items <$> liftA2 (++) motd postIndex

main :: IO ()
main = do
   dataPath <- getEnv "GOPHER_POST_DATA"
   serverHost <- T.pack <$> getEnv "GOPHER_HOST"
   serverPort <- (\port -> read port :: PortNumber) <$> getEnv "GOPHER_PORT"
   putStrLn $ dataPath
   putStrLn $ show serverHost
   putStrLn $ show serverPort
   runGopherApp serverPort $ appBuilder serverHost serverPort
   (forever (threadDelay 1))
