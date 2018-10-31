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

topRow :: T.Text -> PortNumber -> T.Text -> Record
topRow host port name =  Record { recName = name
      , recSelector = name
      , recHost = host
      , recPort = port
      , recOther = []
      }


appBuilder :: T.Text -> PortNumber -> [Listing] -> GopherApp
appBuilder host port motd = let
      postNames = (filter (\x -> (T.isSuffixOf ".post") (T.pack x)) ) <$> (getDirectoryContents "./data")
      postContent = (mapM readFile) =<< (map (\x -> "./data/" ++ x)) <$> postNames
      lookupTable = zip <$> postNames <*> postContent
      postIndex = map (\(name, _) -> Listing PlainText $ ((topRow host port) . T.pack) name) <$> lookupTable
   in
      \_ -> Items <$> liftA2 (++) (pure motd) postIndex

main :: IO ()
main = do
   dataPath <- getEnv "GOPHER_POST_DATA"
   serverHost <- T.pack <$> getEnv "GOPHER_HOST"
   serverPort <- (\port -> read port :: PortNumber) <$> getEnv "GOPHER_PORT"
   motd <- linesToMOTD serverHost serverPort <$> splitOn "\n" <$> readFile "./motd.txt"
   putStrLn $ dataPath
   putStrLn $ show serverHost
   putStrLn $ show serverPort
   runGopherApp serverPort $ appBuilder serverHost serverPort motd
   (forever (threadDelay 1))
