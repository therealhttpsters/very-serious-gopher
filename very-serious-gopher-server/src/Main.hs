{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad

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

postRow :: T.Text -> PortNumber -> T.Text -> Record
postRow host port name =  Record { recName = name
      , recSelector = name
      , recHost = host
      , recPort = port
      , recOther = []
      }


appBuilder :: T.Text -> PortNumber -> [Listing] -> [([Char], [Char])] -> GopherApp
appBuilder host port motd lookupTable = let
      postIndex = map (\(name, _) -> Listing PlainText $ ((postRow host port) . T.pack) name) $ lookupTable
   in
      \_ -> pure $ Items (motd ++ postIndex)

main :: IO ()
main = do
   -- Environment variables
   dataPath <- getEnv "GOPHER_POST_DATA"
   serverHost <- T.pack <$> getEnv "GOPHER_HOST"
   serverPort <- (\port -> read port :: PortNumber) <$> getEnv "GOPHER_PORT"

   -- Get post content into memory
   motd <- linesToMOTD serverHost serverPort <$> splitOn "\n" <$> readFile "./motd.txt"
   postNames <- (filter (\x -> (T.isSuffixOf ".post") (T.pack x)) ) <$> (getDirectoryContents dataPath)
   postContent <- (mapM readFile) (map (\x -> dataPath ++ x) postNames)
   lookupTable <- pure $ zip postNames postContent

   -- Run the Server
   runGopherApp serverPort $ appBuilder serverHost serverPort motd lookupTable
   forever (threadDelay 1000)
