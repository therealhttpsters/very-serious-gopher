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


appBuilder :: T.Text -> PortNumber -> [Listing] -> [([Char], [Char])] -> GopherApp
appBuilder host port motd lookupTable = let
      postIndex = map (\(name, _) -> Listing PlainText $ ((topRow host port) . T.pack) name) $ lookupTable
   in
      \_ -> Items <$> liftA2 (++) (pure motd) (pure postIndex)

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
   lookupTable <- liftA2 zip (pure postNames) (pure postContent)

   -- Run the Server
   runGopherApp serverPort $ appBuilder serverHost serverPort motd lookupTable
   (forever (threadDelay 1000))
