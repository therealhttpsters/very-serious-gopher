{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad

import System.Environment
import System.Directory

import Network.Socket

import qualified Data.ByteString         as BS
import qualified Data.Char         as DC

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

postRow :: T.Text -> PortNumber -> T.Text  -> T.Text -> Record
postRow host port selector name =  Record { recName = name
      , recSelector = selector
      , recHost = host
      , recPort = port
      , recOther = []
      }

getSinglePost :: T.Text -> PortNumber -> [([Char], [Char])] -> BS.ByteString -> [Listing]
getSinglePost host port table s = let
   filename = map (DC.chr . fromIntegral) $ BS.unpack $ BS.filter (\c -> c /= 13) s
   posts = filter (\(name, _) -> name == filename) table
   postContent = case posts of
                  [] -> "Not found"
                  p  -> (snd . head) p
   in
      linesToMOTD host port $ splitOn "\n" postContent


appBuilder :: T.Text -> PortNumber -> [Listing] -> [([Char], [Char])] -> GopherApp
appBuilder host port motd lookupTable = let
      row = postRow host port
      back = Listing Directory $ row "" "Back"
      postList = map (\(name, _) -> Listing Directory $ row (T.pack name) (T.pack name)) $ lookupTable
      postIndex = [Listing Info $ row "" "Index", Listing Info $ row "" ""] ++ postList
   in
      \selector -> case selector of 
                        "\r" -> pure $ Items (motd ++ postIndex)
                        s -> pure $ Items $ (getSinglePost host port lookupTable s) ++ [back]

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
