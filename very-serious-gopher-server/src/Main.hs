{-# LANGUAGE OverloadedStrings #-}

import Network.Gopher
import Control.Concurrent
import Control.Monad
-- import Control.Exception

app :: GopherApp
app _ = let
      k =  Record { recName = "Very Serious Things are Coming"
      , recSelector = "AAAA"
      , recHost = "localhost"
      , recPort = 8080
      , recOther = []
      }
   in
      pure $ Items [Listing PlainText k]

main :: IO ()
main = do
   runGopherApp 8080 app
   (forever (threadDelay 1))
