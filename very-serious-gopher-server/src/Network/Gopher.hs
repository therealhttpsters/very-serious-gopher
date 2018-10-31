{-# LANGUAGE BangPatterns
           , DeriveGeneric
           , OverloadedStrings
           #-}

module Network.Gopher (
    Record(..)
  , Item(..)
  , Listing(..)
  , Response(..)
  , GopherApp
  , runGopherApp
  ) where

import Control.Concurrent

import Control.Monad

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BU

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import GHC.Generics

import Network.Socket

import System.IO

import System.Timeout

data Record = Record {
    recName     :: !T.Text
  , recSelector :: !T.Text
  , recHost     :: !T.Text
  , recPort     :: !PortNumber
  , recOther    :: [T.Text]
  } deriving ( Eq
             , Show
             , Generic
             )

data Item = PlainText
          | Directory
          | CSOSearch
          | ErrorMessage
          | BinHexText
          | BinaryArc
          | UUEncoded
          | SearchEngine
          | TelnetPtr
          | BinaryFile
          | GIFImage
          | HTMLFile
          | Info
          | Image
          | Audio
          | TN3270Ptr
          deriving ( Eq
                   , Show
                   , Enum
                   , Generic
                   )

itemCode :: Item -> BU.Builder
itemCode PlainText    = BU.char7 '0'
itemCode Directory    = BU.char7 '1'
itemCode CSOSearch    = BU.char7 '2'
itemCode ErrorMessage = BU.char7 '3'
itemCode BinHexText   = BU.char7 '4'
itemCode BinaryArc    = BU.char7 '5'
itemCode UUEncoded    = BU.char7 '6'
itemCode SearchEngine = BU.char7 '7'
itemCode TelnetPtr    = BU.char7 '8'
itemCode BinaryFile   = BU.char7 '9'
itemCode GIFImage     = BU.char7 'g'
itemCode HTMLFile     = BU.char7 'h'
itemCode Info         = BU.char7 'i'
itemCode Image        = BU.char7 'l'
itemCode Audio        = BU.char7 's'
itemCode TN3270Ptr    = BU.char7 'T'

data Listing = Listing !Item !Record
             deriving ( Eq
                      , Show
                      , Generic
                      )

tabB :: BU.Builder
tabB = BU.char7 '\t'

listingBuilder :: Listing -> BU.Builder
listingBuilder (Listing i (Record n s h p os)) = itemCode i
                                              <> T.encodeUtf8Builder n
                                              <> tabB
                                              <> T.encodeUtf8Builder s
                                              <> tabB
                                              <> T.encodeUtf8Builder h
                                              <> tabB
                                              <> BU.string7 (show p)
                                              <> tabB
                                              <> T.encodeUtf8Builder
                                                  (T.intercalate "\t" os)

data Response = Items [Listing]
              | File BU.Builder

responseBuilder :: Response -> BU.Builder
responseBuilder (Items ls) =
    foldMap (\l -> listingBuilder l
                <> BU.string7 "\r\n"
            )
            ls
responseBuilder (File bu)  = bu

-- | TODO: Decode the request for the caller, even though it's so easy.
type GopherApp = BS.ByteString -> IO Response

-- | Run a Gopher app forever. Write the the returned MVar to gracefully shut
--   down.
runGopherApp :: PortNumber -> GopherApp -> IO (MVar ())
runGopherApp pn app = do
    sig  <- newEmptyMVar
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet pn iNADDR_ANY)
    listen sock 1024
    forkIO $ loop sock app sig
    pure sig

loop :: Socket -> GopherApp -> MVar () -> IO ()
loop sock app sig = do
    mconn <- timeout 1000000 $ accept sock
    maybe (pure ()) (void . forkIO . handleConn app) mconn
    done <- tryReadMVar sig
    maybe (loop sock app sig) (const (pure ())) done

handleConn :: GopherApp -> (Socket, SockAddr) -> IO ()
handleConn app (sock, _) = void $ timeout 180000000 $ do
    hdl <- socketToHandle sock ReadWriteMode
    req <- BS.hGetLine hdl
    resp <- responseBuilder <$> app req
    putStrLn $ show req
    BU.hPutBuilder hdl (resp <> BU.string7 ".\r\n")
    hClose hdl
