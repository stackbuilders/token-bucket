{-|
Module      : Network.TokenBucket.Client
Description : Client for Token Bucket Server
Copyright   : (c) Stack Builders Inc. 2014

License     : MIT
Maintainer  : justin@stackbuilders.com
Stability   : experimental
Portability : unknown

Connects to a Token Bucket Server and requests tokens from the
specified bucket.
-}
module Network.TokenBucket.Client (connect, get) where

import GHC.IO.Handle ( Handle
                     , BufferMode(..)
                     , hGetLine
                     , hPutStr
                     , hClose
                     , hSetBuffering )

import Data.Pool (Pool(..), createPool, withResource)
import Network

-- | Connects to the given host, using a connection pool.
connect :: String
           -- ^ Host name running the token bucket server

        -> PortNumber
        -- ^ Port number the token bucket service is running on

        -> IO (Pool Handle)
        -- ^ A pool of handles to the token bucket server

connect host port =
  createPool (createConnection host port) destroyConnection 1 30 50


-- | Tries to get a token from the given bucket.
get :: Pool Handle -- ^ The pool of connections to the token bucket server

    -> String -- ^ The name of a token bucket

    -> IO (Either String Bool)
    -- ^ The result of the token bucket server request, or the String error from
    -- the server

get pool bucket = do
  withResource pool (requestToken bucket)


requestToken :: String -> Handle -> IO (Either String Bool)
requestToken bucket handle = do
  _ <- hPutStr handle $ "get " ++ bucket ++ "\r\n"
  line <- hGetLine handle

  return $ case line of
    "1" -> Right True
    "0" -> Right False
    _   -> Left line

createConnection :: String -> PortNumber -> IO Handle
createConnection host port = do
  hdl <- connectTo host (PortNumber port)
  hSetBuffering hdl LineBuffering
  return hdl

destroyConnection :: Handle -> IO ()
destroyConnection hdl = hClose hdl
