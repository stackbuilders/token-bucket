{-|
Module      : Network.TokenBucket.Server
Description : Algorithm for token bucket server.
Copyright   : (c) Stack Builders Inc. 2014

License     : MIT
Maintainer  : justin@stackbuilders.com
Stability   : experimental
Portability : unknown

Implements logic for removing and adding (refilling)
tokens in the token bucket.
-}
module Network.TokenBucket.Server (launch) where

import System.IO
  ( IOMode(..)
  , BufferMode(..)
  , hPutStrLn
  , hGetLine
  , hClose
  , hSetBuffering )

import Control.Monad (liftM)
import Control.Monad.Fix (fix)
import Data.Attoparsec.Text (parseOnly)
import Control.Exception (SomeException(..), handle)

import Network.Socket
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Network.TokenBucket.Parser (Command(..), commandParser)

-- | Returns some computed properties for the bucket.
bucketWithProperties ::
  (T.Text, Int) -- ^ Bucket name, desired rate.
  -> (T.Text, Int, Int) -- ^ Bucket name, refill delay and max size.
bucketWithProperties (bucket, rate) =
  ( bucket, computeDelay rate, rate)
  where computeDelay r = ceiling $ 1000000 / (fromIntegral r :: Double)

launch :: PortNumber -> [(T.Text, Int)] -> IO ()
launch port config = do
  putStrLn $ "\nActivating token bucket server on port " ++ show port
    ++ " with configuration:\n"

  putStrLn $ show config ++ "\n"

  tks <- newMVar $ Map.fromList config

  let bktWithProperties = map bucketWithProperties config

  putStrLn
    "Buckets with properties (name, refill delay in microseconds, max size):\n"

  print bktWithProperties
  putStrLn "\n"

  _ <- mapM_ (forkIO . refillLoop tks) bktWithProperties

  sock <- socket AF_INET Stream 0

  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  listen sock 2

  mainLoop sock tks

-- | Algorithm for topping up a given bucket by adding to the MVar up
-- to the specified limit.
refillLoop :: MVar (Map.Map T.Text Int) -- ^ Main state structure for threads.
           -> (T.Text, Int, Int) -- ^ Tuple with bucket name, delay and max size.
           -> IO ()
refillLoop toks (bucket, delay, maxTokens) = do
  tks <- takeMVar toks

  let adjustedBuckets =
        case Map.lookup bucket tks of
          Nothing -> tks

          Just tokCount ->
            Map.insert bucket (addToBucket maxTokens tokCount) tks

  putMVar toks adjustedBuckets

  threadDelay delay

  refillLoop toks (bucket, delay, maxTokens)

-- | Tops up the bucket specified, up to bucket limit.
addToBucket :: Int    -- ^ Max tokens in the given bucket
            -> Int    -- ^ Number of tokens currently in bucket
            -> Int    -- ^ The number of tokens in bucket after topping up
addToBucket maxTokens currentTokens =
  if currentTokens >= maxTokens then currentTokens else currentTokens + 1

mainLoop :: Socket -> MVar (Map.Map T.Text Int) -> IO ()
mainLoop sock tks = do
    conn <- accept sock
    _    <- forkIO (runConn conn tks)

    mainLoop sock tks

decrementToken :: Int -> Int
decrementToken n = if n > 0 then n - 1 else 0

getToken :: T.Text -> MVar (Map.Map T.Text Int) -> IO (Maybe Bool)
getToken bucket toks = do
  tks <- takeMVar toks

  case Map.lookup bucket tks of
    Nothing  -> do
      putMVar toks tks
      return Nothing

    Just num -> do
      putMVar toks $ Map.insert bucket (decrementToken num) tks
      return $ Just (couldObtainToken num)

  where couldObtainToken n = n > 0


runConn :: (Socket, SockAddr) -> MVar (Map.Map T.Text Int) -> IO ()
runConn (sock, _) toks = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
      line <- liftM init (hGetLine hdl)

      case parseOnly commandParser (T.pack line) of
        Right (Get str) -> do
          res <- getToken str toks

          case res of
            Just True  -> hPutStrLn hdl "1" -- token obtained
            Just False -> hPutStrLn hdl "0" -- empty bucket

            Nothing    -> hPutStrLn hdl "BUCKET NOT FOUND"

          loop

        Right Quit -> hPutStrLn hdl "BYE"

        Left _ -> do
          hPutStrLn hdl "INVALID COMMAND"
          loop

    hClose hdl
