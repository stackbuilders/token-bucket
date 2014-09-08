{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.TokenBucket.Parser
Description : Parser for TokenBucket
Copyright   : (c) Stack Builders Inc. 2014

License     : MIT
Maintainer  : justin@stackbuilders.com
Stability   : experimental
Portability : unknown

Parses commands and configuration for the TokenBucket
server using Attoparsec.
-}
module Network.TokenBucket.Parser ( Command(..)
                                  , commandParser
                                  , parseConfig)
where

import Data.Attoparsec.Text
import Data.Scientific (Scientific)
import Control.Applicative ((<|>), (*>))
import Data.Char (isSpace)

import qualified Data.Text as T

data Command = Get T.Text | Quit deriving Show

bucketNameParser :: Parser T.Text
bucketNameParser = takeWhile1 (not . isSpace)

getParser :: Parser Command
getParser = do
  _ <- string "get" *> takeWhile1 isSpace

  bucketName <- bucketNameParser
  endOfInput

  return $ Get bucketName

quitParser :: Parser Command
quitParser = string "quit" *> endOfInput >> return Quit

commandParser :: Parser Command
commandParser = getParser <|> quitParser

parseConfig :: String -> Either String [(T.Text, Int)]
parseConfig contents =
  parseOnly configFile (T.pack contents) >>= validateConfigTuples

configFileLine :: Parser (T.Text, Scientific)
configFileLine = do
  bucketName <- bucketNameParser
  skipSpace
  rate <- scientific
  endOfLine <|> endOfInput
  return (bucketName, rate)

configFile :: Parser [(T.Text, Scientific)]
configFile = do
  ls <- many1 configFileLine
  endOfInput
  return ls

validateConfigTuples :: [(T.Text, Scientific)] -> Either String [(T.Text, Int)]
validateConfigTuples =
  mapM convertTuple

  where convertTuple :: (T.Text, Scientific) -> Either String (T.Text, Int)
        convertTuple (t, s) =
          case verifyRateNumber s of
            Nothing ->
              Left $ "Invalid rate for bucket " ++ T.unpack t ++ ": " ++ show s

            Just i -> Right (t, i)

verifyRateNumber :: Scientific -> Maybe Int
verifyRateNumber n =
  if n <= 1000000 && n > 0 && isInt n then Just (round n :: Int) else Nothing

  where isInt x = x == fromInteger (round x)
