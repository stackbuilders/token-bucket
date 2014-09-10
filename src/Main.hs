import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import System.IO

import Network.TokenBucket.Server (launch)
import Network.TokenBucket.Parser (parseConfig)

main :: IO ()
main = do
  args <- getArgs

  if length args == 2 then
    do
      let portNum = fromIntegral (read (head args) :: Int)

      cfg <- readFile $ args !! 1

      case parseConfig cfg of
        Left err -> do
          hPutStrLn stderr err
          exitWith (ExitFailure 1)

        Right initialTokens -> launch portNum initialTokens

    else
      do
        hPutStrLn stderr
          "Server must be started with a port and config file path."

        exitWith (ExitFailure 1)
