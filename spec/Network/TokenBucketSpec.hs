module Network.TokenBucketSpec (spec) where

import Test.Hspec
import Data.Text (pack)
import Control.Concurrent (forkIO, threadDelay)

import qualified Network.TokenBucket.Server as S
import qualified Network.TokenBucket.Client as C

import Data.Pool (Pool(..))
import GHC.IO.Handle (Handle)

spec :: Spec
spec = do
  describe "Starting the server and requesting a token" $
    beforeAll launchServer $ do
      it "retrieves tokens from the bucket" $ \pool -> do
        res  <- C.get pool "foo"
        res `shouldBe` (Right True)

      it "receives 'BUCKET NOT FOUND' if an invalid bucket name is given" $
        \pool -> do

          res  <- C.get pool "badkey"
          res `shouldBe` (Left "BUCKET NOT FOUND")


launchServer :: IO (Pool Handle)
launchServer = do
  _ <- forkIO $ S.launch 4242 [(pack "foo", 10)]

  -- This is unfortunate, but we want to wait for the server to spin
  -- up before making queries.
  threadDelay 1000000

  C.connect "localhost" 4242
