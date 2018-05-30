{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module SafeBufferConcurrentTSpec where

import           Control.Concurrent
import           Control.Exception.Base (ErrorCall)
import           Control.Monad
import           Control.Monad.IO.Class
import           SafeBuffer
import           Test.Hspec
import           UnliftIO.Exception     (IOException, throwIO)

spec :: Spec
spec =
  describe "SafeBufferConcurrentT" $ do
    it "writes to buffer" $
      (tryRunBufferConcurrently @IO @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        writeBuffer [3]
        pure "done"
      ) `shouldReturn` ([1,2,3], Right "done")

    it "reads from buffer" $
      (tryRunBufferConcurrently @IO @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        buffer <- readBuffer
        pure (sum buffer)
      ) `shouldReturn` ([1,2], Right 3)

    it "clears buffer" $
      (tryRunBufferConcurrently @IO @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        buffer <- clearBuffer
        writeBuffer [0]
        pure (sum buffer)
      ) `shouldReturn` ([0], Right 3)

    describe "execBufferConcurrently" $ do
      it "catches exceptions" $ do
        buffer <- execBufferConcurrently @_ @ErrorCall $ do
          writeBuffer [1]
          void $ error "yo"
          writeBuffer [2]
        buffer `shouldBe` [1]

    describe "tryRunBufferConcurrently" $ do
      it "returns exceptions" $ do
        (tryRunBufferConcurrently $ do
          writeBuffer [1]
          void . throwIO $ userError "oops"
          writeBuffer [2]
          ) `shouldReturn` ([1], Left (userError "oops"))
    
    describe "runBufferConcurrently" $ do
      it "handles sync exceptions" $ do
        bufferCopy <- newEmptyMVar
        let action = runBufferConcurrently (putMVar bufferCopy) $ do
                        writeBuffer [1]
                        void . throwIO $ userError "oops"
                        writeBuffer [2]
                        pure 8
        action `shouldThrow` \x -> x == userError "oops"
        xs <- takeMVar bufferCopy
        xs `shouldBe` [1]

      it "handles async exceptions" $ do
        bufferCopy <- newEmptyMVar
        threadId <- forkIO $ 
          runBufferConcurrently (putMVar bufferCopy) $ do
            writeBuffer [1]
            liftIO . threadDelay $ 3 * 1000 * 1000
            writeBuffer [2]
        killThread threadId
        xs <- takeMVar bufferCopy
        xs `shouldBe` [1]
