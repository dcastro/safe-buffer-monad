{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module SafeBufferConcurrentTSpec where

import           Control.Concurrent
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           SafeBuffer
import           Test.Hspec

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

      it "handles sync exceptions" $ do
        buffer <- execBufferConcurrently $ do
          writeBuffer [1]
          void $ error "yo"
          writeBuffer [2]
        buffer `shouldBe` [1]

      it "handles async exceptions" $ do
        buffer <- execBufferConcurrently $ do
          writeBuffer [1]

          threadId <- liftIO $ myThreadId
          void $ liftIO . forkIO $ 
            liftIO $ throwTo threadId ThreadKilled
          liftIO . threadDelay $ 3 * 1000 * 1000

          writeBuffer [2]
        buffer `shouldBe` [1]

    describe "runBufferConcurrently" $ do
      it "handles sync exceptions" $ do
        bufferCopy <- newEmptyMVar
        let action = runBufferConcurrently (putMVar bufferCopy) $ do
                        writeBuffer [1]
                        void $ throw $ userError "oops"
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
        throwTo threadId ThreadKilled
        xs <- takeMVar bufferCopy
        xs `shouldBe` [1]

    describe "tryRunBufferConcurrently" $ do
      it "handles sync exceptions" $ do
        (tryRunBufferConcurrently $ do
          writeBuffer [1]
          void $ throw $ userError "oops"
          writeBuffer [2]
          ) `shouldReturn` ([1], Left (userError "oops"))
          
      it "handles async esceptions" $ do
        (tryRunBufferConcurrently @IO @AsyncException $ do
          writeBuffer [1]

          threadId <- liftIO $ myThreadId
          void $ liftIO . forkIO $ 
            liftIO $ throwTo threadId ThreadKilled
          liftIO . threadDelay $ 3 * 1000 * 1000

          writeBuffer [2]
          ) `shouldReturn` ([1], Left ThreadKilled)

