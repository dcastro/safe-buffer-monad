{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module SafeBufferTSpec where

import           Control.Exception.Base (ErrorCall)
import           Control.Exception.Safe
import           Control.Monad
import           SafeBuffer
import           Test.Hspec
import           UnliftIO.Async
import           UnliftIO.Concurrent    (forkIO, killThread, newEmptyMVar,
                                         putMVar, takeMVar, threadDelay)

spec :: Spec
spec =
  describe "SafeBufferT" $ do
    it "writes to buffer" $
      (tryRunBuffer @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        writeBuffer [3]
        pure "done"
      ) `shouldReturn` ([1,2,3], Right "done")

    it "reads from buffer" $
      (tryRunBuffer @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        buffer <- readBuffer
        pure (sum buffer)
      ) `shouldReturn` ([1,2], Right 3)

    it "clears buffer" $
      (tryRunBuffer @IOException $ do
        writeBuffer [1]
        writeBuffer [2]
        buffer <- clearBuffer
        writeBuffer [0]
        pure (sum buffer)
      ) `shouldReturn` ([0], Right 3)

    describe "runBuffer" $ do
      it "handles sync exceptions" $ do
        bufferCopy <- newEmptyMVar
        let action = runBuffer (putMVar bufferCopy) $ do
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
          runBuffer (putMVar bufferCopy) $ do
            writeBuffer [1]
            threadDelay maxBound
            writeBuffer [2]
        killThread threadId
        xs <- takeMVar bufferCopy
        xs `shouldBe` [1]
    

    describe "tryRunBuffer" $ do
      it "returns exceptions" $
        (tryRunBuffer $ do
          writeBuffer [1]
          void . throwIO $ userError "oops"
          writeBuffer [2]
          ) `shouldReturn` ([1], Left (userError "oops"))

      it "does not recover from async exceptions" $ do
        started <- newEmptyMVar
        thread <- async $ do
          void $ tryRunBuffer @SomeException $ do
            writeBuffer [1]
            putMVar started True
            threadDelay maxBound
            writeBuffer [2]
          threadDelay maxBound
        void $ takeMVar started
        cancel thread
        True `shouldBe` True

    describe "execBuffer" $ do
      it "catches exceptions" $ do
        buffer <- execBuffer @ErrorCall $ do
          writeBuffer [1]
          void $ error "yo"
          writeBuffer [2]
        buffer `shouldBe` [1]

      it "does not recover from async exceptions" $ do
        started <- newEmptyMVar
        thread <- async $ do
          void $ execBuffer @SomeException $ do
            writeBuffer [1]
            putMVar started True
            threadDelay maxBound
            writeBuffer [2]
          threadDelay maxBound
        void $ takeMVar started
        cancel thread
        True `shouldBe` True
