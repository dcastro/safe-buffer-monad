{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SafeBufferMonadSpec where

import           Control.Monad.State
import           Data.Functor.Identity
import           SafeBuffer
import           Test.Hspec

spec :: Spec
spec =
  describe "SafeBufferMonad" $
    it "is mockable" $ do
      runIdentity (runBufferMock someFunction) `shouldBe` (6, [1,2,3])
        where
          someFunction :: SafeBufferMonad [Int] m => m Int
          someFunction = do
            writeBuffer [1]
            writeBuffer [2]
            writeBuffer [3]
            buffer <- readBuffer
            pure (sum buffer)

-- an instance of SafeBufferMonad that doesn't rely on IO
-- (but doesn't handle exceptions either) for testing
newtype SafeBufferMockT s m a = SafeBufferMockT { run :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m, Monoid s) => SafeBufferMonad s (SafeBufferMockT s m) where
  readBuffer = SafeBufferMockT get

  writeBuffer msg = modifyBuffer (`mappend` msg)
  
  clearBuffer = SafeBufferMockT $ do
    s <- get
    put mempty
    pure s

  modifyBuffer f = SafeBufferMockT $ modify' f

runBufferMock :: Monoid s => SafeBufferMockT s m a -> m (a, s)
runBufferMock sb = runStateT (run sb) mempty
