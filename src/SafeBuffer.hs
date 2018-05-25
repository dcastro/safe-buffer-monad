{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SafeBuffer
  ( SafeBufferMonad(..)
  , SafeBufferConcurrentT(..)
  , runConcurrently
  , tryRunConcurrently
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer
import           UnliftIO.STM

class Monad m => SafeBufferMonad s m where
  writeBuffer :: s -> m ()
  readBuffer :: m s
  clearBuffer :: m s

--------------------------------------------------------------------------------
-- SafeBufferConcurrentT
--------------------------------------------------------------------------------
newtype SafeBufferConcurrentT s m a =
  SafeBufferConcurrentT { runSafeBufferConcurrentT :: ReaderT (TVar s) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadReader (TVar s)
           , MonadWriter r
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runConcurrently :: (MonadIO m, MonadMask m, Monoid s)
                => (s -> m b)
                -> SafeBufferConcurrentT s m a
                -> m a
runConcurrently finalize sb =
  bracket
    (newTVarIO mempty)
    (\tv -> readTVarIO tv >>= finalize)
    (\tv -> runReaderT (runSafeBufferConcurrentT sb) tv)

tryRunConcurrently :: (MonadIO m, MonadMask m, Monoid s, Exception e)
                   => SafeBufferConcurrentT s m a
                   -> m (s, Either e a)
tryRunConcurrently sb = do
  tv <- newTVarIO mempty
  result <- try $ runReaderT (runSafeBufferConcurrentT sb) tv
  buffer <- readTVarIO tv
  pure (buffer, result)
  
instance (Monad m, MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferConcurrentT s m) where
  writeBuffer :: s -> SafeBufferConcurrentT s m ()
  writeBuffer msg =
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      atomically $ modifyTVar tvar (`mappend` msg)

  readBuffer :: SafeBufferConcurrentT s m s
  readBuffer =
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      readTVarIO tvar

  clearBuffer :: SafeBufferConcurrentT s m s
  clearBuffer = 
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      atomically $ swapTVar tvar mempty

