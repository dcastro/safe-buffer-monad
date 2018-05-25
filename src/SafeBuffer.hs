{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SafeBuffer
  ( SafeBufferMonad(..)
  , SafeBufferConcurrentT
  , runConcurrently
  , tryRunConcurrently
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer

class Monad m => SafeBufferMonad s m where
  writeBuffer :: s -> m ()

newtype SafeBufferConcurrentT s m a = SafeBufferConcurrentT { run :: ReaderT (TVar s) m a }
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
    (newTVarIO' mempty)
    (\tv -> readTVarIO' tv >>= finalize)
    (\tv -> runReaderT (run sb) tv)

tryRunConcurrently :: (MonadIO m, MonadMask m, Monoid s, Exception e)
                   => SafeBufferConcurrentT s m a
                   -> m (s, Either e a)
tryRunConcurrently sb = do
  tv <- newTVarIO' mempty
  result <- try $ runReaderT (run sb) tv
  buffer <- readTVarIO' tv
  pure (buffer, result)
  
instance (Monad m, MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferConcurrentT s m) where
  writeBuffer :: s -> SafeBufferConcurrentT s m ()
  writeBuffer msg =
    SafeBufferConcurrentT $ ReaderT $ \tvar -> do
      modifyTVarIO' tvar (`mappend` msg)

--------------------------------------------------------------------------------
-- Lifted STM functions
--------------------------------------------------------------------------------
newTVarIO' :: MonadIO m => a -> m (TVar a)
newTVarIO' = liftIO . newTVarIO

readTVarIO' :: MonadIO m => TVar a -> m a
readTVarIO' = liftIO . readTVarIO

modifyTVarIO' :: MonadIO m => TVar a -> (a -> a) -> m ()
modifyTVarIO' tv = liftIO . atomically . modifyTVar tv


