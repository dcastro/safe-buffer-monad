{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SafeBuffer where

import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.IORef

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

runConcurrent :: (MonadIO m, MonadMask m, Monoid s)
              => (s -> m b)
              -> SafeBufferConcurrentT s m a
              -> m a
runConcurrent finalize sb =
  bracket
    (liftIO $ newTVarIO mempty)
    (\tv -> liftIO (readTVarIO tv) >>= finalize)
    (\tv -> runReaderT (run sb) tv)

tryRunConcurrently :: (MonadIO m, MonadMask m, Monoid s, Exception e)
                   => SafeBufferConcurrentT s m a
                   -> m (s, Either e a)
tryRunConcurrently sb = do
  tv <- liftIO $ newTVarIO mempty
  result <- try $ runReaderT (run sb) tv
  buffer <- liftIO $ readTVarIO tv
  pure (buffer, result)
  
instance (Monad m, MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferConcurrentT s m) where
  writeBuffer :: s -> SafeBufferConcurrentT s m ()
  writeBuffer msg =
    SafeBufferConcurrentT $ ReaderT $ \tvar -> do
      liftIO $ atomically $ modifyTVar tvar (`mappend` msg)
