{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SafeBuffer
  ( SafeBufferMonad(..)
  , SafeBufferConcurrentT(..)
  , runSafeBufferConcurrently
  , tryRunSafeBufferConcurrently
  , SafeBufferSyncT(..)
  , runSafeBufferSync
  , tryRunSafeBufferSync
  ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Zip
import           UnliftIO.IORef
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
           , Alternative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadReader (TVar s)
           , MonadWriter r
           , MonadState s
           , MonadZip
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadError e
           , MonadFail
           , MonadPlus
           , MonadCont
           , MonadFix
           )

runSafeBufferConcurrently ::
     (MonadIO m, MonadMask m, Monoid s)
  => (s -> m b)
  -> SafeBufferConcurrentT s m a
  -> m a
runSafeBufferConcurrently finalize sb =
  bracket
    (newTVarIO mempty)
    (\tvar -> readTVarIO tvar >>= finalize)
    (\tvar -> runReaderT (runSafeBufferConcurrentT sb) tvar)

tryRunSafeBufferConcurrently ::
     (MonadIO m, MonadMask m, Monoid s, Exception e)
  => SafeBufferConcurrentT s m a
  -> m (s, Either e a)
tryRunSafeBufferConcurrently sb = do
  tvar <- newTVarIO mempty
  result <- try $ runReaderT (runSafeBufferConcurrentT sb) tvar
  buffer <- readTVarIO tvar
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

--------------------------------------------------------------------------------
-- SafeBufferSyncT
--------------------------------------------------------------------------------
newtype SafeBufferSyncT s m a =
  SafeBufferSyncT { runSafeBufferSyncT :: ReaderT (IORef s) m a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadReader (IORef s)
           , MonadWriter r
           , MonadState s
           , MonadZip
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadError e
           , MonadFail
           , MonadPlus
           , MonadCont
           , MonadFix
           )

runSafeBufferSync ::
     (MonadIO m, MonadMask m, Monoid s)
  => (s -> m b)
  -> SafeBufferSyncT s m a
  -> m a
runSafeBufferSync finalize sb =
  bracket
    (newIORef mempty)
    (\ref -> readIORef ref >>= finalize)
    (\ref -> runReaderT (runSafeBufferSyncT sb) ref)

tryRunSafeBufferSync ::
     (MonadIO m, MonadMask m, Monoid s, Exception e)
  => SafeBufferSyncT s m a
  -> m (s, Either e a)
tryRunSafeBufferSync sb = do
  ref <- newIORef mempty
  result <- try $ runReaderT (runSafeBufferSyncT sb) ref
  buffer <- readIORef ref
  pure (buffer, result)

instance (Monad m, MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferSyncT s m) where
  writeBuffer :: s -> SafeBufferSyncT s m ()
  writeBuffer msg =
    SafeBufferSyncT $ ReaderT $ \ref ->
      modifyIORef ref (`mappend` msg)

  readBuffer :: SafeBufferSyncT s m s
  readBuffer =
    SafeBufferSyncT $ ReaderT $ \ref ->
      readIORef ref

  clearBuffer :: SafeBufferSyncT s m s
  clearBuffer = 
    SafeBufferSyncT $ ReaderT $ \ref -> do
      buffer <- readIORef ref
      writeIORef ref mempty
      pure buffer
