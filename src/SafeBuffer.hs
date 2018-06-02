{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module SafeBuffer
  ( SafeBufferMonad(..)
  , SafeBufferConcurrentT(..)
  , execBufferConcurrently
  , runBufferConcurrently
  , tryRunBufferConcurrently
  , SafeBufferT(..)
  , execBuffer
  , runBuffer
  , tryRunBuffer
  ) where

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Zip
import           UnliftIO.IORef
import           UnliftIO.STM

class Monad m => SafeBufferMonad s m | m -> s where
  readBuffer :: m s
  writeBuffer :: s -> m ()
  clearBuffer :: m s
  modifyBuffer :: (s -> s) -> m ()


--------------------------------------------------------------------------------
-- SafeBufferT
--------------------------------------------------------------------------------
newtype SafeBufferT s m a =
  SafeBufferT { runBufferT :: ReaderT (IORef s) m a }
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

instance (MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferT s m) where
  readBuffer :: SafeBufferT s m s
  readBuffer =
    SafeBufferT $ ReaderT $ \ref ->
      readIORef ref

  writeBuffer :: s -> SafeBufferT s m ()
  writeBuffer msg = modifyBuffer (`mappend` msg)

  clearBuffer :: SafeBufferT s m s
  clearBuffer = 
    SafeBufferT $ ReaderT $ \ref -> do
      buffer <- readIORef ref
      writeIORef ref mempty
      pure buffer

  modifyBuffer :: (s -> s) -> SafeBufferT s m ()
  modifyBuffer f = 
    SafeBufferT $ ReaderT $ \ref ->
      modifyIORef' ref f

-- | Recover from and swallow exceptions of type `e`
execBuffer ::
     forall e s m a
   . (MonadIO m, MonadCatch m, Monoid s, Exception e)
  => SafeBufferT s m a
  -> m s
execBuffer sb = do
  ref <- newIORef mempty
  catch @m @e
    (runReaderT (runBufferT sb) ref >> readIORef ref)
    (\_exception -> readIORef ref)

-- | Runs a buffer and applies a given function to it.
-- | If an exception occurs while running the buffer,
-- | the function still runs before the exception is rethrown.
runBuffer ::
     forall s m a b
   . (MonadIO m, MonadMask m, Monoid s)
  => (s -> m b)
  -> SafeBufferT s m a
  -> m a
runBuffer finalize sb =
  bracket
    (newIORef mempty)
    (\ref -> readIORef ref >>= finalize)
    (\ref -> runReaderT (runBufferT sb) ref)

-- | Runs a buffer and returns it, along with either an exception or the computation's result.
tryRunBuffer ::
     forall e s m a
   . (MonadIO m, MonadCatch m, Monoid s, Exception e)
  => SafeBufferT s m a
  -> m (s, Either e a)
tryRunBuffer sb = do
  ref <- newIORef mempty
  result <- try $ runReaderT (runBufferT sb) ref
  buffer <- readIORef ref
  pure (buffer, result)

--------------------------------------------------------------------------------
-- SafeBufferConcurrentT
--------------------------------------------------------------------------------
newtype SafeBufferConcurrentT s m a =
  SafeBufferConcurrentT { runBufferConcurrentT :: ReaderT (TVar s) m a }
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

instance (MonadIO m, Monoid s) => SafeBufferMonad s (SafeBufferConcurrentT s m) where
  readBuffer :: SafeBufferConcurrentT s m s
  readBuffer =
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      readTVarIO tvar

  writeBuffer :: s -> SafeBufferConcurrentT s m ()
  writeBuffer msg = modifyBuffer (`mappend` msg)

  clearBuffer :: SafeBufferConcurrentT s m s
  clearBuffer = 
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      atomically $ swapTVar tvar mempty

  modifyBuffer :: (s -> s) -> SafeBufferConcurrentT s m ()
  modifyBuffer f =
    SafeBufferConcurrentT $ ReaderT $ \tvar ->
      atomically $ modifyTVar' tvar f

-- | Recover from and swallow exceptions of type `e`
execBufferConcurrently ::
     forall e s m a
   . (MonadIO m, MonadCatch m, Monoid s, Exception e)
  => SafeBufferConcurrentT s m a
  -> m s
execBufferConcurrently sb = do
  tvar <- newTVarIO mempty
  catch @m @e
    (runReaderT (runBufferConcurrentT sb) tvar >> readTVarIO tvar)
    (\_exception -> readTVarIO tvar)

-- | Runs a buffer and applies a given function to it.
-- | If an exception occurs while running the buffer,
-- | the function still runs before the exception is rethrown.
runBufferConcurrently ::
     forall s m a b
   . (MonadIO m, MonadMask m, Monoid s)
  => (s -> m b)
  -> SafeBufferConcurrentT s m a
  -> m a
runBufferConcurrently finalize sb =
  bracket
    (newTVarIO mempty)
    (\tvar -> readTVarIO tvar >>= finalize)
    (\tvar -> runReaderT (runBufferConcurrentT sb) tvar)

-- | Runs a buffer and returns it, along with either an exception or the computation's result.
tryRunBufferConcurrently ::
     forall e s m a
   . (MonadIO m, MonadCatch m, Monoid s, Exception e)
  => SafeBufferConcurrentT s m a
  -> m (s, Either e a)
tryRunBufferConcurrently sb = do
  tvar <- newTVarIO mempty
  result <- try $ runReaderT (runBufferConcurrentT sb) tvar
  buffer <- readTVarIO tvar
  pure (buffer, result)
