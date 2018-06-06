# safe-buffer-monad

A monadic buffer resilient to exceptions.

The `SafeBufferMonad` typeclass models a buffer that you can write things to. If an exception is thrown,
you'll still be able to proccess the contents of the buffer up to the point where the computation was interrupted.

```haskell
class Monad m => SafeBufferMonad s m | m -> s where
  readBuffer   :: m s
  writeBuffer  :: s -> m ()
  clearBuffer  :: m s
  modifyBuffer :: (s -> s) -> m ()
```

The buffer can be run using one of these 6 functions:

* `runBuffer` / `runBufferConcurrently`
* `tryRunBuffer` / `tryRunBufferConcurrently`
* `execBuffer` / `execBufferConcurrently`

```haskell
{-# LANGUAGE FlexibleContexts #-}

import SafeBuffer
import Data.List (intercalate)

go :: (SafeBufferMonad [String] m, MonadIO m) => m String
go = do
  writeBuffer ["line 1"]
  writeBuffer ["line 2"]
  liftIO $ putStrLn "brace for impact!"
  liftIO $ throwIO $ userError "boom"
  writeBuffer ["line 3"]
  pure "done!"

main = runBuffer (appendFile "log.txt" . intercalate "\n") go
```

```plain
λ> main
brace for impact!
*** Exception: user error (boom)

λ> :! tail log.txt
line 1
line 2
```
