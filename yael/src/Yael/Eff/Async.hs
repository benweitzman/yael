module Yael.Eff.Async where

import Yael.Eff
import Control.Monad.IO.Class
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control
import Control.Concurrent (myThreadId, ThreadId)

newtype Async m = Async
  { _async :: m () -> m ThreadId
  }

async :: (HasEff Async f m) => EffT f m () -> EffT f m ThreadId
async m = withEffT' $ \lower Async{_async} -> _async (lower m)

concurrentAsync :: (MonadBaseControl IO m) => Async m
concurrentAsync = Async fork

noAsync :: (MonadIO m, Monad n) => m (Async n)
noAsync = do
  tid <- liftIO myThreadId
  return Async
    { _async = \m -> m >> return tid
    }
