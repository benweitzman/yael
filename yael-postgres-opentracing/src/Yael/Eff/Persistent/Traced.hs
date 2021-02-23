{-# LANGUAGE OverloadedStrings #-}

module Yael.Eff.Persistent.Traced where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader      as R
import           Control.Monad.Trans.Class
import           Data.IORef
import qualified Data.Text                 as T
import           Database.Persist.Sql
import           OpenTracing
import           Yael.Eff
import           Yael.Eff.OpenTracing
import           Yael.Eff.Persistent

tracePersistent :: (MonadIO m) => Tracing m -> Persistent m -> Persistent m
tracePersistent t p = p
  { _runPersistent = \tx -> flip runEffT (t :<> p) $ do
      opts <- defaultSpanOpts "runPersistent"
      spanning opts . runPersistent $ do
        ref <- liftIO $ newIORef []
        x <- R.mapReaderT lift . flip R.local tx $ \conn -> conn
          { connPrepare = \stmt -> do
              modifyIORef ref (stmt:)
              connPrepare conn stmt
          }
        ran <- liftIO $ readIORef ref
        lift . modifyActive $ \span' ->
          span' & spanTags %~ setTag ("Queries", StringT $ T.pack $ show ran)
        return x
  }
