{-# LANGUAGE OverloadedStrings #-}

module Yael.Eff.HTTP.Traced where

import           Control.Lens
import           Control.Monad.Trans.Class
import qualified Data.Text                 as T
import           Network.HTTP.Client       (getUri, method, responseStatus)
import           OpenTracing               hiding (HTTP)
import           Yael.Eff
import           Yael.Eff.HTTP
import           Yael.Eff.OpenTracing

traceHTTP :: Monad m => Tracing m -> HTTP m -> HTTP m
traceHTTP t h = HTTP
  { _withResponse = \req r -> shareEffT (t :<> h) $ \lower -> do
      opts <- mkSpanOpts "request" $ \opts -> opts
        & spanOptTags .~ [HttpMethod $ method req
                         ,HttpUrl . T.pack . show $ getUri req
                         ]
      spanning opts $ withResponse req $ \resp -> do
        modifyActive $ \span' -> span'
          & spanTags %~ setTag (HttpStatusCode $ responseStatus resp)
        lift . r . fmap (hoistResponseBody lower) $ resp
  }
