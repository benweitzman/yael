{-# LANGUAGE OverloadedStrings #-}

module Yael.Eff.HTTP where

import Yael.Eff
import Data.ByteString (ByteString, null)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Request, Response, Manager, method, path)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.Internal (Response(..), CookieJar(CJ), ResponseClose(..))
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status (Status, status404, statusCode)
import Network.HTTP.Types.Version (http11)
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M

data ResponseBody m
  = ResponseComplete ByteString
  | ResponseChunked (m ByteString)

hoistResponseBody :: (m ~> n) -> ResponseBody m -> ResponseBody n
hoistResponseBody _ (ResponseComplete c) = ResponseComplete c
hoistResponseBody f (ResponseChunked getChunk) = ResponseChunked $ f getChunk

data HTTP m = HTTP
 { _withResponse :: forall a . Request -> (Response (ResponseBody m) -> m a) -> m a
 }

managerHTTP :: (MonadBaseControl IO m) => Manager -> HTTP m
managerHTTP mgr = HTTP
  { _withResponse = \req h -> control $ \run ->
      Client.withResponse req mgr $
        run . h . fmap (ResponseChunked . liftBase)
  }

mockHTTP :: (Request -> Response ByteString) -> HTTP m
mockHTTP f = HTTP
  { _withResponse = \req h ->
      let response = f req
      in h $ ResponseComplete <$> response
  }

staticHTTP :: Map (Method, BS.ByteString) (Status, ByteString) -> HTTP m
staticHTTP endpoints = mockHTTP findEndpoint
  where
    findEndpoint :: Request -> Response ByteString
    findEndpoint req = case M.lookup (method req, path req) endpoints of
      Nothing -> Response status404 http11 [] "" (CJ []) (ResponseClose $ return ())
      Just (s, b) -> Response s http11 [] b (CJ []) (ResponseClose $ return ())


request :: (HasEff HTTP f m) => Request -> EffT f m (Response ByteString)
request req = withResponse req $ \res -> do
    bs <- brReadAll $ Client.responseBody res
    return res { Client.responseBody = bs }

withResponse
  :: (HasEff HTTP f m)
  => Request
  -> (Response (ResponseBody (EffT f m)) -> EffT f m a)
  -> EffT f m a
withResponse req f = withEffT'' $ \lower raise HTTP{_withResponse} ->
  _withResponse req $ lower . f . fmap (hoistResponseBody raise)

brReadAll :: Monad m => ResponseBody m -> m ByteString
brReadAll (ResponseComplete c) = return c
brReadAll (ResponseChunked getChunk) = do
  bss <- brConsume getChunk
  return $ BS.concat bss

brConsume :: Monad m => m ByteString -> m [ByteString]
brConsume brRead' =
    go id
  where
    go front = do
      x <- brRead'
      if BS.null x
        then return $ front []
        else go (front . (x:))
