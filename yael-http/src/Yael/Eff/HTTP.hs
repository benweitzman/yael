module Yael.Eff.HTTP where

import Yael.Eff
import Data.ByteString (ByteString, null)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Request, Response, Manager)
import qualified Network.HTTP.Client as Client
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.IO.Class

data HTTP m = HTTP
 { _withResponse :: forall a . Request -> (Response (m ByteString) -> m a) -> m a
 }

managerHTTP :: (MonadBaseControl IO m) => Manager -> HTTP m
managerHTTP mgr = HTTP
  { _withResponse = \req h -> control $ \run ->
      Client.withResponse req mgr $
        run . h . fmap liftBase
  }

request :: (HasEff HTTP f m) => Request -> EffT f m (Response ByteString)
request req = withResponse req $ \res -> do
    bss <- brConsume $ Client.responseBody res
    return res { Client.responseBody = BS.concat bss }

withResponse
  :: (HasEff HTTP f m)
  => Request
  -> (Response (EffT f m ByteString) -> EffT f m a)
  -> EffT f m a
withResponse req f = withEffT'' $ \lower raise HTTP{_withResponse} ->
  _withResponse req $ lower . f . fmap raise

brConsume :: Monad m => m ByteString -> m [ByteString]
brConsume brRead' =
    go id
  where
    go front = do
      x <- brRead'
      if BS.null x
        then return $ front []
        else go (front . (x:))
