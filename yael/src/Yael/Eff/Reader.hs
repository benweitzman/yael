module Yael.Eff.Reader where

import Yael.Eff
import qualified Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Monad.Trans.Control

data Reader r m = Reader
  { _ask :: m r
  , _local :: forall a . (r -> r) -> m a -> m a
  }

ask :: r :+ '[Reader r]
ask = withEffT _ask

local :: (HasEff (Reader r) f m) => (r -> r) -> EffT f m a -> EffT f m a
local f m = withEffT' $ \lower Reader{_local} -> _local f (lower m)

mtlReader :: R.MonadReader r m => Reader r m
mtlReader = Reader
  { _ask = R.ask
  , _local = R.local
  }

underReader :: Monad m => Reader r (R.ReaderT q (R.ReaderT r m))
underReader = Reader
  { _ask = lift R.ask
  , _local = \f qr -> liftWith $ \run -> R.local f (run qr)
  }
