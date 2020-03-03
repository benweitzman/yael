module Yael.Eff.State where

import qualified Control.Monad.State as S
import           Yael.Eff
import Data.IORef
import Control.Monad.IO.Class

data State s m = State
  { _get :: m s
  , _put :: s -> m ()
  }

get :: s :+ '[State s]
get = withEffT _get

put :: s -> () :+ '[State s]
put s = withEffT $ \State{_put} -> _put s


mtlState :: S.MonadState s m => State s m
mtlState = State
  { _get = S.get
  , _put = S.put
  }

ioRefState :: MonadIO m => IORef s -> State s m
ioRefState ref = State
  { _get = liftIO $ readIORef ref
  , _put = liftIO . writeIORef ref
  }
