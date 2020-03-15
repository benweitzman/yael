module Yael.Eff.State where

import Control.Lens
import qualified Control.Monad.State as S
import           Yael.Eff
import Data.IORef
import Control.Monad.IO.Class
import Data.STRef
import Control.Monad.ST

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


withState :: s -> (forall n . Monad n => State s n -> n a) -> (a, s)
withState initial f = runST $ do
  ref <- newSTRef initial
  let state = State
        { _get = readSTRef ref
        , _put = writeSTRef ref
        }
  result <- f state
  final <- readSTRef ref
  return (result, final)


shareState :: Monad m => Lens' s s' -> State s m -> State s' m
shareState l state = State
  { _get = (^. l) <$> _get state
  , _put = \s' -> do
      s <- _get state
      _put state $ s
        & l .~ s'
  }
