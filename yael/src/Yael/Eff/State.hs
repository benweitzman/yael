module Yael.Eff.State where

import Control.Lens (Lens', (&), (.~), (^.))
import qualified Control.Monad.State as S
import           Yael.Eff
import Data.IORef
import Control.Monad.IO.Class
import Data.STRef
import Control.Monad.ST
import Yael.Eff.Builder
import GHC.Generics

data State s m = State
  { _get :: m s
  , _put :: s -> m ()
  }

get :: s :+ '[State s]
get = withEffT _get

put :: HasEffs '[State s] f m => s -> EffT f m ()
put = EffT . flip (_put . {-# SCC prjj #-} (^. {-# SCC resolvePrj #-} prj))
{-# SPECIALIZE put :: HasEffs '[State s] (AEffList f) m => s -> EffT (AEffList f) m () #-}
{-# SPECIALIZE put :: Monad m => s -> EffT (AEffList (State s ': fs)) m () #-}

modify :: (s -> s) -> () :+ '[State s]
modify f = do
  s <- get
  put $ f s

mtlState :: S.MonadState s m => State s m
mtlState = State
  { _get = S.get
  , _put = S.put
  }
{-# NOINLINE mtlState #-}  

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
