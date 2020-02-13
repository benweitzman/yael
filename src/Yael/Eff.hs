{-# LANGUAGE UndecidableInstances #-}

module Yael.Eff
  ( EffT
  , runEffT
  , askEffT
  , localEffT
  , MonadEff(..)
  , asksEff
  , (&)
  , (:+)
  , Project(..)
  , HasEffs
  ) where

import Control.Lens ((^.), Lens')
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import Control.Monad.Catch
import UnliftIO (MonadUnliftIO(..), withUnliftIO, UnliftIO(..))
import Data.Function ((&))

newtype EffT f m a = EffT
  { unEffT :: R.ReaderT (f (EffT f m)) m a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
                     , MonadMask, MonadPlus, Alternative)

runEffT :: EffT f m a -> f (EffT f m) -> m a
runEffT = R.runReaderT . unEffT

instance MonadUnliftIO m => MonadUnliftIO (EffT f m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = EffT . R.ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runEffT r))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    EffT . R.ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runEffT r)

instance MonadTrans (EffT f) where
  lift = EffT . lift

askEffT :: Monad m => EffT f m (f (EffT f m))
askEffT = EffT R.ask

localEffT :: (f (EffT f m) -> f (EffT f m)) -> EffT f m a -> EffT f m a
localEffT f m = EffT . R.ReaderT $ runEffT m . f

class Monad m => MonadEff m where
  type F m :: (* -> *) -> *

  askEff :: m (F m m)

  localEff :: (F m m -> F m m) -> m a -> m a

instance (Monad m) => MonadEff (EffT f m) where
  type F (EffT f m) = f

  askEff = askEffT

  localEff = localEffT

class Project f g where
  prj :: Lens' f g

instance Project x x where
  prj = id

type Has a m = (Project (F m m) a, MonadEff m)

type HasEff f m = Has (f m) m

asksEff :: (Has a m) => m a
asksEff = (^. prj) <$> askEff

type family HasEffs xs m where
  HasEffs '[x] m = HasEff x m
  HasEffs (x ': xs) m = (HasEff x m, HasEffs xs m)


type (:+) v effs = forall m . (HasEffs effs m) => m v
