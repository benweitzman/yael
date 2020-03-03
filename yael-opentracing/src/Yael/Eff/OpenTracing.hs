{-# LANGUAGE UndecidableInstances #-}

module Yael.Eff.OpenTracing
  ( HigherOrder(..)
  , Tracer(..)
  , defaultTracer
  , Traceable(..)
  , Tracing(..)
  , traceWrapL
  , mkSpanOpts
  , defaultSpanOpts
  ) where

import Data.Monoid (Last(..))
import Data.Maybe
import Control.Lens (setting, Setter')
import Data.Generic.HKD
import GHC.Generics
import GHC.Exts
import Data.Functor.Const
import           OpenTracing             hiding (HasTracer, Tracer, traced_, tracer)
import qualified Data.Text as T
import Data.Functor.Product
import Data.Barbie.Constraints
import Control.Monad.Reader
import GHC.TypeLits
import Data.Proxy
import Yael.Eff
import Yael.Eff.Data

defaultTracer
  :: forall f m
   . ( ConstraintsB (HKD (f m))
     , AllB (Split m) (HKD (f m))
     , Label (f m)
     , Generic (f m)
     )
  => HKD (f m) (Tracer m)
defaultTracer = bmap mkTracer (baddDicts label)
  where
    mkTracer :: forall a . Product (Dict (Split m)) (Const String) a -> Tracer m a
    mkTracer (Pair Dict (Const name)) = Tracer
      { traceWrap = \tracing method -> singleUse tracing name method
      }

type ParentSpan = Maybe ActiveSpan

newtype Tracer m a = Tracer
  { traceWrap :: MonadReader ParentSpan m => Tracing m -> a -> a
  }

traceWrapL :: Setter' (Tracer m a) (Tracing m -> a -> a)
traceWrapL = setting $ \m x -> x {traceWrap = m (traceWrap x)}

mkSpanOpts
  :: MonadReader ParentSpan m
  => T.Text
  -> (SpanOpts -> SpanOpts)
  -> m SpanOpts
mkSpanOpts t f = do
  parent <- ask
  return . f . spanOpts t $ maybe mempty childOf parent

defaultSpanOpts t = mkSpanOpts t id

class Split m a where
  singleUse
    :: (MonadReader ParentSpan m)
    => Tracing m
    -> String
    -> a
    -> a

instance {-# OVERLAPPING #-} Split m (HigherOrder n m) where
  singleUse Tracing{spanning} name (HigherOrder f) = HigherOrder $ \x -> do
    opts <- spanOpts (T.pack name) . maybe mempty childOf <$> ask
    spanning opts $ \activeSpan ->
      local (const $ Just activeSpan) $ f x

instance {-# OVERLAPPING #-} (Split m b) => Split m (a -> b) where
  singleUse t n ab x = singleUse t n (ab x)

type family CheckFun m :: Constraint where
  CheckFun ((->) a) = TypeError (Text "explode")
  CheckFun x = ()

instance {-# INCOHERENT #-} (CheckFun m) => Split m (m x) where
  singleUse Tracing{spanning} name x = do
    opts <- spanOpts (T.pack name) . maybe mempty childOf <$> ask
    spanning opts $ \activeSpan ->
      local (const $ Just activeSpan) x

data Tracing m = Tracing
  { spanning :: forall a . SpanOpts -> (ActiveSpan -> m a) -> m a
  , modifyActive :: (Span -> Span) -> m ()
  , parentSpan :: ParentSpan
  }

class Traceable f where
  tracing :: (TracerC f m, MonadReader ParentSpan m, CheckFun m) => Tracing m -> f m -> f m

  default tracing
    :: forall m
     . ( GTraceable m (Rep (HKD (f m) (Tracer m))) (Rep (f m))
       , Generic (f m)
       , Generic (HKD (f m) (Tracer m))
       , TracerC f m
       , MonadReader ParentSpan m
       , CheckFun m
       )
    => Tracing m -> f m -> f m
  tracing f fm = to $ gTracing f (from $ tracer @f f) (from fm)

  type TracerC f :: (* -> *) -> Constraint
  type TracerC f = Monad

  tracer :: (TracerC f m, MonadReader ParentSpan m, CheckFun m) => Tracing m -> HKD (f m) (Tracer m)

  default tracer
    :: ( ConstraintsB (HKD (f m))
       , AllB (Split m) (HKD (f m))
       , Label (f m)
       , Generic (f m)
       , CheckFun m
       )
    => Tracing m -> HKD (f m) (Tracer m)
  tracer _tr = defaultTracer

class GTraceable m f g | f -> m where
  gTracing :: MonadReader ParentSpan m => Tracing m -> f x -> g x -> g x

instance (Monad m) => GTraceable m (K1 r (Tracer m v)) (K1 r v) where
  gTracing tracer (K1 (Tracer f)) (K1 x) = K1 $ do
    f tracer x

instance GTraceable m t f => GTraceable m (M1 k x t) (M1 k x f) where
  gTracing tracer  (M1 x) (M1 y) = M1 $ gTracing tracer x y

instance (GTraceable m tl fl, GTraceable m tr fr) => GTraceable m (tl :*: tr) (fl :*: fr) where
  gTracing tracer  (tl :*: tr) (fl :*: fr) = gTracing tracer tl fl :*: gTracing tracer tr fr
