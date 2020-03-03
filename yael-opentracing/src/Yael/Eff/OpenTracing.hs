{-# LANGUAGE UndecidableInstances #-}

module Yael.Eff.OpenTracing
  ( Tracing(..)
  , spanning
  , modifyActive
  , mkSpanOpts
  , defaultSpanOpts
  , _defaultSpanOpts
  , defaultTrace
  , otTracing
  ) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Text as T
import GHC.Exts
import GHC.Generics
import GHC.TypeLits
import OpenTracing (ActiveSpan, Span, SpanOpts, spanOpts, childOf)
import qualified OpenTracing             as OT
import qualified OpenTracing.Standard    as OT
import OpenTracing.Tracer      (traced_)
import Yael.Eff
import Yael.Eff.Reader

data Tracing m = Tracing
  { _spanning :: forall a . SpanOpts -> m a -> m a
  , _modifyActive :: (Span -> Span) -> m ()
  , _mkSpanOpts :: T.Text -> (SpanOpts -> SpanOpts) -> m SpanOpts
  }

spanning :: (HasEff Tracing f m) => SpanOpts -> EffT f m a -> EffT f m a
spanning opts block = withEffT' $ \lower Tracing{_spanning} ->
  _spanning opts (lower block)

modifyActive :: (Span -> Span) -> () :+ '[Tracing]
modifyActive f = withEffT $ \Tracing{_modifyActive} -> _modifyActive f

mkSpanOpts :: T.Text -> (SpanOpts -> SpanOpts) -> SpanOpts :+ '[Tracing]
mkSpanOpts name f = withEffT $ \Tracing{_mkSpanOpts} ->
  _mkSpanOpts name f

otTracing
  :: (MonadIO m, MonadMask m)
  => Reader ParentSpan m
  -> OT.Tracer
  -> Tracing m
otTracing Reader{_ask,_local} tracer' = Tracing
  { _spanning = \opts block -> traced_ tracer' opts $ \activeSpan ->
      _local (const $ Just activeSpan) block

  , _modifyActive = \f -> do
      mActive <- _ask
      void . forM mActive $ \active ->
        liftIO $ OT.modifyActiveSpan active f

  , _mkSpanOpts = \name f -> do
      parent <- _ask
      return . f . spanOpts name $ maybe mempty childOf parent
  }

type ParentSpan = Maybe ActiveSpan

_defaultSpanOpts :: Tracing m -> T.Text -> m SpanOpts
_defaultSpanOpts Tracing{_mkSpanOpts} n = _mkSpanOpts n id

defaultSpanOpts :: T.Text -> SpanOpts :+ '[Tracing]
defaultSpanOpts name = mkSpanOpts name id

class Split m a where
  singleUse
    :: Monad m
    => Tracing m
    -> String
    -> a
    -> a

instance {-# OVERLAPPING #-} Split m (HigherOrder n m) where
  singleUse t@Tracing{_spanning} name (HigherOrder f) = HigherOrder $ \x -> do
    opts <- _defaultSpanOpts t (T.pack name)
    _spanning opts $ f x

instance {-# OVERLAPPING #-} (Split m b) => Split m (a -> b) where
  singleUse t n ab x = singleUse t n (ab x)

type family CheckFun m :: Constraint where
  CheckFun ((->) a) = TypeError (Text "explode")
  CheckFun x = ()

instance {-# INCOHERENT #-} (CheckFun m) => Split m (m x) where
  singleUse t@Tracing{_spanning} name x = do
    opts <- _defaultSpanOpts t (T.pack name)
    _spanning opts $ x

defaultTrace
  :: forall m f
   . ( GTraceable m (Rep (f m))
     , Generic (f m)
     , CheckFun m
     , Monad m
     )
  => Tracing m -> f m -> f m
defaultTrace f fm = to $ gTracing f (from fm)

class GTraceable m f where
  gTracing :: (Monad m) => Tracing m -> f x -> f x

instance (Split m v, KnownSymbol n) =>
  GTraceable m (S1 ('MetaSel ('Just n) u s l) (K1 r v)) where

  gTracing tracer (M1 (K1 x)) = M1 . K1 $
    singleUse tracer (symbolVal $ Proxy @n) x

instance GTraceable m f => GTraceable m (M1 k x f) where
  gTracing tracer (M1 x) = M1 $ gTracing tracer x

instance (GTraceable m f, GTraceable m g) => GTraceable m (f :*: g) where
  gTracing tracer (f :*: g) = gTracing tracer f :*: gTracing tracer g
