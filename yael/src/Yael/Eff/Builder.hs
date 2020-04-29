{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Yael.Eff.Builder
  ( runEffTWith
  , wrapping
  -- * Helper datatypes and classes in case you want to write `EffStack` instances by hand (but hint: you shouldn't)
  , EffList(..)
  , EffStack(..)
  , EffectIdentity
  , EffectBuilder
  , EffectProxy
  , Effect
  ) where

import Yael.Eff
import GHC.Generics
import Data.Proxy
import Data.Function
import Control.Lens (lens, (^.), (.~))

data EffList fs m where
  EffNil :: EffList '[] m
  EffCons :: f m -> EffList fs m -> EffList (f ': fs) m

instance {-# OVERLAPPING #-} Project (EffList (f ': fs)) f where
  prj = lens extract update

    where
      extract :: EffList (f : fs) m -> f m
      extract (EffCons f _) = f

      update :: EffList (f : fs) m -> f m -> EffList (f : fs) m
      update (EffCons _ fs) f = EffCons f fs

instance Project (EffList fs) f => Project (EffList (g ': fs)) f where
  prj = lens extract update

    where
      extract :: EffList (g ': fs) m -> f m
      extract (EffCons _ fs) = fs ^. prj

      update :: EffList (g : fs) m -> f m -> EffList (g : fs) m
      update (EffCons g fs) f = EffCons g (fs & prj .~ f)

data EffectIdentity m
data EffectBuilder s (m :: * -> *)
data EffectProxy

type family Effect f t where
  Effect (EffectIdentity m ) t = t m
  Effect (EffectBuilder s m) t = EffList (ToEffList s) m -> t m
  Effect EffectProxy t = Proxy t

type family GToEffList s where
  GToEffList (K1 r (Proxy (t :: (* -> *) -> *))) = '[t]
  GToEffList (M1 s d x) = GToEffList x
  GToEffList (l :*: r) = GToEffList l :++ GToEffList r

type Builder f m = f (EffectBuilder f m)

type family l :++ r where
  '[] :++ r = r
  (l ': ls) :++ r = l ': (ls :++ r)

class GTo f p m where
  gToEffList :: Proxy p -> f x -> EffList (GToEffList p) m

instance GTo (K1 r (f m)) (K1 r (Proxy f)) m where
  gToEffList _ (K1 v) = EffCons v EffNil

instance GTo x y m => GTo (M1 s d x) (M1 s d y) m where
  gToEffList _ (M1 x) = gToEffList (Proxy @y) x

instance (GTo l l' m, GTo r r' m) => GTo (l :*: r) (l' :*: r') m where
  gToEffList _ (l :*: r) = appendEffList (gToEffList (Proxy @l') l) (gToEffList (Proxy @r') r)

appendEffList :: EffList xs m -> EffList ys m -> EffList (xs :++ ys) m
appendEffList EffNil ys = ys
appendEffList (EffCons x xs) ys = EffCons x (appendEffList xs ys)

class GApply f g fs m where
  gApply :: f x -> EffList fs m -> g x

instance GApply (K1 r (EffList fs m -> f m)) (K1 r (f m)) fs m where
  gApply (K1 f) preBuilt = K1 $ f preBuilt

instance GApply x y m fs => GApply (M1 s d x) (M1 s d y) m fs where
  gApply (M1 f) preBuilt = M1 $ gApply f preBuilt

instance (GApply l l' m fs, GApply r r' m fs) => GApply (l :*: r) (l' :*: r') m fs where
  gApply (l :*: r) preBuilt = gApply l preBuilt :*: gApply r preBuilt

apply
  :: ( GApply (Rep (Builder f m)) (Rep (f (EffectIdentity m))) (ToEffList f) m
     , Generic (Builder f m)
     , Generic (f (EffectIdentity m))
     )
  => Builder f m -> EffList (ToEffList f) m -> f (EffectIdentity m)
apply x preBuilt = to $ gApply (from x) preBuilt


class EffStack f where
  type ToEffList f :: [(* -> *) -> *]
  type ToEffList f = GToEffList (Rep (f EffectProxy))

  build :: Builder f m -> EffList (ToEffList f) m

  default build
    :: ( GTo (Rep (f (EffectIdentity m))) (Rep (f EffectProxy)) m
       , GApply (Rep (Builder f m)) (Rep (f (EffectIdentity m))) (GToEffList (Rep (f EffectProxy))) m
       , Generic (f (EffectIdentity m))
       , Generic (Builder f m)
       , ToEffList f ~ GToEffList (Rep (f EffectProxy))
       )
    => Builder f m -> EffList (ToEffList f) m
  build b = fix $ \preBuilt ->
    toEffList (apply b preBuilt)

  toEffList :: f (EffectIdentity m) -> EffList (ToEffList f) m

  default toEffList
    :: ( Generic (f (EffectIdentity m))
       , GTo (Rep (f (EffectIdentity m))) (Rep (f EffectProxy)) m
       , ToEffList f ~ GToEffList (Rep (f EffectProxy))
       )
    => f (EffectIdentity m)
    -> EffList (ToEffList f) m
  toEffList x = gToEffList (Proxy @(Rep (f EffectProxy))) $ from x


-- | Run an effectful computation using a set of effect handlers that are
-- implemented in terms of each other.
runEffTWith :: EffStack f => Builder f m -> EffT (EffList (ToEffList f)) m a -> m a
runEffTWith b e = runEffT e $ build b

infixr `wrapping`

wrapping :: (f m -> g m -> g m) -> (f m -> g m) -> f m -> g m
wrapping = (<*>)
