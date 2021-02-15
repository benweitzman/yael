{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Yael.Eff.Builder2
  {-
  ( runEffTWith
  , wrapping
  -- * Helper datatypes and classes in case you want to write `EffStack` instances by hand (but hint: you shouldn't)
  , EffList(..)
  , EffStack(..)
  , EffectIdentity
  , EffectBuilder
  , EffectProxy
  , Effect
  , AEffList
  )
-}
where

import Yael.Eff
import GHC.Generics
import Data.Proxy
import Data.Function
import Control.Lens (lens, (^.), (.~), ix, Lens', iso, _1, _2) 
import qualified Data.Array as A 
import GHC.Exts (Any)
import GHC.TypeLits  
import Unsafe.Coerce
import Data.Barbie
import Yael.Eff.State
import Yael.Eff.Reader
import Data.Functor.Product
import GHC.Generics.Lens
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Reader as MTL

{-
data EffectIdentity m
data EffectBuilder s (m :: * -> *)
data EffectProxy

type family Effect f t where
  Effect (EffectIdentity m ) t = t m
--  Effect (EffectBuilder s m) t = EffList (ToEffList s) m -> t m
  Effect EffectProxy t = Proxy t
-}

newtype Effect m s = Effect { unEffect :: s m }

data X n t m where
  X :: t (Effect m) -> X m t m

newtype Builder t m s = Builder { unBuilder :: X m t m -> s m }

build :: forall t m . FunctorB t => t (Builder t m) -> X m t m
build b = X $ fix $ \preBuilt -> bmap (Effect . ($ X preBuilt) . unBuilder) b

type family InLeft x f m where
  InLeft (K1 r (Effect m f)) f m = 'True
  InLeft (M1 r d x) f m = InLeft x f m
  InLeft (l :*: r) f m = InLeft l f m
  InLeft _ _ _ = 'False

class GProject g f m where
  gPrj :: Lens' (g x) (f m)

instance GProject (K1 r (Effect m f)) f m where
  gPrj = _K1 . lens unEffect (const Effect)

instance GProject x f m => GProject (M1 r d x) f m where
  gPrj = _M1 . gPrj
  
class GProjectProduct signal l r f m where
  gPrjProd :: Proxy signal -> Lens' ((l :*: r) x) (f m)

instance GProject l f m => GProjectProduct 'True l r f m    where
  gPrjProd _ = _1 . gPrj

instance GProject r f m => GProjectProduct 'False l r f m    where
  gPrjProd _ = _2 . gPrj
  
instance GProjectProduct (InLeft l f m) l r f m => GProject (l :*: r) f m where
  gPrj = gPrjProd (Proxy @(InLeft l f m))

instance (Generic (t (Effect m)), GProject (Rep (t (Effect m))) g m) => Project (X m t) g where
  prj = lens
    (\(X t) -> from t ^. gPrj)
    (\(X t) g -> X . to . (gPrj .~ g) $ from t)

data SomeEffect m = SomeEffect
  { _op :: m String
  }

op :: String :+ '[SomeEffect]
op = withEffT _op  

data MyStack f = MyStack
  { stackState :: f (State Int)
  , stackReader :: f (Reader Bool)
  , stackReader2 :: f (Reader Bool)
  , stackOp :: f SomeEffect
  } deriving (Generic, FunctorB)

baseEffect :: s m -> Builder t m s
baseEffect = Builder . const

built :: (MTL.MonadState Int m, MTL.MonadReader Bool m) => X m MyStack m
built = build MyStack
  { stackState = baseEffect mtlState
  , stackReader = baseEffect mtlReader
  , stackReader2 = baseEffect mtlReader
  , stackOp = Builder $ \eff -> SomeEffect
    { _op = flip runEffT eff $ do
        a <- get @Int
        b <- ask @Bool
        return $ show a ++ ", " ++ show b
    }
  }
