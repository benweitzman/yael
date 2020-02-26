{-# LANGUAGE UndecidableInstances #-}

module Yael.Eff.Mock where

import Data.Functor.Identity
import Data.Generic.HKD
import GHC.Generics
import GHC.Exts
import Data.Barbie
import Data.Maybe
import Data.Function

type family Args f where
  Args (a -> b) = (a, Args b)
  Args c  = ()

type family Return f where
  Return (a -> b) = Return b
  Return (m b) = b
  Return b = b

data MockAction x where
  (:=>) :: Args x -> Return x -> MockAction x

instance (Show (Return x)) => Show (MockAction x) where
  show (args :=> res) = "... :=> " ++ show res

newtype Mock x = Mock { getMockActions :: [MockAction x] }
  deriving newtype (Semigroup, Monoid, IsList)

deriving newtype instance Show (Return x) => Show (Mock x)

type Mocked f m = HKD (f m) Mock

class Mockable x where
  mock :: Mock x -> x

instance {-# OVERLAPPABLE #-} (x ~ Return x) => Mockable x where
  mock (Mock (_ :=> y : _)) = y

instance {-# OVERLAPPABLE #-} (Monad m, x ~ Return (m x)) => Mockable (m x) where
  mock (Mock (_ :=> y : _)) = return y

instance (Eq x, Mockable y) => Mockable (x -> y) where
  mock (Mock mocks) = \x ->
    let filtered = flip mapMaybe mocks $ \((x', xs) :=> r) ->
          case x == x' of
            True -> Just $ xs :=> r
            False -> Nothing
    in mock $ Mock filtered

mocking
  :: forall f m
   . ( Generic (f m)
     , Construct Identity (f m)
     , ConstraintsB (HKD (f m))
     , AllB Mockable (HKD (f m))
     )
  => Mocked f m
  -> f m
mocking m = runIdentity . construct $ bmapC @Mockable (Identity . mock) m
